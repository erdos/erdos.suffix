(ns erdos.h)

; https://github.com/kvh/Python-Suffix-Tree

(defn ->node [] {:suffix_node -1})

(defn ->edge [[fci lci] [sni dni]]
  (assert (and fci lci sni dni))
  {:fci fci
   :lci lci
   :sni sni
   :dni dni})

(defn edge-length [n]
  (assert (map? n))
  (- (:lci n) (:fci n)))

(defn ->suffix [sni, fci, lci]
  (assert (and sni fci lci))
  {:sni sni, :fci fci, :lci lci})

(def suffix-length edge-length)

(defn explicit? [suffix]
  (assert (map? suffix))
  (> (:fci suffix) (:lci suffix)))

(def implicit (complement explicit?))

(defn insert-edge [self, edge] ; ok
  (assert (map? self))
  (assoc-in self [:edges [(:sni edge) (nth (:str self) (:fci edge))]] edge))

(defn remove-edge [self {:keys [sni fci]}]
  (assert (map? self))
  (assoc-in self [:edges [sni (nth (:str self) fci)]] nil))

;; ln 159
(defn split-edge [self, edge, suffix]
  (assert (map? self) "split-edge requires self map")
  (let [self (update-in self [:nodes] conj (->node))
        e (->edge [(:fci edge)    (+ (:fci edge) (suffix-length suffix) )]
                  [(:sni suffix)  (dec (count (:nodes self)))])
        self (remove-edge self edge)
        self (insert-edge self e)
        self (assoc-in self [:nodes (:dni e) :suffix_node] (:sni suffix))
        edge (update-in edge [:fci] + (inc (suffix-length suffix)))
        edge (assoc-in edge [:sni] (:dni e))
        self (insert-edge self edge)]
    [self, (:dni e)]))


;; ln 170, jonak tunik.
(defn canonize-suffix- [self, suffix]
  (if-not (explicit? suffix)
    (let [e (get-in self [:edges [(:sni suffix) (nth (:str self) (:fci suffix))]])]
      (if (<= (edge-length e) (suffix-length suffix))
        (recur self
               (assoc suffix
                 :fci (+ (:fci suffix) 1 (edge-length e))
                 :sni (:dni e)))
        suffix))
    suffix))

;; kenyelmi funkcio, ugyis csak innen hivjuk.
(defn canonize-suffix [self]
  (assoc self :active (canonize-suffix- self (:active self))))


(defn add-prefix [self, lci]
  (assert (map? self) "add-prefix requires map")
  (assert (:str self))
  (let [self (atom self)
        last_parent_node (atom -1)
        parent_node (atom nil)]
    (try

      (while true
        (do

          (println "update!!" (-> @self :active))
          (reset! parent_node (-> @self :active :sni))
          (if (explicit? (:active @self))
            (if (contains? (:edges @self) [(-> @self :active :sni) (nth (:str @self) lci)])
              (throw (InterruptedException.)))
            (let [e (get (:edges @self) [(-> @self :active :sni) (nth (:str @self) (-> @self :active :fci))])]
              (if (= (nth (:str @self) (+ (:fci e) (-> @self :active edge-length) 1))
                     (-> (:str @self) (nth lci)))
                (throw (InterruptedException.)))
              (let [[s idx] (split-edge @self, e (:active @self))]
                (reset! self s)
                (println "update")
                (reset! parent_node idx))))

                                        ;      (println self)


          ;; self.nodes.append(Node())
          (swap! self update-in [ :nodes] conj  (->node))
                                        ;(println :> @parent_node)
          (let [e (->edge [lci, (:n @self)]
                      [@parent_node (dec (count (:nodes @self)))])]
                                        ;(println :>> (:sni e) e)
            (swap! self insert-edge e))

          (when (pos? @last_parent_node)
            (swap! self assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))
          (reset! last_parent_node @parent_node)

          (if (zero? (-> @self :active :sni))
            (swap! self update-in [:active :fci] inc)
            (swap! self assoc-in [:active :sni]
                   (get-in @self [:nodes (-> @self :active :sni) :suffix_node])))

          (swap! self canonize-suffix)
          ;;(swap! self assoc :active (canonize-suffix @self (:active @self)))

          ))

      (catch InterruptedException e nil))

    (when (pos? @last_parent_node)
      (swap! self assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))

    (swap! self update-in [:active :lci] inc)
    (swap! self canonize-suffix)
    ;;(swap! self assoc :active (canonize-suffix @self (:active @self)))

    @self))

(defn st [s]
  (let [t {:str s
           :n (dec (count s))
           :nodes [(->node)]
           :edges {}
           :active (->suffix 0 0 -1)}]
    (reduce add-prefix t (range (count s)))))

(st "cacao")

;(st "abcd")

(defn scontains? [subs idx fulltxt]
  (if (seq subs)
    (if (= (nth fulltxt idx) (first subs))
      (recur (next subs) (inc idx) fulltxt)
      false)
    true))

(assert (scontains? [2 3] 2 [0 1 2 3 4 5]))


(defn find-subs [tree s]
  (let [|s| (count s)
        tree-str (:str tree) tree-edges (:edges tree)]
    (loop [cur_node 0, i 0, ln nil, edge nil]
      (if (< i |s|)
        (when-some [edge (get tree-edges [cur_node (nth s i)])]
          (let [ln (min (inc (edge-length edge)) (- |s| i))]
            (when (scontains? (subvec (vec s) i (+ i ln))
                              (:fci edge) tree-str)
              (recur (:dni edge) (+ i (edge-length edge) 1) ln edge))))
        (+ (:fci edge) (- |s|) ln)))))

(assert (nil? (find-subs (st "dolorem ipsum dolor sit amet") "ipsumedo")))
(assert (integer? (find-subs (st "dolorem ipsum dolor sit amet") "ipsum")))


;; TODO: massive debug!! pl.: tuple elso fele miert mindig nil??
