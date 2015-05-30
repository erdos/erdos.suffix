(ns erdos.suffixtree)

(defn- ->node []
  {:suffix_node -1})

(defn- ->edge [[fci lci] [sni dni]]
  {:fci fci, :lci lci, :sni sni, :dni dni})

(defn- edge-length [n]
  (- (:lci n) (:fci n)))

(defn- ->suffix [sni, fci, lci]
  {:sni sni, :fci fci, :lci lci})

(def suffix-length edge-length)

(defn- explicit? [suffix]
  (> (:fci suffix) (:lci suffix)))

(def implicit (complement explicit?))

(defn- insert-edge [self, edge]
  (assoc-in self [:edges [(:sni edge) (nth (:str self) (:fci edge))]] edge))

(defn- remove-edge [self {:keys [sni fci]}]
  (assoc-in self [:edges [sni (nth (:str self) fci)]] nil))

(defn split-edge [self, edge, suffix]
  (let [self (update-in self [:nodes] conj (->node))
        e (->edge [(:fci edge)    (+ (:fci edge) (suffix-length suffix))]
                  [(:sni suffix)  (dec (count (:nodes self)))])
        self (remove-edge self edge)
        self (insert-edge self e)
        self (assoc-in self [:nodes (:dni e) :suffix_node] (:sni suffix))
        edge (update-in edge [:fci] + (inc (suffix-length suffix)))
        edge (assoc-in edge [:sni] (:dni e))
        self (insert-edge self edge)]
    [self, (:dni e)]))

(defn- canonize-suffix- [self, suffix]
  (if-not (explicit? suffix)
    (do ;(println "canonize suffix explicit" suffix)
     (let [e (get-in self [:edges [(:sni suffix) (nth (:str self) (:fci suffix))]])]
       (if (<= (edge-length e) (suffix-length suffix))
         (recur self
                (assoc suffix
                  :fci (+ (:fci suffix) 1 (edge-length e))
                  :sni (:dni e)))
         suffix)))
    suffix))

;; kenyelmi funkcio, ugyis csak innen hivjuk.
(defn- canonize-suffix [self]
  (assoc self :active (canonize-suffix- self (:active self))))

(defn- add-prefix [self, lci]

  (let [self (atom self)
        last_parent_node (atom -1)
        parent_node (atom 0)]
    (try
      (while true
        ;;(println "(while loop")
        (do
          (reset! parent_node (-> @self :active :sni))
          (if (explicit? (:active @self))
            (do ;;(println "explicit!" @self)
             (if (contains? (:edges @self) [(-> @self :active :sni) (nth (:str @self) lci)])
               (throw (InterruptedException.))))
            (do
              ;(println "not explicit!" @self)
              (let [e (get (:edges @self) [(-> @self :active :sni) (nth (:str @self) (-> @self :active :fci))])]
               ;(println " (let")
               (if (= (nth (:str @self) (+ (:fci e) (-> @self :active edge-length) 1))
                      (-> (:str @self) (nth lci)))
                 (throw (InterruptedException.)))
               (let [[s idx] (split-edge @self, e (:active @self))]
                 (reset! self s)
                 (reset! parent_node idx)))))

          (as-> @self *
                (update-in * [:nodes] conj  (->node))
                (insert-edge * (->edge [lci, (:n *)] [@parent_node (dec (count (:nodes *)))]))
                (cond-> *
                        (pos? @last_parent_node)
                        (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))

                (if (zero? (-> * :active :sni (or 0)))
                  (update-in * [:active :fci] inc)
                  (assoc-in * [:active :sni]
                            (get-in * [:nodes (-> * :active :sni) :suffix_node])))
                (canonize-suffix *)
                (reset! self *))

          (reset! last_parent_node @parent_node)))

      (catch InterruptedException e nil)) ;; end of while loop

    (-> @self
        (cond-> (pos? (or @last_parent_node -1))
                (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))
        (update-in [:active :lci] inc)
        (canonize-suffix))))

(defn ->suffixtree [s]
  (let [t {:str s
           :n (dec (count s))
           :nodes [(->node)]
           :edges {}
           :active (->suffix 0 0 -1)}]
    (-> (reduce add-prefix t (range (count s)))
        (dissoc :active))
    ))


                                        ; (->suffixtree "cacao")


(defn scontains? [subs idx fulltxt]
  (if (seq subs)
    (if (= (nth fulltxt idx) (first subs))
      (recur (next subs) (inc idx) fulltxt)
      false)
    true))

                                        ; (assert (scontains? [2 3] 2 [0 1 2 3 4 5]))


(defn index-of [tree s]
  (let [|s| (count s)
        ws (or (seq (:words tree)) [(:str tree)])


        tree-edges (:edges tree)]
;    (assert tree-str)
    (loop [cur_node 0, i 0, ln nil, edge nil]
      (if (< i |s|)
        (when-some [edge (get tree-edges [cur_node (nth s i)])]
          (let [ln (min (inc (edge-length edge)) (- |s| i))]
            ;(println (vec s) |s| i (+ i ln) edge)
            (when (scontains? (subvec (vec s) i (+ i ln))
                              (:fci edge) (nth ws (or (first (:ws edge)) 0)))
              (recur (:dni edge) (+ i (edge-length edge) 1) ln edge))))
        (+ (:fci edge) (- |s|) ln)))))



(->suffixtree "abab")
