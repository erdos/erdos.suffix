(ns erdos.suffixtree3)

;;
;; in active development
;; do not compile
;;
;;

(def node-empty {:suffix_node -1})

(defn- ->edge [fci len sni dni wm]
  {:fci fci, :sni sni, :dni dni, :len len :wm wm})

(defn- explicit? [{len :len}]
  (> 1 len))

(def implicit (complement explicit?))

(defn edge-char [self edge i]
  (let [[k v] (first (:wm edge))]
    (-> self :words (nth k) (nth (+ i k)))))

(defn- insert-edge [self, edge]
  (assoc-in self [:edges [(:sni edge) (edge-char self edge (:fci edge))]] edge))

(defn- remove-edge [self edge]
  (assoc-in self [:edges [(:sni edge) (edge-char self edge (:fci edge))]] nil))

(defn split-edge [self, edge, suffix e1<-m]
  (let [self (update-in self [:nodes] conj node-empty)
        e (->edge (:fci edge)    (:len suffix)
                  (:sni suffix)  (dec (count (:nodes self)))
                  (into (:wm edge) e1<-m))
        self (remove-edge self edge)
        self (insert-edge self e)

        edge (->edge (+ (:fci edge) (:len suffix)) (- (:len edge) (:len suffix))
                     (:dni e) (:dni edge) (:wm edge))

        self (assoc-in self [:nodes (:dni e) :suffix_node] (:sni suffix))
        self (insert-edge self edge)]
    [self, (:dni e)]))



(defn- canonize-suffix- [self, suffix]
  (if-not (explicit? suffix)
    (let [word (nth (:words self) (:wi suffix)) ;; maybe edge word?
          e (get-in self [:edges [(:sni suffix) (nth word (:fci suffix))]])]
      (if (<= (:len e) (:len suffix))
        (recur self
               {:fci (+ (:fci suffix) (:len e))
                :len (- (:len suffix) (:len e))
                :sni (:dni e)
                :wi (:wi suffix)})
        suffix))
    suffix))

;; kenyelmi funkcio, ugyis csak innen hivjuk.
(defn- canonize-suffix [self]
  (assoc self :active (canonize-suffix- self (:active self))))



(defn- add-prefix [self, lci]
  (let [word-idx (-> self :active :wi)
        word (nth (:words self) word-idx)
        self (atom self)
        last_parent_node (atom -1)
        parent_node (atom 0)] (assert word)
    (try
      (while

          ;true
        (do
          (println "while")
          (println parent_node)
          (println last_parent_node)
          (println self)
          (println " ")

          (Thread/sleep 100)
          ;(println parent_node)
          (reset! parent_node (-> @self :active :sni))

          (if (explicit? (:active @self))
            (if (contains? (:edges @self) [(-> @self :active :sni) (nth word lci)])
              (do
                (println "aa0")
                (throw (InterruptedException.))))
            (let [e (get (:edges @self)
                         [(-> @self :active :sni)
                          (nth word (-> @self :active :fci))])]
              (assert (some? e))

              (if (= (edge-char @self e (-> @self :active :len inc)) ;; talan inc kell ide.
                     (nth word lci))
                (do (println "aaa") (throw (InterruptedException.))))
              (let [[s idx] (split-edge @self, e (:active @self)
                                        ;; XXX not sure about fci in next line
                                        {word-idx (:fci (:active @self))})]
                (reset! self s)
                (reset! parent_node idx))))

          (as-> @self *
                (update-in * [:nodes] conj  node-empty)
                (insert-edge * (->edge lci, (- (count word) lci -1)
                                       @parent_node (dec (count (:nodes *)))
                                       {word-idx lci}))
                (cond-> *
                        (pos? @last_parent_node)
                        (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))
                (if (zero? (-> * :active :sni (or 0)))
                  (-> * (update-in [:active :fci] inc)
                      (update-in [:active :len] dec))
                  (assoc-in * [:active :sni]
                            (get-in * [:nodes (-> * :active :sni) :suffix_node])))
                (canonize-suffix *)
                (reset! self *))
          (reset! last_parent_node @parent_node)))
      (catch InterruptedException e nil)) ;; end of while loop
    (-> @self
        (cond-> (pos? @last_parent_node)
                (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))
        (update-in [:active :len] inc)
        (canonize-suffix))))

(def tree-empty
  {:words [] :nodes [node-empty] :edges {} :len 0 :sni 0})

(def suffix-empty {:fci 0 :len 0 :sni 0 :wi nil})

(defn reducer [t s]
  (as-> t t
        (update-in t [:words] conj s)
        (assoc t :active suffix-empty)
        (assoc-in t [:active :wi] (dec (count (:words t))))
        (reduce add-prefix t (range (count s)))
        (dissoc t :active)))

(defn ->suffixtree [& xs]
  (reduce reducer tree-empty xs))

;(time (some? (->suffixtree "there is no law its a . dolorem ipsum dolor sit. latjatuk")))
;(->suffixtree "dolorem ipsum dolor sit amet consectetuer")


;; gets into infinite loop
;;(->suffixtree "ABAB" "qwer")
