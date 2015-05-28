(ns erdos.suffixtree2
  (:require [erdos.suffix.core :as sc :refer [find-word find-word-idx]]))

(defn- ->node []
  {:suffix_node -1})

;; first char idx, last char idx, source node idx, dest node idx
(defn- ->edge [[fci lci] [sni dni] word-index]
  (assert sni)
  {:fci fci, :lci lci, :sni sni, :dni dni :wi word-index})

(defn- edge-length [n]
  (- (:lci n) (:fci n)))

;; source node indes, first char index, last char index
(defn- ->suffix [sni, fci, lci]
  (assert (and sni fci lci))
  {:sni sni, :fci fci, :lci lci})

(def empty-suffix {:sni 0 :fci 0 :lci -1})

(def suffix-length edge-length)

(defn- explicit? [suffix]
  (> (:fci suffix) (:lci suffix)))

(def implicit (complement explicit?))

(defn- insert-edge [self, edge]
  (assoc-in self [:edges [(:sni edge) (-> self :words (nth (:wi edge)) (nth (:fci edge)))]] edge))

(defn- remove-edge [self {:keys [sni fci wi]}]
  (assoc-in self [:edges [sni   (-> self :words (nth wi) (nth fci))]] nil))

(defn- split-edge [self, edge, suffix]
  (let [self (update-in self [:nodes] conj (->node))
        e (->edge [(:fci edge)    (+ (:fci edge) (suffix-length suffix) )]
                  [(:sni suffix)  (dec (count (:nodes self)))]
                  (:wi edge)
                 ; (:w self) ;; ??!!
                  ) ;; talan a current word kell ide?
        self (remove-edge self edge)
        self (insert-edge self e)
        self (assoc-in self [:nodes (:dni e) :suffix_node] (:sni suffix))
        edge (update-in edge [:fci] + (inc (suffix-length suffix)))
        edge (assoc-in edge [:sni] (:dni e))
        ;; edge (assoc-in edge [:wi] (:w self)) ;;; ???
        self (insert-edge self edge)]
    [self, (:dni e)]))

(defn- canonize-suffix- [self, suffix]
  (if-not (explicit? suffix)
    (let [e (get-in self [:edges [(:sni suffix) (-> (:words self) (nth (:w self)) (nth (:fci suffix)))]])]
      (assert e)
      (if (<= (edge-length e) (suffix-length suffix))
        (recur self
               (assoc suffix
                 :fci (+ (:fci suffix) 1 (edge-length e))
                 :sni (:dni e)))
        suffix))
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
        (do
          (println :> (:active @self))
          (reset! parent_node (-> @self :active :sni))
          (assert @parent_node) ;; ez itt vmiert null!!
          (if (explicit? (:active @self))
            (if (contains? (:edges @self) [(-> @self :active :sni)
                                           (-> @self :words (nth (:w @self)) (nth lci))])
              (throw (InterruptedException.)))
            (let [e (get (:edges @self) [(-> @self :active :sni)
                                         (-> @self :words (nth (:w @self)) (nth (-> @self :active :fci)))])]
              (if (= (nth
                      ;(:str @self)
                      (-> @self :words (nth (:wi e)))

                      (+ (:fci e) (-> @self :active edge-length) 1))

                    ;  (-> @self :words (nth (:wi e)) (nth lci))
                    (-> (:str @self) (nth lci))
                      )
                (throw (InterruptedException.)))
              (let [[s idx] (split-edge @self, e (:active @self))]
                (assert idx "split edge must not return empty idx")
                (reset! self s)
                (reset! parent_node idx))))

          (println :>> (:active @self)) ;; itt az active->sni=-1

          (as-> @self *
                (update-in * [:nodes] conj  (->node))
                (insert-edge * (->edge [lci, (:n *)]
                                       [@parent_node (dec (count (:nodes *)))]
                                       (:w *)))
                (cond-> *
                        (pos? @last_parent_node)
                        (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))

                (do (println :|>> (:active *)) *)



                ;; itt van a gebasz, mi van, ha active|sni=-1
                (if (zero? (-> * :active :sni))
                  (update-in * [:active :fci] inc)
                  (assoc-in * [:active :sni] ;; nem kene nulla legyen
                            (get-in * [:nodes (-> * :active :sni) :suffix_node])))

                (do (println :|>> (:active *)) *)

                (canonize-suffix *)
                (reset! self *))

          (println :>>> (:active @self))

          (reset! last_parent_node @parent_node)))

      (catch InterruptedException e nil)) ;; end of while loop

    (-> @self
        (cond-> (pos? @last_parent_node)
                (assoc-in [:nodes @last_parent_node :suffix_node] @parent_node))
        (update-in [:active :lci] inc)
        (canonize-suffix))))

(defn- put-word- [tree s]
  (let [word-idx (count (:words tree))]
    (as-> tree *
          (assoc *
            :str s
            :n       (dec (count s))
            :active  empty-suffix
            :w     word-idx
            :words (conj (:words * []) s))
          (reduce add-prefix * (range (count s))))))

(defn- scontains? [subs idx fulltxt]
  (if (seq subs)
    (if (= (nth fulltxt idx) (first subs))
      (recur (next subs) (inc idx) fulltxt)
      false)
    true))


(defn find-word-idx- [tree s]
  (let [|s| (count s)
        tree-edges (:edges tree)]
    (loop [cur_node 0, i 0, ln nil, edge nil]
      (if (< i |s|)
        (when-some [edge (get tree-edges [cur_node (nth s i)])]
          (let [ln (min (inc (edge-length edge)) (- |s| i))]
            (when (scontains? (subvec (vec s) i (+ i ln))
                              (:fci edge) (-> tree :words (nth (:wi edge)) vec))
              (recur (:dni edge) (+ i (edge-length edge) 1) ln edge))))
        [(-> tree :words (nth (:wi edge)))
         (+ (:fci edge) (- |s|) ln)]))))

(defrecord SuffixTree [str n words edges nodes active]
  sc/IOnline
  (put-word [tree s] (put-word- tree s))
  sc/IFindWord
  (find-word [t s] (first (find-word-idx- t s)))
  sc/IFindWordIdx
  (find-word-idx [t s] (find-word-idx- t s)))


; (assert (scontains? [2 3] 2 [0 1 2 3 4 5]))

(defn ->suffixtree [& xs]
  (reduce sc/put-word (->SuffixTree nil nil [] {} [(->node)] nil) xs))

(comment
  (time (count (time (->suffixtree (time (slurp "/home/jano/wordlist-hu-0.3/list/freedict"))))))
  (assert (nil? (find-subs (->suffixtree "dolorem ipsum dolor sit amet") "ipsumedo")))
  (assert (integer? (find-subs (->suffixtree "dolorem ipsum dolor sit amet") "olo")))
  (find-subs-data (->suffixtree "dolorem ipsum dolor sit amet") "olo")
  :OK)



(let [w "kukutyatya"
      t (->suffixtree w)]
  (assert (= (find-word t "utya") w))
  (assert (= (find-word t "tya") w))
  (assert (= (find-word t "ty") w))
  (assert (= (find-word t "ya") w))
  t)



(let [t (->suffixtree "kutya" "macska")]
  (assert (= (find-word t "utya") "kutya"))
  (assert (= (find-word t "tya") "kutya"))
  (assert (= (find-word t "ty") "kutya"))
  (assert (= (find-word t "ya") "kutya"))

  (assert (= (find-word t "macska") "macska"))
  (assert (= (find-word t "acska") "macska"))
  (assert (= (find-word t "macs") "macska"))
  (assert (= (find-word t "mac") "macska"))
  (assert (= (find-word t "m") "macska"))


  (assert (= (find-word t "acska") "macska"))
;  (assert (= (find-word t "ska") "macska"))
  ;(assert (= (find-word t "ka") "macska"))
(find-word t "ska")
t)

(->suffixtree "kutya" "macska")
