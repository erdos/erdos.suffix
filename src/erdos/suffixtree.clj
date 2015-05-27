(ns erdos.suffixtree)

;; not used yet.

;; for strategies in fast string search

(defprotocol StringSearch
  (prefix-of [_ pref])
  (postfix-of [_ post])
  (contains [_ part])
  (palindromes [_ part]))

;; naive approach

                                        ; Prefix-trie

;; O(N2) building time, O(N2) space

(defn trie-add1 [trie xs]
  (->> (seq xs)
       (iterate next)
       (take-while some?)
       (reduce #(update-in %1 (conj (vec %2) :RES)  conj xs) trie)))

(defn ->trie [& xs]
  (reduce trie-add1 {} xs))

(defn ukkonen [trie xs]
  )

(defn st-init [xs]
  (let [root (atom {})]

    ))

(defn follow-path [tree, s]
  (let [cur (atom (:root tree))
        i   0]

    ))

                                        ; Prefix-tree

;; O(N2) -> O(N) space
(defn trie->tree [trie]

  (clojure.walk/postwalk
   (fn [x] (if (and (map? x))
            (into {}
                  (for [[k v] x]
                    (if (and (map? v) (= 1 (count v)))
                      (let [[a b] (first v)]
                        [(cons k a) b]
                        #_(if (vector? a) [(conj a k) b]
                            [[k a] b]))
                      [(list k) v])))

            x))
h   trie))

(defn tree-result [subtree]
  (set (flatten (filter sequential? (tree-seq map? vals subtree)))))

(defn tree-find [tree word]
  (if-let [word (seq word)]
    (if-let [[k v] (some (partial find tree)
                         (prefixes word))]
      (if (= k word)
        (tree-result v)
        (recur v (drop (count k) word)))
      (if-let [k (first (filter (partial prefix? word) (keys tree)))]
        (recur (tree k) (drop (count k) word))
        nil))
    (tree-result tree)))

;; cli repl

(defmulti main- first)

(defmethod main- :default [_]
  (->>
   ["Arguments:"
    "index input.txt index.txt - "
    "find index.txt -"]
   (map println) (dorun)))

(defmethod main "index"
  [[_ input index]]
  (let [r (clojure.java.io/reader (str input))
        ls (line-seq r)
        tree (->> ls (apply ->trie) trie->tree)]
    (spit (str index) (str tree))))

(defmethod main "find"
  [[_ index]]
  (let [tree (read-string (slurp index))]
    (doseq [ln (line-seq *in*)
            :while (not= i ":q!")
            :let [fs (tree-find tree ln)]]
      (println (clojure.string/join " " fs)))))

(defn main [& args]
  (try (main- args)
       (finally (shutdown-agents))))


(comment

(prefix? "aba" "abacus")

(time (tree-find tree0 "erd"))

(tree-find

   (trie->tree (->trie "abacus" "macko"))



   (count trie0)


   (def trie0 (apply ->trie
           (line-seq (clojure.java.io/reader "/home/jano/wordlist-hu-0.3/list/freedict"))))

   (def tree0 (trie->tree trie0))


   "ack")

  (clojure.walk/prewalk #(if (coll? %) (seq %) %) (->trie "abacus"))


  )




(defn node [w, off, len] {:word w, :off off, :len len, :link 0})

(def empty-suffix-tree
  {;;:word "";; str
   :nodes {0 {}} ;; i => {:offset int, :len int, :children #{chr/str => node-idx, :link :idx}}
   })

'''
(def add-link [tree, node to-node]
  (assoc-in tree [:nodes node :link] to-node))


'''
(defn add-word-step [tree w]
  (loop [idx 0
         [w0 & ws :as w] (seq w)]
    (if (seq ws)
      (or
       (if-let [id' (-> tree :nodes (get idx) :children (get w0))]
         (recur id' ws)
         (if-let [k' (some (set (prefixes w)) (-> tree :nodes (get idx) :children keys))]
           (recur
            (-> tree (assoc [.../././././.]))
            (-> tree :nodes (get idx) :children (get k'))

                  (drop (count k') w))
           (let [id (count (:nodes tree))]
             (-> tree
                 (assoc-in [:nodes id] {:w w})
                 (assoc-in [:nodes idx :children w] id))))))
      (let [id (count (:nodes tree))]
        (-> tree
            (assoc-in [:nodes id] {:w w})
            (assoc-in [:nodes idx :children w] id))))
    ))

(defn add-word [tree w]
  (reduce add-word-step tree (suffixes w)))
;; (add-word empty-suffix-tree "asdfa")


(defn prefixes [xs]
  (take-while some? (iterate butlast (seq xs))))

(defn suffixes [xs]
  (take-while some? (iterate next xs)))

(defn prefix? [word xs]
  (loop [w word, x xs]
    (if (seq w)
      (if (= (first w) (first x))
        (recur (next w) (next xs))
        false)
      true)))

;; (prefix? "ab" "abacus")
;; (prefix? nil "asd")
;; (prefix? "aba" "ab")

(defn suffix? [word xs]
  (prefix? (reverse word) (reverse xs)))

(defn subseqs [xs]
  (mapcat prefixes (suffixes xs)))

;; (subseqs [1 2 3 4 5 6])

;; returns a lazy seq of words containing xs
(defn words [tree xs]

  )

(defn contains? [tree xs] (some? (seq (words tree xs))))

;; seq of words that it is a prefix of
(defn prefix-of [tree xs])

(defn suffix-of [tree xs])














;; class SuffixTree
;; Node[] nodes
;; char[] text
;; int root, pos=-1, currentNode, needSUffixLink, remainder, active_node active_length, active_edge

;; class Node
;; int start, end=oo, link
;; {Char => Int} next
;; edgeLength = min(end, pos+1) - start

(def empty-stree
  {:nodes {1 {:start -1 :end -1}} ;; int->Node
   :text {} ;; int->char
   :root 1, :position -1, :current-node 0, :needSuffixLink 0
   :active-node 1, :active-length 0, :active-edge 0
   })

(defn walk-down [stree nxt]
  (let [next-el (:edge-length (nth (:nodes stree) nxt))]
    (when (>= (:active-length stree) next-el)
      (assoc stree
        :active-edge (+ (:active-edge stree) next-el)
        :active-length (- (:active-length stree) next-el)
        :active-node nxt))))

(defn edge-length [{:keys [start end position]}]
  (- (min end, (inc position)) start))

(defn active-edge [{:keys [text active-edge]}]
  (int (get text active-edge)))

(defn new-node [stree, start]
  (-> stree
      (update-in [:current-node] inc)
      (assoc-in [:nodes (inc (:current-node stree))]
                {:start start})))

(defn add-suffix-link [stree node]
  (assert (map? stree))
  (assert (integer? node))
  (-> stree

      (cond-> (pos? (:needSuffixLink stree))
              (assoc-in [:nodes (:needSuffixLink stree) :link] node))
      (assoc :needSuffixLink node)))


(defn add-char [stree, c]
  (assert (map? stree))
  (assert (char? c))
  (let [position (-> stree :position inc)
        text (assoc (:text stree) position c)
        needSuffixLink -1
        active-edge (if (zero? (:active-length stree))
                      (:position stree) (:active-edge stree))


        ]
    (as-> stree <S>
          (update-in <S> [:position] inc)
          (assoc-in <S> [:text (:position <S>)] c)
          (assoc <S> :needSuffixLink -1)
          (if (zero? (:active-length <S>))
            (assoc <S> :active-edge (:position <S>)) <S>)

          (if-not (-> <S> :nodes (get (:active-node <S>)) :next (contains? (:active-edge <S>)))
            (let [leaf (new-node <S> (:position <S>))]
              (-> <S>
                  (assoc-in [:nodes (:active-node <S>) :next (:active-edge <S>)] leaf)
                  (add-suffix-link (:active-node <S>))
                  ;; rule 2
                  ))
            (let [next (-> <S> :nodes (get (:active-node <S>)) :next (get (:active-edge <S>)))]
              (or (walk-down <S>)
                  (as-> <S> <S>
                        (if (= c (get (:text <S>) (+ (:start (get (:nodes <S>) (:next <S>))) (:active-length <S>))))
                          (-> <S>
                              (assoc
                                :active-length (inc (:active-length <S>)))
                              (add-suffix-link (:active-node <S>)))
                          <S>)

                        (new-node <S> (-> <S> :nodes (get next) :start))
                        (let [split (:current-node <S>)
                              <S> (assoc-in <S> [:nodes (:active-node <S>) :next (:active-edge <S>)] split)
                              <S> (new-node <S> (:position <S>))
                              leaf (:current-node <S>)
                              <S> (assoc-in <S> [:nodes split :next c] leaf)
                              <S> (update-in <S> [:nodes next :start] + (:active-length <S>))
                              <S> (assoc-in <S> [:nodes split :next (get-in <S> [:text (:start (get (:nodes <S>) next))])] next)]

                          (add-suffix-link <S> split))

                        ))))
          (do (println <S>) <S>)
          ;; loop with remainder may take place here.

          (if (and (= (:active-node <S>) (:root <S>)) (pos? (:active-length <S>)))
            (assoc <S> :active-length
                   (-> <S> :active-length :dec)
                   :active-edge (+ (- (:position <S>) (:remainder <S>)) +1))
            (assoc <S> :active-node
                   (if (pos? (-> <S> :nodes (get (:active-node <S>)) :link))
                     (-> <S> :nodes (get (:active-node <S>)) :link)
                     (:root <S>))) ;; rule 3
            ))))

;(add-char empty-stree \a)



;; http://pastie.org/5925812#9,84



(defn suffix-tree [txt]
  {})

;; suffix faa.
