(ns erdos.gst
  (:require [erdos.suffixtree :as est]))

(est/->suffixtree "doloremipsumdolo")

(defn mk-map []
  (let [a (atom 0)]
    (memoize (fn [& xs] (swap! a inc)))))

(defn cpl
  "common prefix length"
  [a b]
  (loop [a (seq a), b (seq b), i 0]
    (cond (nil? a)   i
          (nil? b)   i
          (not= (first a) (first b)) i
          :otherwise (recur (next a) (next b) (inc i)))))



;; (cpl "asdf" "asfd")

(defn group-by* [keyfn valfn coll]
  (let [m (group-by keyfn coll)]
    (zipmap (keys m)
            (for [v (vals m)] (map valfn v)))))

(defn part [as bs]
  (let [as (set as) bs (set bs)]
    [(remove bs as)
     (filter bs as)
     (remove as bs)]))

(defn cslen
  "common substring length"
  [s1 beg1 end1, s2 beg2, end2]
  (loop [i 0]
    (if (and (< i (min (- end1 beg1) (- end2 beg2)))
             (= (nth s1 (+ beg1 i))
                (nth s2 (+ beg2 i))))
      (recur (inc i))
      i)))

;(= 3 (cslen "abacus" 1 5 "ebacee" 1 6))


'



;; seems ok.
;; G*V*G*E =>G
;; copy given edges of source tree to destree (recursively)

(defn copy-tree [dest-tree dest-root, source-tree source-edge]
  (let [node->chars (group-by* first second (-> source-tree :edges keys))]
    (loop [dest-tree dest-tree
          xs        [[dest-root source-edge]]]
     (if-let [[dest-root {:keys [fci lci dni]}] (first xs)]
       (let [idx (-> dest-tree :nodes count)
             chr (-> source-tree :str (nth fci))
             new-edge {:fci fci :lci lci :dni idx :sni dest-root :k :k}]
         (recur
          (-> dest-tree
              (update-in [:nodes] conj {})
              (assoc-in  [:edges [dest-root chr]] new-edge))
          (concat (for [c (node->chars dni)
                        :let [ed (-> source-tree :edges (get [dni c]))]]
                    [idx ed]) (rest xs))))
       dest-tree))))

(copy-tree {:nodes [{}]}
           0
           {:nodes [{} {} {}]
            :str "abcdefgh"
            :edges {[0 \a] {:fci 0 :lci 1 :dni 1}
                    [1 \b] {:fci 1 :lci 2 :dni 2}}}
           {:fci 0 :lci 1 :dni 1})


(defn edge-split [tree edge idx]
  (let [i (-> tree :nodes count)
        c1 (-> tree :str (nth (:fci edge)))
        c2 (-> tree :str (nth idx))
        e1 {:fci (:fci edge), :lci idx, :sni (:sni edge), :dni i}
        e2 {:fci idx, :lci (:lci edge), :sni i, :dni (:dni edge) }]
    [(-> tree
         (update-in [:nodes] conj {})
         (assoc-in [:edges [(:sni edge) c1]] e1)
         (assoc-in [:edges [i c2]]           e2))
     e2]))

(edge-split {:nodes [{} {}]
             :str "abcdef"
             :edges { [0 \a] {:sni 0 :dni 1 :fci 0 :lci 4}}}
            {:sni 0 :dni 1 :fci 0 :lci 4} 2)


;; G*E*G*E -> G*V*G*V
(defn handle-ab [tree1 edge1 tree2 edge2]
  (assert (every? map? [tree1 edge1 tree2 edge2]))
  ;; assert: len>0
  (let [e1w (- (:lci edge1) (:fci edge1)),
        e2w (- (:lci edge2) (:fci edge2)),
        len (cslen (:str tree1)
                   (:fci edge1) (:lci edge1)
                   (:str tree2)
                   (:fci edge2) (:lci edge2))]
;    (println len e1w e2w)
    ;(assert (pos? len) "a ket elnek legyen kozos prefixe!")
    (cond
     ;(= 0 e1w e2w)    [tree1 (:dni edge1) tree2 (:dni edge2)]
     ;(zero? len)     (assert false "kell legyen kozos prefix!!")
     (= e1w e2w len)   [tree1 (:dni edge1) tree2 (:dni edge2)]
     (= e1w len)       (let [[tree2 e2] (edge-split tree2 edge2 (:lci edge1))]
                         [tree1 (:dni edge1) tree2 (:dni e2)])
     (= e2w len)       (let [[tree1 e1] (edge-split tree1 edge1 (:lci edge2))]
                         [tree1 (:dni e1) tree2 (:dni edge2)])
     :else             (let [[tree1 edge1] (edge-split tree1 edge1 (+ len (:fci edge1)))
                             [tree2 edge2] (edge-split tree2 edge2 (+ len (:fci edge2)))]
                         [tree1 (:dni edge1) tree2 (:dni edge2)]))))



;; G*V*G*V -> G
(defn merge-trees
  [tree1 root1 tree2 root2]
  (let [state (atom [tree1 root1 tree2 root2])]
    ;; assert -- legyen kozos prefixuk a root-tol kezdve!!!
    (let []
      (let [[_ ab-keys b-keys]
            ,,,(part ;; XXX this is slow, precache maybe?
                (for [[[a b] v] (:edges tree1) :when (= a root1)] b)
                (for [[[a b] v] (:edges tree2) :when (= a root2)] b)
                )]
        (-> (as-> tree1 tree
                  ;; bs only
                  (reduce (fn [a x] (copy-tree a root1 tree2 x))
                          tree (for [b b-keys] (-> tree2 :edges (get [root2 b]))))
                  (reduce (fn [tree k]
                            (let [[t1 n2 t2 n2] (handle-ab tree  (-> tree  :edges (get [root1 k]))
                                                           tree2 (-> tree2 :edges (get [root2 k])))]
                              (merge-trees t1 n2 t2 n2)))
                          tree ab-keys)))))))

(defn join [t1 t2]
  (merge-trees t1 0 t2 0))




#_
(let [tree1 (est/->suffixtree "latjatuk feleim szumtukhel mik vogymuk")
      tree2 (est/->suffixtree "isa pur es homou vogymuk")]
  (-> (join tree1 tree2)
      (est/index-of "szumtu")))


#_
(let [tree1 (est/->suffixtree "dolorempsumdolo")
      tree2 (est/->suffixtree "dumpsteripsolore")

      [word1 word2 w1idx w2idx] [(:str tree1) (:str tree2) 0 1]

      b-edges (group-by* first (comp set second) (keys (:edges tree2)))

      ;; returns triple of [[only-in-a] [common] [only-in-b]]
      find-common-edges (fn [tree1 anode, bnode] ;; int * int => [[() () ()]]
                          (let [pf (partial contains? (:edges tree1))
                                bf (b-edges bnode)
                                af (set (for [i bf :let [k [anode i]] :when (pf k)] k))]
                            [(if bf (remove bf af) af)
                             (when af (filter af bf))
                             (if bf (remove af bf) bf)]))

      tree-insert-node (fn [tree rootnode fci lci words]
                         (let [n {}, i (-> tree :nodes count),
                               c (nth (first words) fci)
                               edge {:fci fci :lci lci :words words :sni rootnode :dni i}]
                           [(-> tree
                                (update-in [:nodes] conj n)
                                (assoc-in [:edges [rootnode c]]))
                            edge]))

      migrate-tree (fn [destree destroot, othertree-edge])

      ;; a baloldali faba olvasztjk a jobboldalit
      find-conf (fn f [tree [aedge bedge]]
                  (let [len (cslen word1 (:fci aedge) (:lci aedge)
                                   word2 (:fci bedge) (:lci bedge))
                        awidth (- (:lci aedge) (:fci aedge))
                        bwidth (- (:lci bedge) (:fci bedge))]
                    (cond
                     (= awidth bwidth len)
                     (let [[as abs bs] (find-common-edges tree (:dni aedge) (:dni bedge))]
                       [(-> tree
                            ;; as mar benne van
                            (migrate-tree tree (:sni aedge) bs) ;bs-t is atvisszuk
                            )
                        (for [c abs]
                          [(-> tree :edges (get [(:dni aedge) c]))
                           (-> tree2 :edges (get [(:dni bedge) c]))])])
                     (= awidth len) ;; awidth=len < bwidth
                     -1
                     (= bwidth len) ;; awidth < bwidth=len
                     -1
                     (<= len awidth bwidth)
                     ;; -- 0-len -- o ==
                     -1
                     (<= len bwidth awidth)
                     -1
                     :otherwise
                     (assert false) ;; will not hapen.

                     )
                    ))


      ;; optimalizalhatunk algebraval
      ;; root node mindket esetben 0
      ]
  '(loop [tree {}, xs []])
tree1
  )
