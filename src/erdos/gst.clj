(ns erdos.gst
  (:require [erdos.suffixtree :as est]))

(declare edge-var handle-ab join copy-tree)


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
;(= 3 (cslen "xabacus" 2 5 "ebacee" 1 6))
;  (cslen "xxxxx" 2 5 "aaaaaa" 1 6)
;  (cslen "vxxxxx" 0 5 "vaaaaaa" 0 6)
;  (cslen "vxxxxx" 0 5 "vaaaaaa" 0 0)

;(cslen "va" 0 2, "va" 0 2)



;; seems ok.
;; G*V*G*E =>G
;; copy given edges of source tree to destree (recursively)

(defn copy-tree [dest-tree dest-root, source-tree source-edge]
  (println "-")
  (println "should copy " source-edge "to" dest-root)
  (println :src source-tree)
  (println :dest dest-tree)
  (println "_")
  (assert (contains? (:nodes dest-tree) dest-root) "Root is not in tree")
  (assert (contains? (:nodes source-tree) (:sni source-edge)))
  (assert (contains? (:nodes source-tree) (:dni source-edge)))
  (let [node->chars (group-by* first second (-> source-tree :edges keys))]
    (loop [dest-tree dest-tree
           xs        [[dest-root source-edge]]]
     (if-let [[dest-root {:keys [fci lci dni ws]}] (first xs)]
       (let [idx (-> dest-tree :nodes count)
             chr (-> source-tree :words (nth (or (first ws) 0)) (nth fci) )
             new-edge {:fci fci :lci lci
                       :dni idx :sni dest-root
                       :_ :cp, :ws (map (:wmap source-tree) ws) }]
         (println :beszurando new-edge)
         (recur
          (-> dest-tree
              (update-in [:nodes] conj {})
              (assoc-in  [:edges [dest-root chr]] new-edge))
          (concat (for [c (node->chars dni)
                        :let [ed (-> source-tree :edges (get [dni c]))]]
                    [idx ed]) (rest xs))))
       dest-tree))))


(msuffixtree "x" "xa")

;; ok
(defn edge-split [tree edge idx e1<-ws]
  (let [i (-> tree :nodes count)
        c1 (edge-char tree edge)
        c2 (edge-char tree edge (inc idx))
        e1 {:fci (:fci edge) :lci idx,
            :sni (:sni edge), :dni i,
            :ws (set (concat (:ws edge) e1<-ws))}
        e2 {:fci (inc idx) :lci (:lci edge),
            :sni i, :dni (:dni edge),
            :ws (:ws edge)}]
    (println "splitting edge" edge "to" e1 e2)
    [(-> tree
         (update-in [:nodes] conj {})
         (assoc-in [:edges [(:sni e1) c1]] e1)
         (assoc-in [:edges [(:sni e2) c2]] e2))
     e2]))

(edge-split {:nodes [{} {}]
             :str "abcdef"  :words ["abcde"]
             :edges { [0 \a] {:sni 0 :dni 1 :fci 0 :lci 4 :ws [0]}}}
            {:sni 0 :dni 1 :fci 0 :lci 4 :ws [0]} 2 nil)

(defn edge-words [tree edge]
  (if-let [ws (:words tree)]
    (if-let [w (:ws edge)]
      (map ws w)
      [(first ws)])
    [(:str tree)]))

(defn edge-char
  ([tree edge] (edge-char tree edge nil))
  ([tree edge i]
   (if-let [w (first (:ws edge))]
     (-> (:words tree) (nth w) (nth (or i (:fci edge))))
     (-> (:words tree) (first) (nth (or i (:fci edge)))))))

(defn tree-edge-update [tree edge f & args]
  (apply update-in tree [:edges [(:sni edge) (nth (first (edge-words tree edge)) (:fci edge))]] f args))

;; G*E*G*E -> G*V*G*V, lefele haladunk a fan.
(defn handle-ab [tree1 edge1 tree2 edge2]
  (assert (every? map? [tree1 edge1 tree2 edge2]))
  (let [e1w (- (:lci edge1) (:fci edge1)),
        e2w (- (:lci edge2) (:fci edge2)),
        len (cslen (first (edge-words tree1 edge1)) (:fci edge1) (+ 1 (:lci edge1))
                   (first (edge-words tree2 edge2)) (:fci edge2) (+ 1 (:lci edge2))) ;; to prevent overflow
        len (dec len)]
    (assert (not (neg? len)) "nincs kozos prefix!")
    (cond
     (= e1w e2w len)   (let [tree1 (tree-edge-update tree1 edge1 update-in [:ws] concat (map (:wmap tree2) (:ws edge2)))]
                         [tree1 (:dni edge1) tree2 (:dni edge2)]) ;; ok

     (= e1w len)       (let [[tree2 e2] (edge-split tree2 edge2 (:lci edge1) nil)
                             tree1 (tree-edge-update tree1 edge1 update-in [:ws] concat (map (:wmap tree2) (:ws edge2)))]
                         ;(println "e1w vegig er"
                         [tree1 (:dni edge1) tree2 (:dni e2)]) ;; ezt piszkaltam dni+sni

     (= e2w len)       (let [[tree1 e1] (edge-split tree1 edge1 (:lci edge2)
                                                    (map (:wmap tree2) (:ws edge2)))]
                         [tree1 (:dni e1) tree2 (:dni edge2)])

     :else             (let [[tree1 e1] (edge-split tree1 edge1 (+ len (:fci edge1)) (map (:wmap tree2) (:ws edge2)))
                             [tree2 e2] (edge-split tree2 edge2 (+ len (:fci edge2)) nil)]
                         [tree1 (:dni e1) tree2 (:dni e2)])))) ;; XXX dni dni volt.


(msuffixtree "x" "xa")
(msuffixtree "xa" "x")


                                        ;1a huanyzik


;; Recursive algo, may throw stackoverflow exception
;; G*V*G*V -> G
(defn merge-trees [tree1 root1 tree2 root2]
  (let []
    (println 'merge-trees :root1= root1)
    ;; assert -- legyen kozos prefixuk a root-tol kezdve!!!
    (let []
      (let [[_ ab-keys b-keys]
            ,,,(part ;; XXX this is slow, precache maybe?
                (for [[[a b] v] (:edges tree1) :when (= a root1)] b)
                (for [[[a b] v] (:edges tree2) :when (= a root2)] b)
                )
            tree2-edges (for [b b-keys] (-> tree2 :edges (get [root2 b])))]
        (-> (as-> tree1 tree
                  ;; bs only
                  ;; (reduce (fn [a x] (update-in a [:edges [root1 x] :ws] (fn [c] (or )))))
                  (reduce (fn [tree edge] (copy-tree tree root1 tree2 edge)) tree tree2-edges)
                  (reduce (fn [tree k]
                            (let [[t1 n2 t2 n2] (handle-ab tree  (-> tree  :edges (get [root1 k]))
                                                           tree2 (-> tree2 :edges (get [root2 k])))]
                              (println :after-handle-ab :n2= n2)
                              (merge-trees t1 n2 t2 n2)))
                          tree ab-keys)))))))

(msuffixtree "x" "xa")



(defn join [t1 t2]
  (let [idxmap1 (if-let [ws (:words t1)] (vec (range (count ws))) [0])

        t1 (if (:words t1) t1
               (-> t1
                   (assoc :words [(:str t1)])
                   (dissoc :str :n)
                   (assoc :edges (into {} (for [[k m] (:edges t1)] [k (assoc m :ws #{0})])))))

        t2 (if (:words t2) t2
               (-> t2
                   (assoc :words [(:str t2)])
                   (dissoc :str :n)
                   (assoc :edges (into {} (for [[k m] (:edges t2)] [k (assoc m :ws #{0})])))))

        idxmap2  (mapv (partial + (count (:words t1))) (range (count (:words t2))))
        t2       (assoc t2 :wmap idxmap2)

        ]
    ;;(println (:wmap t2))
    (-> (merge-trees t1 0 t2 0)
        (assoc :words (vec (concat (:words t1) (:words t2)))))))

(defn msuffixtree [& xs]
  (reduce join (map est/->suffixtree xs)))

;;;(msuffixtree "abcd" "bcd")
(msuffixtree "a" "b") ;; OK

;; 4y hianyzik
(:edges (msuffixtree "ax" "ay")) ;; 4y hianyzik.

(msuffixtree "ax" "x") ;; OK
(msuffixtree "x" "ax") ;; OK

(msuffixtree "xa" "x") ; jo
(msuffixtree "x" "xa") ; 1a huanyzik

(msuffixtree "a" "a") ;; OK
(msuffixtree "aa" "aa");; OK

(msuffixtree "a" "aa") ;;1a hianyzik
(msuffixtree "aa" "a") ;;OK


;(msuffixtree "abc" "abcd")


;(join (est/->suffixtree "abcd") (est/->suffixtree "bcd"))

(join (est/->suffixtree "abcd") (est/->suffixtree "abxy"))



#_
(let [tree1 (est/->suffixtree "latjatuk feleim szumtukhel mik vogymuk")
      tree2 (est/->suffixtree "isa pur es homou vogymuk")]
  (-> (join tree1 tree2)
      (est/index-of "zum")))

#_
(let [tree1 (est/->suffixtree "latjatuk feleim szumtukhel mik vogymuk")
      tree2 (est/->suffixtree "isa pur es homou vogymuk menyi miloszt")]
  (-> (join tree1 tree2)
      (est/index-of "tjatuk")
      ))

#_
(let [tree1 (est/->suffixtree "aaabbb")
      tree2 (est/->suffixtree "ab")]
  (-> (join tree1 tree2)
      ;(est/index-of "menyi")
      ))
