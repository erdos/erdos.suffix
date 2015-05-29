(ns erdos.gst
  (:require [erdos.suffixtree :as est]
            [erdos.suffix.core :as c]))

(declare edge-var handle-ab join copy-tree merge-edges)


                                        ;(est/->suffixtree "doloremipsumdolo")


(defn group-by* [keyfn valfn coll]
  (let [m (group-by keyfn coll)]
    (zipmap (keys m)
            (for [v (vals m)] (map valfn v)))))

(defn part [as bs]
  (let [as (set as) bs (set bs)]
    [(remove bs as)
     (filter bs as)
     (remove as bs)]))

(defn edge-char' [tree edge off]
  (let [[k v] (first (:wm edge))]
    (-> tree :words (nth k) (nth (+ v off)))))

(defn edge-first-char [tree edge]
  (edge-char' tree edge 0))

(defn copy-tree [dest-tree dest-root, source-tree source-edge]
  {:pre [(contains? (:nodes dest-tree) dest-root)
         (contains? (:nodes source-tree) (:dni source-edge))
         (contains? (:nodes source-tree) (:sni source-edge))]}
  (let [node->chars (group-by* first second (-> source-tree :edges keys))
        wmap (:wmap source-tree)]
    (loop [dest-tree dest-tree
           xs        [[dest-root source-edge]]]
     (if-let [[dest-root {:keys [fci lci dni wm len]}] (first xs)]
       (let [idx (-> dest-tree :nodes count)
             chr (edge-first-char source-tree {:wm wm})
             new-edge {:dni idx :sni dest-root
                       :len len :wm (zipmap (map wmap (keys wm)) (vals wm))
                       :_ :cp}]
         (recur
          (-> dest-tree
              (update-in [:nodes] conj {:_idx idx})
              (assoc-in  [:edges [dest-root chr]] new-edge))
          (concat (for [c (node->chars dni)
                        :let [ed (-> source-tree :edges (get [dni c]))]]
                    [idx ed]) (rest xs))))
       dest-tree))))

;(msuffixtree "x" "xa")

;; rev1
(defn edge-split [tree edge idx-rel e1<-wm]
  ;;(println idx-rel)
'  {:pre [(contains? (:nodes tree) (:dni edge))
         (contains? (:nodes tree) (:sni edge))
         (< 0 idx-rel (:len edge))]
   :post [(contains? (:nodes (first %))   (:dni (second %)))
          (contains? (:nodes (first %))   (:sni (second %)))]}
  (let [i (-> tree :nodes count)
        c1 (edge-first-char tree edge)
        c2 (edge-char' tree edge idx-rel)
        e1 {:sni (:sni edge), :dni i,
            :len idx-rel
            :wm (into (:wm edge) e1<-wm)}
        e2 {:sni i, :dni (:dni edge),
            :len (- (:len edge) idx-rel 1)
            :wm (zipmap (keys (:wm edge))
                        (map (partial + idx-rel) (vals (:wm edge))))}]
    [(-> tree
         (update-in [:nodes] conj {})
         (assoc-in [:edges [(:sni e1) c1]] e1)
         (assoc-in [:edges [(:sni e2) c2]] e2))
     e2]))

(defn- tree-edge-wm [tree edge m]
  (update-in tree [:edges [(:sni edge) (edge-first-char tree edge)] :wm] into m ))

(defn edges-substr-length [tree1 edge1 tree2 edge2]
  (let [[w1 i1] (first (:wm edge1)) w1 (-> tree1 :words (nth w1))
        [w2 i2] (first (:wm edge2)) w2 (-> tree2 :words (nth w2))]
    (loop [i 0]
      (if (and (< i (:len edge1)) (< i (:len edge2))
               (= (nth w1 (+ i1 i)) (nth w2 (+ i2 i))))
        (recur (inc i))
        i))))

;; G*E*G*E -> G*V*G*V, lefele haladunk a fan.
(defn handle-ab [tree1 edge1 tree2 edge2]
  {:pre [(= (edge-first-char tree1 edge1)
            (edge-first-char tree2 edge2))]
   :post [(let [[t1 n1, t2 n2] %]
            (contains? (:nodes t1) n1))]}
  ;;(printf "(handle-ab %s %s)\n" edge1 edge2)
  (assert (every? map? [tree1 edge1 tree2 edge2]))
  (let [len (+ (edges-substr-length tree1 edge1 tree2 edge2))
        wm-right (for [[k v] (:wm edge2)] [((:wmap tree2) k) v])]
    (assert (not (neg? len)) "nincs kozos prefix!")
    (cond
     (= (:len edge1) (:len edge2) len)   (let [tree1 (tree-edge-wm tree1 edge1 wm-right)]
                                           [tree1 (:dni edge1) tree2 (:dni edge2)]) ;; ok

     (= (:len edge1) len)       (let [[tree2 e2] (edge-split tree2 edge2 (+ len)  nil)
                                      tree1      (tree-edge-wm tree1 edge1  wm-right)]
                                  [tree1 (:dni edge1) tree2 (:sni e2)]) ;; ezt piszkaltam dni+sni

     (= (:len edge2) len)       (let [[tree1 e1] (edge-split tree1 edge1 (+ len) wm-right)]
                                  [tree1 (:sni e1) tree2 (:dni edge2)])

     :else             (let [[tree1 e1] (edge-split tree1 edge1 len wm-right)
                             [tree2 e2] (edge-split tree2 edge2 len nil)]
                         [tree1 (:sni e1) tree2 (:sni e2)])))) ;; XXX dni dni volt.

;;(msuffixtree "x" "xa")
;;(msuffixtree "xa" "x")


;;(msuffixtree "xa" "xa")


;;(msuffixtree "x" "ax")
;;(msuffixtree "ax" "x")

;;(msuffixtree "latjatuk feleim szumtukhel mik vogymuk" "isa pur es homou vogymuk")


;(est/->suffixtree "x")


;; Recursive algo, may throw stackoverflow exception
;; G*V*G*V -> G
(defn merge-trees [tree1 root1 tree2 root2]
  (let []
    ;;(printf  "(merge-trees %s %s)\n" root1 root2)
    ;; assert -- legyen kozos prefixuk a root-tol kezdve!!!
    (let []
      (let [[_ ab-keys b-keys]
            ,,,(part ;; XXX this is slow, precache maybe?
                (for [[[a b] v] (:edges tree1) :when (= a root1)] b)
                (for [[[a b] v] (:edges tree2) :when (= a root2)] b)
                )
            tree2-edges (for [b b-keys] (-> tree2 :edges (get [root2 b])))]
        (-> (as-> tree1 tree
                  (reduce (fn [tree edge] (copy-tree tree root1 tree2 edge)) tree tree2-edges)
                  (reduce (fn [tree k]
                            (let [[t1 n1 t2 n2] (handle-ab tree  (-> tree  :edges (get [root1 k]))
                                                           tree2 (-> tree2 :edges (get [root2 k])))]
                              (merge-trees t1 n1 t2 n2)))
                          tree ab-keys)))))))

;(msuffixtree "x" "xa")

(defrecord MSuffixTree [words nodes edges]
  ;clojure.lang.Counted
  ;(count [t] (-> t .words count))
  ;clojure.lang.Seqable
  ;(seq [t] (-> t .words seq))
  ;clojure.lang.IPersistentCollection
  ;(empty [t] (->MSuffixTree [] [] {}))
  ;(cons [t x] (join t x))
  ;(equiv [t x] (= t x))
  )

;(instance? clojure.lang.IPersistentCollection (->suffixtree "asd"))
;(count (->suffixtree "asd" "asd"))
;(count (->MSuffixTree [] 1 1))

(defmulti prepare type)
(defmethod prepare String [t]
  (prepare (est/->suffixtree t)))
(defmethod prepare MSuffixTree [t] t)

(defmethod prepare clojure.lang.APersistentMap [t]
  (->MSuffixTree
   [(:str t)] (:nodes t)
   (into {} (for [[k m] (:edges t)]
              [k
               {:len (inc (- (:lci m) (:fci m)))
                :wm {0 (:fci m)}
                :sni (:sni m) :dni (:dni m)}]))))

(defmethod prepare :default [t]
  (throw (IllegalArgumentException.
          (format "Unexpected type: %s" (type t)))))

(defn join [t1 t2]
  (let [t1 (prepare t1)
        idxmap1 (vec (range (count (:words t1))))

        t2 (prepare t2)
        idxmap2  (mapv (partial + (count (:words t1))) (range (count (:words t2))))
        t2       (assoc t2 :wmap idxmap2)]
    ;;(println (:wmap t2))
    (-> (assoc t1 :words (vec (concat (:words t1) (:words t2))))
        (merge-trees 0 t2 0))))

(defn ->suffixtree [& xs]
  (reduce join (map prepare xs)))

;; (->suffixtree "alma" "korte" "ablak" "zsiraf")

(defn- subseq? [seq1 off1, seq2 off2, len]
  (loop [i 0]
    (if (< i len)
      (if (= (nth seq1 (+ off1 i)) (nth seq2 (+ off2 i)))
        (recur (inc i))
        false)
      true)))

(defn indices-of [tree s]
  (let [|s| (count s)
        ws (:words tree)]
    (loop [node 0, i 0, ln nil, edge nil]
      (if (< i |s|)
        (when-some [edge (get (:edges tree) [node (nth s i)])]
          (let [ln (min (:len edge) (- |s| i))
                [wk wi] (first (:wm edge))] ;; +1 to len maybe?
            (when (subseq? (vec s) i (ws wk) wi ln)
              (recur (:dni edge) (+ i (:len edge)) ln edge))))
        (for [[k v] (:wm edge)]
          [(ws k) (+ v (- |s|) ln)])))))

(extend-type MSuffixTree
  c/IOnline
  (put-word [t w] (join t w))
  c/IFindWord
  (find-word [t w] (ffirst (indices-of t w))))
