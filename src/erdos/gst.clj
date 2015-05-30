(ns erdos.gst
  (:require [erdos.suffixtree :as est]
            [erdos.suffix.core :as c]))

(declare edge-var handle-ab join copy-tree merge-edges)

(defn group-by* [keyfn valfn coll]
  (let [m (group-by keyfn coll)]
    (zipmap (keys m)
            (for [v (vals m)] (map valfn v)))))

(defn part [as bs]
  (let [as (set as) bs (set bs)]
    [(remove bs as)
     (filter bs as)
     (remove as bs)]))

(defn- edge-char' [tree edge i]
  (let [[k v] (first (:wm edge))]
    (-> tree :words (nth k) (nth (+ v i)))))


(defn copy-tree [dest-tree dest-root, source-tree source-edge]
  {:pre [(contains? (:nodes dest-tree) dest-root)
         (contains? (:nodes source-tree) (:dni source-edge))
         (contains? (:nodes source-tree) (:sni source-edge))]}
  (let [node->chars (into {} (map-indexed (fn [i x] [i (keys x)]) (:nodes source-tree)))

        wmap (:wmap source-tree)]
    (loop [dest-tree dest-tree
           xs        [[dest-root source-edge]]]
     (if-let [[dest-root {:keys [fci lci dni wm len]}] (first xs)]
       (let [idx (-> dest-tree :nodes count)
             chr (edge-char' source-tree {:wm wm} 0)
             new-edge {:dni idx :sni dest-root
                       :len len :wm (zipmap (map wmap (keys wm)) (vals wm))}]
         (recur
          (-> dest-tree
              (update-in [:nodes] conj {})
              (assoc-in  [:nodes dest-root chr] new-edge))
          (concat (for [c (node->chars dni)
                        :let [ed (-> source-tree :nodes (get dni) (get c))]]
                    [idx ed]) (rest xs))))
       dest-tree))))

(defn edge-split [tree edge idx-rel e1<-wm]
  ;;(println idx-rel)
 #_ {:pre [(contains? (:nodes tree) (:dni edge))
         (contains? (:nodes tree) (:sni edge))
         (< 0 idx-rel (:len edge))]
     :post [(contains? (:nodes (first %))   (:dni (second %)))
          (contains? (:nodes (first %))   (:sni (second %)))]}
  (let [i (-> tree :nodes count)
        c1 (edge-char' tree edge 0)
        c2 (edge-char' tree edge idx-rel)
        e1 {:sni (:sni edge), :dni i,
            :len idx-rel
            :wm (into (:wm edge) e1<-wm)}
        e2 {:sni i, :dni (:dni edge),
            :len (- (:len edge) idx-rel)
            :wm (zipmap (keys (:wm edge))
                        (map (partial + idx-rel) (vals (:wm edge))))}]
    [(-> tree
         (update-in [:nodes] conj {})
         (assoc-in [:nodes (:sni e1) c1] e1)
         (assoc-in [:nodes (:sni e2) c2] e2))
     e2]))

(defn- tree-edge-wm [tree edge m]
  (update-in tree [:nodes (:sni edge) (edge-char' tree edge 0) :wm] into m ))

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
  {:pre [(= (edge-char' tree1 edge1 0)
            (edge-char' tree2 edge2 0))]
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
                         [tree1 (:sni e1) tree2 (:sni e2)]))))


;; Recursive algo, may throw stackoverflow exception on very long texts
;; G*V*G*V -> G
(defn merge-trees [tree1 root1 tree2 root2]
  (let []
    ;;(printf  "(merge-trees %s %s)\n" root1 root2)
    ;; assert -- legyen kozos prefixuk a root-tol kezdve!!!
    (let []
      (let [[_ ab-keys b-keys]
            ,,,(part ;; XXX this is slow, precache maybe?
                (keys (get (:nodes tree1) root1))
                (keys (get (:nodes tree2) root2))
                ;(for [[[a b] v] (:edges tree1) :when (= a root1)] b)
                ;(for [[[a b] v] (:edges tree2) :when (= a root2)] b)
                )
            tree2-edges (map (-> tree2 :nodes (get root2)) b-keys)]
        (-> (as-> tree1 tree
                  (reduce (fn [tree edge] (copy-tree tree root1 tree2 edge)) tree tree2-edges)
                  (reduce (fn [tree k]
                            (let [[t1 n1 t2 n2] (handle-ab tree  (-> tree  :nodes (get root1) (get k))
                                                           tree2 (-> tree2 :nodes (get root2) (get k)))]
                              (merge-trees t1 n1 t2 n2)))
                          tree ab-keys)))))))

;(msuffixtree "x" "xa")

(defrecord MSuffixTree [words nodes]
  ;clojure.lang.Counted
  ;(count [t] (-> t .words count))
  ;clojure.lang.Seqable
  ;(seq [t] (-> t .words seq))
  ;clojure.lang.IPersistentCollection
  ;(empty [t] (->MSuffixTree [] [] {}))
  ;(cons [t x] (join t x))
  ;(equiv [t x] (= t x))
  )

(defmulti prepare type)
(defmethod prepare String [t]
  (prepare (est/->suffixtree t)))
(defmethod prepare MSuffixTree [t] t)

(defmethod prepare clojure.lang.APersistentMap [t]
  (let [edges (:edges t)
        n->c (group-by* first second (keys edges))
        prep-edge (fn [m]
                    {:len (inc (- (:lci m) (:fci m)))
                     :wm {0 (:fci m)}
                     :sni (:sni m) :dni (:dni m)})
        nodes (map-indexed (fn [i n]
                             (let [cs (n->c i)]
                               (zipmap cs (for [c cs] (prep-edge (get edges [i c]))))))
                           (:nodes t))]
    (->MSuffixTree [(:str t)] (vec nodes))))

(defmethod prepare :default [t]
  (throw (IllegalArgumentException.
          (format "Unexpected type: %s" (type t)))))

;; TODO: check for duplicate words.

(defn join [t1 t2]
  (let [t1 (prepare t1)
        t2 (prepare t2)

        ;; larger tree should be on the left - so less nodes will be moved
        [t1 t2] (if (< (-> t1 :nodes count) (-> t2 :nodes count)) [t2 t1] [t1 t2])

        idxmap1 (vec (range (count (:words t1))))
        idxmap2  (mapv (partial + (count (:words t1))) (range (count (:words t2))))
        t2       (assoc t2 :wmap idxmap2)]
    ;;(println (:wmap t2))
    (-> (assoc t1 :words (vec (concat (:words t1) (:words t2))))
        (merge-trees 0 t2 0))))

(defn ->suffixtree [& xs]
  (reduce join (map prepare xs)))

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
        (when-some [edge (get-in tree [:nodes node (nth s i)])]
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

(def tree-empty (->MSuffixTree [] [{}]))


(comment


;; naive O(n2) algo taking O(n2) space
(defn add-str-naive [tree s]
  (let [wi (count (:words tree)), ws (conj (:words tree) s)
        |s| (count s)]
    (as-> nil *
          (reduce (fn [nodes start]
                    (loop [nodes nodes, node 0, end start]
                      (if (< end |s|)
                        (let [ch (nth s end)]
                          (if-let [m (get (nth nodes node) ch)]
                            (recur (update-in nodes [node ch :wm] assoc wi end)
                                   (get m :dni)
                                   (+ end (:len m)))
                            (let [idx(count nodes)
                                  nodes (conj nodes {})
                                  nodes (assoc-in nodes [node ch]
                                                  {:wm {wi end} :sni node, :dni idx, :len 1})
                                  ]
                              (recur nodes idx (inc end)))
                            ))
                        nodes)))
                  (:nodes tree) (range |s|))
          {:nodes * :words ws})))

(add-str-naive tree-empty "asd")

(indices-of
 (reduce add-str-naive tree-empty ["cuscus" "abacus babacus" "cudaus"])
 "dd")


  )
