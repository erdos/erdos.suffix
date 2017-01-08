(ns erdos.suffix.array
  "Suffix array implementation.")


(defn invert-permutations-vec [v]
  (apply assoc (vec v) (interleave v (range))))

(defn- cmp [a b]
  (if (= (:0 a) (:0 b))
    (compare (:1 a) (:1 b))
    (compare (:0 a) (:0 b))))

(def sort-l
  ;; TODO: can be replaced by a radix sort (2 x counting sort) for better runtime complexity
  (partial sort cmp))

(defn- powers-of-two-until [max]
  (take-while (partial > (* 2 max)) (iterate (partial * 2) 1)))

(defn- make-l [P cnt]
  (->
   (for [i (range (count P))]
     {:0 (P i)
      :1 (nth P (+ i cnt) -1)
      :p i})
   (sort-l)
   (vec)))

(defn suffix-sort-permutation [A]
  (let [|A|     (count A)
        range-n (range |A|)
        zeros   (vec (repeat |A| 0))]
    (invert-permutations-vec
     (reduce
      (fn [P cnt]
        (let [L (make-l P cnt)]
          (reduce
           (fn [P i]
             (assoc P
                    (:p (L i))
                    (if (zero? (cmp (L i) (L (dec i))))
                      (P (:p (L (dec i))))
                      i)))
           zeros
           (next range-n))))
      (mapv int A)
      (powers-of-two-until |A|)))))

;; TODO: should be also able to create an lcp array in here.

;; (suffix-sort-permutation "alabama")

;; vector of pairs: [[String, offset-int]]
(defn suffix-pairs [s]
  (for [[order offset] (map vector (range) (suffix-sort-permutation s))]
    {:string s, :offset offset, :order order}))


(defn print-suffix-array [s]
  (doseq [i (suffix-sort-permutation s)]
    (println (.substring s i) )))


;; TODO: maybe throw illegal argument exception if one seq is not sorted.
(defn- lazy-merge
  "Creates a lazily merged sorted seq of two sorted seq's. T=O(|M|+|N|)"
  ([as bs] (lazy-merge compare as bs))
  ([compare as bs]
   (lazy-seq
    (cond
      (empty? as) bs
      (empty? bs) as
      ;; here we could compare head elems with an accumulator value to
      (neg? (compare (first as) (first bs)))
      (cons (first as) (lazy-merge (next as) bs))
      :else
      (cons (first bs) (lazy-merge as (next bs)))))))

(defn assoc-suffix-array [suffix-table key value])

(defn find-suffix-table [suffix-table key]
  ;; returns a value of [key value] tuple when a key is assoced in a suffix table object.

  )

(deftype SuffixArray [order comparator imeta]
  clojure.lang.IObj
  (meta [_]
    imeta)
  (withMeta [_ new-imeta]
    (SuffixArray. order comparator new-imeta))

  clojure.lang.Seqable
  (seq [_]
    (for [x order] (nth (:string x) (:order x))))

  clojure.lang.Counted
  (count [_]
    (count order))
  ;; TODO: toString

  ;; TODO: pretty print
  ;; TODO: may retrieve and assoc meta value
  ;; TODO: should be able to conj and disj strings. (maybe key-value based?)
  ;; TODO: sorted interface? would be cool to have.
  )

(defn suffix-array? [x]
  (instance? SuffixArray x))

(defn suffix-array [xs & {:keys [comparator]}]
  (->SuffixArray []
                 ;; fill with first item
                 (vec (for [x (suffix-sort-permutation xs)]
                        {:string xs :order x}))
                 ;; default comparator:
                 (or comparator compare)
                 ;; empty meta map:
                 {}
                 ))

(defn find-first-substring-index [suffixarray subs]
  {:pre  [(suffix-array? suffixarray) (seq subs)]
   :post [(or (nil? %) (integer? %))]}
  ;; finds first occurence idx (int) of substring in  order list.
  nil)

(defn find-last-substring-index [suffix-array subs]
  ;; like find-first-substring-idx but for last occurence.
  nil)

(defn idx->location [table idx]
  (assert (suffix-array? table))
  (assert (integer? idx))
  ;; shall return a {:string String :index Integer} map
  ;;
  )

(defn find-substrings
  "Retrieves a seq of locations where a substr can be found in the table"
  [table subs]
  (when-let [idx (find-first-substring-idx suffix-table subs)]
    (let [idx' (find-last-substring-idx suffix-table subs)]
      (->> idx (drop idx) (take (inc (- idx' idx)))
           (map (partial idx->location table))))))

:ok
