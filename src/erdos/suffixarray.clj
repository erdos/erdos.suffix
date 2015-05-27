(ns erdos.suffixarray)

(defn prefixes [xs]
  (->> xs
       (iterate next)
       (take-while some?)
       (map vec)))

;; (prefixes "mississippi")

(defn ->suffixarray [s]
  )

(defn cc [as bs]
  (cond
   (empty? as) -1
   (empty? bs) +1
   ;(= +1 (compare (first as) (first bs))) -1
   (= (first as) (first bs)) (recur (next as) (next bs))
   :else (compare (first as) (first bs))))

(defn cc! [as bs]
  (cond
   (empty? as) 0
   (empty? bs) 0
   (= (first as) (first bs)) (recur (next as) (next bs))
   :else (compare (first as) (first bs))))

(defn srot [xs]
  (hash-map
   :s xs
   :v (mapv first (sort-by second cc (zipmap (range) (prefixes xs))))))

(comment

 (defn score [a b]
   (loop [a a, b b, p 0]
     (if (and (seq a) (seq b) (= (first a) (first b)))
       (recur (next a) (next b) (inc p))
       p)))

 ;; (score "aaaf" "aaacd")

 )

(defn starts-with? [sub full]
  (if (seq sub)
    (when (= (first sub) (first full))
      (recur (next sub) (next full)))
    true))
;; (starts-with? "asdf" "asdffd")
;; (starts-with? "assdf" "asdffd")

;; return idx of substring
(defn find-subs [sr s]
  (loop [a 0
         z (-> sr :v count dec)]
    (cond (= a z)
          (when (starts-with? (-> (:v sr) (get a) (drop (:s sr))) s) a)
          (= (inc a) z)
          (cond (starts-with? (-> (:v sr) (get a) (drop (:s sr))) s) a
                (starts-with? (-> (:v sr) (get z) (drop (:s sr))) s) z)
          :else
      (let [x (int (/ (+ a z) 2))
            x' (-> sr :v (get x))
            xi (cc! (drop x' (:s sr)) s)]
        (cond
         (pos? xi) (recur a x)
         (neg? xi) (recur x z)
         :else     x')))))

;; (find-subs (srot "egy napon, mikor micimackonak semmi dolga nem akadt") "micimacko")

(int (/ 1 2))




;;;; UKKONEN algorithm
;; most csak ON2
(defn step [tree i]
  (let [ln (:last-node tree 0)
        c (nth (:str tree) i)
        path (if (= ln 0)
               (list 0)
               (loop [i ln, a (list ln)]
                 (let [ni (get (:links tree) i 0)]
                   (if (= ni 0)
                     (cons ni a)
                     (recur ni (cons ni a))))))
        tree' (assoc tree :last-node nil)]
    (reduce (fn [a x]
              (let [j (inc (:cnt a))]
                (-> a
                    (assoc :last-node j
                           :cnt j)
                    (assoc-in [:edges [x c]] j)
                    (assoc-in [:links j] (:last-node a 0))))
              ) tree' path)))

(defn ->strie [s]
  (reduce step
          {:str (vec s)
           :last-node 0
           :cnt 0
           :nodes {0 {}} ;; node-idx => [begin end]
           :edges {}, ;; [node-idx, char] => node-idx
           :links {}, ;; node-idx => node-idx
           :root 0}   ;; node-idx
          (range (count s))))

 (->strie "cacao")
