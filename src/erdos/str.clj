(ns erdos.str
  "Some seq operations")

;; naive and slow
(defn lazy-prefixes [xs]
  (take-while some? (iterate butlast (seq xs))))

(defn prefixes [xs]
  (rest (reductions conj [] xs)))

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
  (mapcat suffixes (prefixes xs)))

(comment
  ;; in a functional style
  (mapcat suffixes (prefixes xs))
  ;; is faster than
  (mapcat prefixes (suffixes xs))

  ;; see:

  (->> (range 1000)
       (prefixes) (mapcat suffixes)
       (map doall) (doall)
       (time) (count));; 4250 msecs

  (->> (range 1000)
       (suffixes) (mapcat prefixes)
       (map doall) (doall)
       (time) (count)) ;; 5071 msecs

  comment)
