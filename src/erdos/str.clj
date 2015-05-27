(ns erdos.str
  "Some seq operations")

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
