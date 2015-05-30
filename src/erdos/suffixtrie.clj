(ns erdos.suffixtrie
  (:require [erdos.str :as s]))

(defn ->trie
  "Creates a suffix trie for a given list of words. T~S~O(sigma N_i^2)"
  [& words]
  (let [words (vec words)]
    {:map (reduce
           (fn [acc i]
             (reduce (fn [acc s] (update-in acc (conj (vec s) :>) conj i))
                     acc (s/suffixes (words i))))
           {} (range (count words)))
     :words words}))

(defn suffix-of [tree word]
  (map (:words tree)
       (:> (get-in (:map tree) (vec word)))))

(defn find-words [tree word]
  (map (:words tree)
       ((fn f [root]
          (when root
            (concat (:> root) (mapcat f (filter map? (vals root))))))
        (get-in (:map tree) (vec word)))))
