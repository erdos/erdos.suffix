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

''(defn trie->tree [tree]
  {:map 12
   :words (:words tree)})

(defn suffix-of [tree word]
  (map (:words tree)
       (:> (get-in (:map tree) (vec word)))))

(defn find-words [tree word]
  (map (:words tree)
       (let [f (fn f [root]
                 (when root
                           (concat (:> root)
                                   (mapcat f (filter map? (vals root))))))]
         (f (get-in (:map tree) (vec word))))))


(comment

(suffix-of (->trie "abacus" "bobcat" "cus" "tomcat") "cat")

(find-words (->trie "abacus" "bobcat" "cus" "tomcat") "ca")


 (time (doall
        (for [ws (s/prefixes ["abacus" "babacus" "alala" "lololo" "trololo" "huhufds" "jano " "indexurl" "kupak" "majonez" "kutyafule" "egyeb szo" "mindenmas"])]
          (count (tree-seq coll? seq (apply ->trie ws)))
          )))

  (do
    (System/gc)
    (time (apply ->trie (line-seq (clojure.java.io/reader "/home/jano/wordlist-hu-0.3/list/freedict"))))
    :ok)


  )
