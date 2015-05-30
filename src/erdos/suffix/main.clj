(ns erdos.suffix.main
  ;(require [erdos.suffixtree :as stree])
  (require [erdos.suffixtrie :as strie]
           [erdos.gst :as gst])
  (require [clojure.java.io :as io]
           [clojure.core.reducers :as r]))

(defn main [file]
  (let [t1   (System/currentTimeMillis)
        trie (apply strie/->trie (line-seq (io/reader file)))
        t2   (System/currentTimeMillis)]
    (printf "read %d words in %d ms\n" (count (:words trie)) (- t2 t1)) (println)
    (doseq [line (line-seq (io/reader *in*))
            :when (not (empty? line))
            :while (not= line ":q")
            fnd  (strie/find-words trie line)]
      (println fnd))))

(comment


  (def tree (atom nil))

  (let [ls (line-seq (io/reader "/home/jano/wordlist-hu-0.3/list/freedict"))
        ls (take 10000 ls)
        ls (time (vec ls))
        ;; generalized ukkonen
        tr (time (apply gst/->suffixtree ls))
        ;; naive trie building
        tr2 (time (apply strie/->trie ls))

        ]
    (System/gc))




          folder (fn ([] (gst/->suffixtree "abakusz"))
                 ([a b] (gst/join a b)))

        ;tr2 (time (r/fold folder ls))



  (gst/indices-of @tree "vit")


  )
