(ns erdos.suffix.main
  ;(require [erdos.suffixtree :as stree])
  (require [erdos.suffixtrie :as strie])
  (require [clojure.java.io :as io]))

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
