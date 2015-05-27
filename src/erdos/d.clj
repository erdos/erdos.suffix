(ns erdos.d
  ;(:require [rhizome.viz :as r])
  )


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

;; https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf


(defn test-and-split [])

(defn canonize [s [k i]]
  (if (< p k)
    [s k]
    ;; find the tk-transition g'(s, k', p') = s' from s

    ))


(defn update [s [k i]]

  )
