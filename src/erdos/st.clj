(ns erdos.st
  (:require [erdos.str :as s]))

;;
;; in active development
;; do not compile
;;
;;; based on https://github.com/zhangliyong/generalized-suffix-tree
;;
;; found a bug in algorithm, see: https://github.com/zhangliyong/generalized-suffix-tree/issues/1


(declare edge-get-char canonize split-edge go-next get-char insert-node)

;; 7
(defn- ->node
  ([] (->node nil))
  ([index]
   {:index-set (if (some? index) #{index} #{})
    :edge-dict {}
    :suffix-link nil}))

;; 14
(defn- node-add-index [node idx]
  (assert (and (map? node) (integer? idx)))
  (update-in node [:index-set] conj idx))

;; 17
(defn get-indexs [n] (:index-set n))

;; 20
(defn node-add-edge [node, edge ts]
  (assert (map? node) (format "expected map, got " (type node)))
  (assert (map? edge) (format "expected map, got " (type edge)))
  (assert (ifn? ts))
  (let [char (edge-get-char edge ts)]
    (assoc-in node [:edge-dict char] edge)))

;; 24
(defn node-remove-edge [node char]
  (assert (map? node)) (assert (char? char))
  (update-in node [:edge-dict] dissoc char))

;; 27
(defn node-get-edge [node char]
  (assert (map? node)) (assert (char? char))
  (-> node :edge-dict (get char)))

;; 39
(defn ->edge [index start end target_node]
  (assert (and (integer? index) (integer? start) (integer? end) (integer? target_node)))
  {:index index
   :start start, :end end
   :target-node target_node})


;; 47
(defn- edge-length [edge] (assert (map? edge))
  (- (:end edge) (:start edge)))

;; 50 -- text store is an additional argument (fun of idx => str)
(defn- edge-get-char
  ([edge text-store]
   (edge-get-char edge 0 text-store))
  ([edge i, text-store]
   (assert (map? edge))
   (assert (integer? i))
   (assert (ifn? text-store))
   (nth (text-store (:index edge)) (+ (:start edge) i))))

;; 53
(defn edge-split [edge, i, new-target]
  (assert (and (map? edge) (integer? i) (integer? new-target)))
  (let [new-edge (->edge (:index edge) (:start edge) (+ (:start edge) i) new-target)
        edge (update-in edge [:start] + i)]
    [edge new-edge]))

;; https://github.com/zhangliyong/generalized-suffix-tree/blob/master/gst.py
;; 97
(def empty-gst
  {:remainder 0
   :active-length 0
   :active-edge nil
   :text-store []
   :nodes [(->node)]
   :root 0, :active-node 0
   })

;; 107
(defn reset-active [self]
  (assoc self
    :active-node (:root self)
    :active-length 0
    :active-edge nil
    :remainder 0))

(declare add-char)


(defn- insert-node-
  "Returns pair of [new_tree, new_node_idx"
  [self arg]
  (let [n (->node arg)
        i (count (:nodes self))]
    [(assoc-in self [:nodes i] n)
     i]))


(defn node-add-edge- [node, edge char]
  (let [;char (edge-get-char edge)
        ]
    (assoc-in node [:edge-dict char] edge)))


(defn insert-node-edge [tree, start-node, index, start, end, node-arg]
  (let [[tree target-node] (insert-node- tree node-arg)
        e (->edge index start end target-node)
        char    (get-char tree index     start) ;; XXX test this point.
        ;_ (println start-node)
        tree     (update-in tree [:nodes start-node] node-add-edge- e char)]
    tree))



;; 113
(defn- add-text [self text]
  (assert (and (map? self) (string? text)))
  (let [index (-> self :text-store count)]
    (as-> self self
          (reset-active self)
          (update-in self [:text-store] conj text)
          (assoc-in self [:text-len] (count text))
          (reduce (fn [a i] (add-char a i index))
                  self (range (:text-len self))))))

;; moved from 85
(defn- get-char [tree index i]
  (assert (map? tree))
  (assert (and (integer? index) (integer? i)))
  (-> tree :text-store (nth index) (nth i)))

(defn- get-node [self, idx]
  (assert (and (map? self) (integer? idx)))
  (get-in self [:nodes idx]))

(declare add-suffix-link get-suffix-link add-index)

;; 120
(defn add-char [self, start, index]
  (assert (map? self) self)
  (assert (integer? start) start)
  (assert (integer? index) index)
;  (assert (and (map? self) (integer? self) (integer? index)))
  ;; index is text index
  (let [self (update-in self [:remainder] inc)
        char (get-char self index start)
        over (= start (dec (:text-len self)))

        self (atom self)
        last-parent (atom nil)
        ]

    (try
      (while true
        (if-not (-> @self :remainder pos?)
          (throw (InterruptedException.))
         (do
           (swap! self canonize)

           (swap! self assoc :active-edge
                  (get-char @self index, (-> start (- (:remainder @self)) (+ 1))))
           (let [edge (-> (get-node @self (:active-node @self))
                          (node-get-edge (:active-edge @self)))]
             (if (nil? edge)
               (let [
                     self- (insert-node-edge @self (:active-node @self) index start (:text-len @self) index)
                     self- (add-suffix-link self- @last-parent (:active-node self-))]
                 (reset! self self-)
                 (reset! last-parent (:active-node self-)))
               (do          ;; XXX check next
                 ;(println (:active-length @self))
                 (when (=  (get-char @self (:index edge) (+ (:active-length @self) (:start edge)))
                            char)
                   (swap! self update-in [:active-length] inc)
                   (swap! self add-suffix-link @last-parent (:active-node @self))
                   (reset! last-parent (:active-node @self))
                   (when over
                     (swap! self add-index index))
                   (throw (InterruptedException.)))
                 (let [[self- internal-node ] (split-edge @self edge)]
                   (reset! self self-)
                   (swap! self add-suffix-link @last-parent internal-node)
                   (reset! last-parent internal-node)
                   ;(println :>>>)
                   ;(println @self)

                   (swap! self insert-node-edge internal-node index start (:text-len @self) index))))
             (swap! self update-in [:remainder] dec)
             (swap! self go-next)))))
      (catch InterruptedException e @self))))

(defn split-edge [self, edge]
  (assert (and (map? self) (map? edge)))

  (let [self (update-in self [:nodes (:active-node self)]
                        node-remove-edge (:active-edge self))
        [self internal-node] (insert-node- self nil)

        ;;; XXX ha az alabbi sorrendet megforditom, exceptiont dob.
        [edge new-edge] (edge-split edge (:active-length self) internal-node)
        self (update-in self [:nodes internal-node] node-add-edge edge (:text-store self))
        self (update-in self [:nodes (:active-node self)] node-add-edge new-edge (:text-store self))]
    [self internal-node]))

;; 181 okok
(defn- go-next [self]
  (assert (map? self))
  (if (= (:active-node self) (:root self))
    (if (pos? (:active-length self))
      (update-in self [:active-length] dec)
      self)
    (assoc self :active-node
           (or (get-suffix-link self (:active-node self))
               (:root self)))))

;; 190
(defn- add-index [self index]
  (assert (and (map? self) (integer? index)))
  (loop [self (assoc self :last-parent nil)]
    (if (pos? (:remainder self))
      (as-> self self
            (canonize self)
            (if  (or (nil? (:active-length self)) (zero? (:active-length self)))

              (update-in self [:nodes (:active-node self)]
                         node-add-index index)
              (let [;_ (println self)
                    edge (->  (node-get-edge (get-node self (:active-node self))
                                             (:active-edge self)))
                    _ (assert (some? edge) "Edge should not be none")
                    [self internal_node] (split-edge self edge)
                    self (update-in self [:nodes internal_node] node-add-index index)

                    self (add-suffix-link self (:last-parent self) internal_node)]
                (assoc self :last-parent internal_node)))

            (update-in self [:remainder] dec)
            (go-next self))
      self)))

;; 209: search

;; 241 ok
(defn add-suffix-link [self, last_parent current]
  (assert (map? self))
;  (assert (integer? last_parent))
  (assert (integer? current))
  (if (some? last_parent)
    (assoc-in self [:nodes last_parent :suffix-link] current)
    self))

(defn- get-suffix-link [self node]
  (assert (and (map? self) (integer? node)))
  (get-in self [:nodes node :suffix-link]))

;; 245 okok
(defn canonize [self]
  (or
   (when (-> self :active-length pos?)
     (let [edge (node-get-edge (get-node self (:active-node self))
                               (:active-edge self))]
       (if (<= (edge-length edge) (:active-length self))
         (assoc self
           :active-length (- (:active-length self) (edge-length edge))
           :active-node   (:target-node edge)))))
   self))

;; 241 ??
(defn add-sufix-link [self last_parent current]
  (if-not (nil? last_parent)
    (assoc self :suffix-link last_parent current)
    self))

(defn search [tree pattern]
  (loop [cur_node (:root tree)
         pattern pattern] ;; string!
    (if (seq pattern)
      (do
        ;(println (get-node tree cur_node))
        (if-let [edge (node-get-edge (get-node tree cur_node) (first pattern))]
         (let [label (-> (:text-store tree) (nth (:index edge))
                         (->> (drop (:start edge))))
               minlen (min (- (:end edge) (:start edge)) (count pattern))]
           ;(println edge)
           (when (= (take minlen label) (take minlen pattern))
             (recur    (:target-node edge)
                       (subs pattern minlen))))))
      (get-node tree cur_node))))

;(add-suffix-link {} 1 2)


;;
;; bad behavior
;; does not find str
;; --> error still persiss in original python implementatin
;; /!\

(comment

 (-> empty-gst
     (add-text "mambma")
     time
     (search "mambm")
     time)

 )
