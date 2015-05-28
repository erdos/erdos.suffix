(ns erdos.suffix.core)

(defprotocol IOnline
  "Words can be added on the fly"
  (put-word [_ xs]))

(defprotocol IFindWord
  "Find word by a given substring"
  (find-word [_ subs] "Returns word containing substring or nil"))

(defprotocol IFindWordIdx
  (find-word-idx [_ subs] "Returns [word index] pair or nil"))

(defprotocol IFindWords
  "Find all words containing a given substring"
  (find-words [_ subs]))
