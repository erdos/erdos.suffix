# erdos.suffixtree

## Status

**done**
 - Suffix trie, naive suffix tree, suffix array algorithms implemented
 - Ukkonen's algorithm implemented and generalized

**in progress** 
 - test, measure and optimize

**ideas**
 - parallelized Ukkonen's algorithm in map+reduce style
 - introduce protocols for common string operations (see `erdos.suffix.core`) and Clojure interop
 
## Usage

1. install a Java environment and [Leiningen](https://leiningen.org/)

2. clone source code to your computer using git: `git clone https://github.com/erdos/erdos.suffix`

3. run the demo application `lein run /path/to/word/list.txt`

It reads a newline separated list of words from a file and then repeatedly prompts the user for substring search. Can also be used in an unix manner: `echo "resz" | lein run /path/to/word/list.txt`

## Example

```clojure
(require '[erdos.gst :as gst])

(def tree (atom nil))

(let [ls (line-seq (io/reader "/home/jano/wordlist-hu-0.3/list/freedict"))
      ls (take 1000 ls)
      tr (time (apply gst/->suffixtree ls))]   
  (reset! tree tr)
  :well)
;; => well


(gst/indices-of @tree "vit")
;; => (["additivit�s" 6] ["aktivit�s" 4])

```

## API - packages

### erdos.gst

Generalized suffix tree implementation. See: `erdos.suffixtree`

`(->suffixtree "abc" "def" ...)` Constructs a suffix tree object.

`(indices-of t "word")` Returns a seq of `[word i]` pairs where the given substring occurs in *word* on index *i*.

`(join t1 t2)` Merges two suffix trees.


### erdos.suffixtrie

This package contains a suffix trie agorithm. Construction is O(N^2) time, space is O(N^2).

`(->trie "word1" "word2" ...)` Create a suffix trie object.

`(suffix-of trie "abc")` Returns a lazy list of words having the suffix "abc"

`(find-words trie "abc")` Returns a lazy list of words that contain "abc".

### erdos.suffixtree

Suffix tree algorithm. Construction is based on Ukkonen's algorithm, thus providing a very fast O(N) construction time and requires O(N) space.

`(->tree "long text")` Creates a suffix tree object for a single long text. 

`(index-of triee "substring")` Returns the integer index of a substring in a tree, or `nil` when not found.

### erdos.suffixarray

Suffix array algorithm.

`(->suffixarray "long text")` Constructs a suffix array.

`(index-of suffixarray "subs")` Returns integer index of substring in suffix array or `nil` when not found.

### erdos.str

String and lazy sequence manipulation.

`(prefixes xs)` Creates a sequence of all prefixes of xs.

`(suffixes xs)` Creates a seq of all sufixes.

`(subseqs xs)` Creates seq of all subsequences. That is a list of all prefixes of all suffixes.

## Measurements

### Tree and trie construction

The following example build a suffix tree and trie for a long text.

```clojure
(let [s
"Requiem aeternam dona eis, Domine,
et lux perpetua luceat eis.
Te decet hymnus, Deus, in Sion,
et tibi reddetur votum in Jerusalem.
Exaudi orationem meam,
ad te omnis care veniet.
Requiem aeternam dona eis, Domine,
et lux perpetua luceat eis."]
    (time (doto s gst/->suffixtree))
    (time (doto s strie/->trie)))
```

tree construction (ms) | trie construction (ms)
---------------------- | ----------------------
8msec                  | 30msec


The next code snippet reads _n_ small words from a dictionary and measure tree and trie construction times. It turns out that naive trie construction is more effective for a big number of small words.

```clojure
(defn measure [n]
  (let [ls (line-seq (io/reader "/tmp/wordlist-hu-0.3/list/freedict"))
        ls (vec (take n ls))]
    (System/gc)
    (time (apply gst/->suffixtree ls))
    (System/gc)
    (time (apply strie/->trie ls))
    nil))
```

n   | tree construction (ms) | trie construction (ms)
--- | ---------------------- | ----------------------
100 | 115ms | 7ms 
1000 | 1760ms | 70ms 
2000 | 3640ms | 150ms 
3000 | 6700ms | 240ms
4000 | 9140ms | 290ms 
5000 | 12200ms | 400ms 
6000 | 14100ms | 445ms 
7000 | 16778ms | 544ms 


## Resources

 - Generalized Suffix Tree implementation in java [1](https://github.com/abahgat/suffixtree) and [2](https://gist.github.com/bicepjai/3355993), C [3](https://github.com/Rerito/suffix-tree), ruby [4](https://gist.github.com/suchitpuri/9304856)
 - Nice introduction to ST [5](http://www.cise.ufl.edu/~sahni/dsaaj/enrich/c16/suffix.htm) and [6](http://programmerspatch.blogspot.hu/2013/02/ukkonens-suffix-tree-algorithm.html)
 - A great explanation on [stackoverflow](http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english)
 - 
 - [Ukkonen's article](https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf) and [7](http://web.stanford.edu/~mjkay/gusfield.pdf)
 
- found a [bug in a python implementation](https://github.com/zhangliyong/generalized-suffix-tree/issues/1)

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
