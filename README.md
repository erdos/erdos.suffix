# erdos.suffix

Efficient string search algorithms implemented in Clojure.

## Rationale

The task is to find a substring of length _M_ in a string of length _N_. The naive linear search algorithm has time complexity of _O(M+N)_. We want something better.

## Suffix Arrays

```clojure
(require 'erdos.suffix.array)
(def arr1 (suffix-array "Georgia on My Mind"))

(print (find-first-substring-index [arr1 "My"]))
```

## Resources

 - Suffix Tree: generalized implementation in java [1](https://github.com/abahgat/suffixtree) and [2](https://gist.github.com/bicepjai/3355993), C [3](https://github.com/Rerito/suffix-tree), ruby [4](https://gist.github.com/suchitpuri/9304856)
 - Suffix trees: nice introduction [5](http://www.cise.ufl.edu/~sahni/dsaaj/enrich/c16/suffix.htm) and [6](http://programmerspatch.blogspot.hu/2013/02/ukkonens-suffix-tree-algorithm.html)
 - Ukkonen: A great explanation on [stackoverflow](http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english)
 - [Ukkonen's article](https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf) and [7](http://web.stanford.edu/~mjkay/gusfield.pdf) 
 - Ukkonen: found a [bug in a python implementation](https://github.com/zhangliyong/generalized-suffix-tree/issues/1)

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
