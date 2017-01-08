(ns erdos.suffix-array-test
  (:require [clojure.test :refer [deftest is are testing]]
            [erdos.suffix.array :refer :all]))



(deftest suffix-sort-permutation-test

  (testing "trivial cases"
    (is (= [] (suffix-sort-permutation [])))
    (is (= [] (suffix-sort-permutation nil)))
    (is (= [0] (suffix-sort-permutation "a")))
    (is (= [0 1 2 3] (suffix-sort-permutation "abcd"))))

  (testing "some examples"
    (are [input output] (= output (suffix-sort-permutation input))
      "abcdef" [0 1 2 3 4 5]
      "abacus" [0 2 1 3 5 4])))


(deftest invert-permutation-vec-test
  (testing "Identity on these calls"
    (are [x] (= (vec x) (invert-permutations-vec x))
      nil [] [0] [0 1] [0 1 2] [1 0 2] [2 1 0] [0 1 2 3 4 5 6 7]))
  (testing "Self inverse (ident on second eval)"
    (are [x] (= (vec x) (invert-permutations-vec (invert-permutations-vec x)))
      [5 4 3 2 1 0]
      [4 3 2 1 0 5]
      [3 4 2 1 5 0])))
