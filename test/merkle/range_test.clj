(ns merkle.range-test
  (:use clojure.test
        [merkle.range :only [disjoint? subset? superset?]]))

(deftest disjoint-test
  (are [a b] (disjoint? a b)
       [0 0] [1 1]
       [1 1] [0 0]
       [1 2] [3 4]
       [3 4] [1 2])

  (are [a b] (not (disjoint? a b))
       [0 0] [0 0]
       [0 0] [0 1]
       [0 1] [0 0]
       [1 2] [1 3]
       [1 3] [1 2]
       [1 1] [0 2]
       [0 2] [1 1]))

(deftest subset-test
  (are [a b] (subset? a b)
       [1 1] [1 1]
       [1 1] [1 2]
       [1 1] [0 1]
       [1 1] [0 2]
       [1 2] [1 2]
       [1 2] [0 2]
       [1 2] [1 3]
       [1 2] [0 3])

  (are [a b] (not (subset? a b))
       [1 2] [5 6]
       [1 2] [2 3]
       [1 4] [2 6]))
