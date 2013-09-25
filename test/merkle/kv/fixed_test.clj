(ns merkle.kv.fixed-test
  (:require [clojure.test :refer :all]
            [merkle.kv.fixed :refer :all]
            [clojure.pprint :refer :all]
            [simple-check.clojure-test :refer [defspec]]
            [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]))

(defn power-of-two?
  "Is x a positive integer power of 2?"
  [x]
  (if-not (pos? x)
    false
    (loop [x x]
      (if (= 1 x)
        true
        (let [x (/ x 2)]
          (if-not (integer? x)
            false
            (recur x)))))))

(extend-protocol gen/Shrink
  nil
  (shrink [_] nil))

(deftest power-of-two-test
  (is (power-of-two? 1))
  (is (power-of-two? 2))
  (is (power-of-two? 4))
  (is (not (power-of-two? 0)))
  (is (not (power-of-two? 3))))

(def segments (gen/bind (gen/such-that power-of-two? gen/int)
                        (fn [size]
                          (gen/tuple
                            (apply gen/tuple (repeat size gen/int))
                            (apply gen/tuple (repeat size gen/int))))))

(defn repair-diffs
  "Takes a merge function, a set of diffs, and two segments, v1 and v2. Returns
  v1 with updates from v2."
  [merge-fn diffs v1 v2]
  (map-indexed (fn [i segment]
                 (if (diffs i)
                   (merge-fn segment (nth v2 i))
                   segment))
               v1))

(defn repair
  "Takes two segments and repairs differences using their merkle trees. Merges
  conflicting elements with (merge-fn). Returns a vector like [(repaired v1)
  (repaired v2)]."
  [merge-fn depth v1 v2]
  (let [t1 (tree depth v1)
        t2 (tree depth v2)
        _  (pprint v1)
        _  (pprint t1)
        diffs (set (diffs t1 t2))]
    [(repair-diffs merge-fn diffs v1 v2)
     (repair-diffs merge-fn diffs v2 v1)]))

(prefer-method clojure.core/print-method clojure.lang.ISeq clojure.lang.IPersistentVector)
(prefer-method clojure.pprint/simple-dispatch clojure.lang.ISeq clojure.lang.IPersistentVector)

(defspec randomized-resolve-test
  ; Generates a couple sequences, then brings them into sync by copying the
  ; differing parts.
  100
  (prop/for-all [[v1 v2] segments]
                (do
                  (prn :v1 v1)
                  (prn :v2 v2)
                  (let [depth (max 0 (inc (int (log2 (count v1)))))]
                    (prn :depth depth)
                    (prn (repair max depth v1 v2)))
                  true)))
