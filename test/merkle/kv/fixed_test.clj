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
  [merge-fn depth [v1 v2]]
  (let [t1 (tree depth v1)
        t2 (tree depth v2)
;        _ (prn :t1 t1)
;        _ (prn :t2 t2)
        diffs (set (diffs t1 t2))]
;    (prn :diff-fraction (try (float (/ (count diffs) (count v1)))
;                             (catch ArithmeticException e :nan)))
    [(repair-diffs merge-fn diffs v1 v2)
     (repair-diffs merge-fn diffs v2 v1)]))

(defn fixed
  "Calls (f arg), then feeds the return value into f again, until a fixed point
  is found. (f (f (f arg))) and so on."
  [f arg]
  (let [value (f arg)]
    (if (= value arg)
      value
      (recur f value))))

(prefer-method clojure.core/print-method
               clojure.lang.ISeq
               clojure.lang.IPersistentVector)
(prefer-method clojure.pprint/simple-dispatch
               clojure.lang.ISeq
               clojure.lang.IPersistentVector)

(def depth-and-segments
  (gen/bind (gen/choose -1 5)
            (fn [power]
              (if (= -1 power)
                ; Special case: empty vectors
                (gen/return [1 []])

                (gen/bind (gen/choose 0 3)
                          (fn [card]
                            (let [element (gen/choose (- card) card)
                                  ; Construct a vector 2x as big as we need;
                                  ; we'll split it in half to be the two
                                  ; divergent copies.
                                  size    (* 2 (int (Math/pow 2 power)))]
                              (gen/tuple (gen/choose 1 (inc power))
                                         (gen/vector element size)))))))))

(defn segments
  "Takes a vector and returns two segment vectors from it."
  [v]
  (let [n (/ (count v) 2)]
    (assert (integer? n)) 
    (assert (or (zero? n)
                (power-of-two? n)))
    [(subvec v 0 n)
     (subvec v n)]))

(defspec randomized-resolve-test
  ; Generates a couple sequences, then brings them into sync by copying the
  ; differing parts.
  100000
  (prop/for-all [ds depth-and-segments]
                (let [[d v]    ds           ; Split apart depth and big vector
                      versions (segments v) ; Split vector into 2
                      d        (max d 1)]   ; simple-check will shrink to 0. :(
                  ;(prn :d d)
                  ;(prn :v1 (first versions))
                  ;(prn :v2 (second versions))
                  (assert (apply = (map count versions)))
                  (let [[r1 r2] (fixed (partial repair max d) versions)]
;                    (prn "repaired" r1 r2)
;                    (prn "succcess?" (= r1 r2))
                    (or
                      ; Either the repaired pieces are actually equal....
                      (= r1 r2)
                      ; Or there was a hash collision and their trees are now
                      ; equal
                      (= (:hash (tree d r1))
                         (:hash (tree d r2))))))))
