(ns merkle.kv.linear-test
  (:require [clojure.test :refer :all]
            [merkle.kv.linear :refer :all]
            clojure.pprint
            [clojure.data.generators :as gen]))

(deftest empty-test
  (is (= nil (tree nil)))
  (is (= nil (tree {}))))

(deftest single-test
  (is (= (tree {:foo 1})
         (->Node 1 :foo :foo nil nil)))
  (is (= (tree {:foo "hi there"})
         (->Node 588714629 :foo :foo nil nil))))

(deftest two-test
  (is (= (tree (sorted-map :foo 1 :bar 2))
         (->Node 1024 :bar :foo
                 (->Node 2 :bar :bar nil nil)
                 (->Node 1 :foo :foo nil nil)))))

(deftest three-test
  (is (= (tree (sorted-map :foo 1 :bar 2 :baz 3))
         (->Node 32768 :bar :foo
                 (->Node 1026 :bar :baz
                         (->Node 2 :bar :bar nil nil)
                         (->Node 3 :baz :baz nil nil))
                 (->Node 1 :foo :foo nil nil)))))

(deftest five-test
  (is (= (tree (sorted-map :foo 1 :bar 2 :baz 3 :xyzzy 4 :frob 5))
         (->Node 1047649 :bar :xyzzy
                 (->Node 33764 :bar :frob
                         (->Node 1026 :bar :baz
                                 (->Node 2 :bar :bar nil nil)
                                 (->Node 3 :baz :baz nil nil))
                         (->Node 997 :foo :frob
                                 (->Node 1 :foo :foo nil nil)
                                 (->Node 5 :frob :frob nil nil)))
                 (->Node 4 :xyzzy :xyzzy nil nil)))))

(defn random-map
  ([fk fv] (random-map fk fv gen/default-sizer))
  ([fk fv sizer]
   (into (sorted-map)
         (zipmap (gen/reps sizer fk)
                 (gen/reps sizer fv)))))

(deftest identity-test
  (dotimes [i 10]
    (let [m (random-map gen/string gen/long (gen/geometric 0.001))]
      (let [t1 (tree m)
            t2 (tree m)]
        (= t1 t2)))))

(deftest different-test
  (->> (repeatedly #(random-map gen/int gen/int (gen/geometric 0.01)))
       (take 10)
       (map tree)
       (partition 2 1)
       (map (fn [[t1 t2]] (is (not= t1 t2))))
       dorun))

(deftest identical-range-test
  (testing "identical"
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 1 :b 2 :c 3 :d 4)))
           [[:a :d]])))

  (testing "one difference"
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 1 :b 0 :c 3 :d 4)))
           [[:a :a] [:c :d]])))

  (testing "one branch different"
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 0 :b 0 :c 3 :d 4)))
           [[:c :d]])))

  (testing "All values different."
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 2 :b 1 :c 4 :d 3)))
           [])))

  (testing "One different key at the end."
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 1 :b 2 :c 3 :e 4)))
           [[:a :b] [:c :c]])))

  (testing "One new key at the end."
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4))
                             (tree (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5)))
           [[:a :d]])))
  
  (testing "One new key at the beginning."
    (is (= (identical-ranges (tree (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5))
                             (tree (sorted-map      :b 2 :c 3 :d 4 :e 5))))
        [[:b :e]])))

(deftest randomized-resolve-test
  "Generate a couple maps, then bring them into sync by copying the different
  parts."
  (dotimes [i 1000]
    ; Build a random map
    (let [seed (random-map gen/byte gen/byte (gen/geometric 0.05))
          ; Perturb it into two variant copies
          m1 (merge seed (random-map gen/byte gen/byte (gen/geometric 0.1)))
          m2 (merge seed (random-map gen/byte gen/byte (gen/geometric 0.1)))
          ; Compute trees
          t1 (tree m1)
          t2 (tree m2)
          ; And delta
          identical (identical-ranges t1 t2)
          ; And diffs
          d1 (diff m1 t1 t2)
          d2 (diff m2 t2 t1)]
;      (prn :map1 m1)
;      (prn :map2 m2)
;      (prn :diff1 (doall d1))
;      (prn :diff2 (doall d2))

      ; Check that the updates force the maps to converge:
      (is (into m1 d2) (into m2 d1)))))
