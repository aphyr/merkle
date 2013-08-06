(ns merkle.range
  "Functions over inclusive ranges, which are just pairs of comparable objects
  like [1 2] or (:a :b)."
  (:refer-clojure :exclude [< <= contains?]))

; For concision, override comparison here to work with all comparables.
(defn- <= [a b] (clojure.core/< (compare a b) 1))
(defn- < [a b] (clojure.core/< (compare a b) 0))

(defn subset?
  "Is range1 a subset of range2?"
  [range1 range2]
  (and (<= (first range2)
           (first range1))
       (<= (second range1)
           (second range2))))

(defn superset?
  "Is range1 a superset of set2?"
  [r1 r2]
  (and (<= (first r1)
           (first r2))
       (<= (second r2)
           (second r1))))

(defn disjoint?
  "Are r1 and r2 totally disjoint ranges?"
  [r1 r2]
  (or
    ; [r1 r1]  [r2 r2]
    (< (second r1)
       (first r2))
    ; [r2 r2]  [r1 r1]
    (< (second r2)
       (first r1))))

(defn intersect?
  "Do r1 and r2 intersect at all?"
  [r1 r2]
  (not (disjoint? r1 r2)))

(defn contains?
  "Does the range contain the given value?"
  [range element]
  (and (<= (first range) element)
       (<= element (second range))))
