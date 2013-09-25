(ns merkle.kv.fixed
  "This merkle tree implementation operates over a fixed-size sequence of
  32-bit integers, presumably the hashes of some other data structure. All
  participants must agree on the sequence size in advance. Trees are of fixed
  size, and identify the indices (starting with 0) of differing or identical
  segments."
  (:require
    [merkle.range :as range]
    [primitive-math :as p])
  (:import
    [java.util.zip
     CRC32]
    [java.util
     ArrayList]))

;;;

(defrecord Node
  [^long hash
   ^long min-segment
   ^long max-segment
   left
   right])

(defn node->map
  "Converts a Node to a standard Clojure map; useful for serialization."
  [^Node node]
  (when node
    {:hash    (.hash node)
     :min-segment (.min-segment node)
     :max-segment (.max-segment node)
     :left    (node->map (.left node))
     :right   (node->map (.right node))}))

(defn map->node
  "Converts a map to a Node; useful for serialization."
  [m]
  (when m
    (Node.
      (:hash m)
      (:min-segment m)
      (:max-segment m)
      (map->node (:left m))
      (map->node (:right m)))))

;;;

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn hash-levels
  "Returns a seq of seqs, with `output-level` elements. Each represents a
  level of the hash-tree, from bottom to top.

  The `segment-seq` represents the input hashes of the underlying segments.
  Elements in the hash-seq may be `nil`, denoting no elements within that
  segment."
  [output-levels num-segments segment-seq]
  (let [levels (long (p/inc (log2 num-segments)))
        emit-all? (= output-levels levels)
        ^objects crcs (object-array levels)
        get-crc (fn [^long idx]
                  (if-let [crc (aget crcs idx)]
                    crc
                    (let [crc (CRC32.)]
                      (aset crcs idx crc)
                      crc)))
        lists (object-array
                (repeatedly output-levels
                  #(ArrayList.)))]
    (loop [idx 0, s segment-seq]
      (when-not (empty? s)

        (let [x (first s)]
         
          ;; update the level-0 hash
          (when x
            (let [^CRC32 c (get-crc 0)]
              (.update c x)))

          (when emit-all?
            (.add ^ArrayList (aget lists 0) x))
          
          ;; ascend the levels as appropriate
          (loop [idx idx, level 0]
            (when (== 1 (p/bit-and 1 idx))
              (let [crc (aget crcs level)]
                
                ;; if there's a crc, propagate it upwards
                (when crc
                  (.update ^CRC32 (get-crc (p/inc level)) (.getValue crc))
                  (aset crcs level nil))

                ;; if we're above the threshold for the output tree, write to it
                (let [output-level (p/+ output-levels 1 (p/- level levels))]
                  (when (p/<= 0 output-level)
                    (.add ^ArrayList (aget lists output-level) (when crc (.getValue crc))))))
              (recur (p/>> idx 1) (p/inc level)))))

        (recur (p/inc idx) (rest s))))

    (map seq lists)))

(defn- hash-levels->tree
  "Takes tiered sequences from `hash-levels`, and returns the root `Node` of a
  tree."
  [hash-levels num-segments]
  (let [k (long (/ num-segments (Math/pow 2 (dec (count hash-levels)))))]
    (first
      (reduce
        (fn [nodes hashes]
          (map
            (fn [hash [^Node l ^Node r]]
              (Node. hash (.min-segment l) (.max-segment r) l r))
            hashes
            (partition 2 nodes)))
        (map
          (fn [idx hash]
            (Node. hash (* idx k) (* (inc idx) k) nil nil))
          (range)
          (first hash-levels))
        (rest hash-levels)))))

(defn tree
  "Returns the root `Node` of a hash-tree with `depth` levels.  The input
  `segment-seq` is a list of hashes for a discrete number of segments, which
  must have a cardinality which is a power of two."
  ([depth segment-seq]
   (tree depth (count segment-seq) segment-seq))
  ([depth num-segments segment-seq]
   (hash-levels->tree 
     (hash-levels depth num-segments segment-seq)
     num-segments)))

;;;

(defn- merge-ranges [a b]
  "Merges contiguous ranges.  Returns a list of one or two ranges, depending on whether `a` and `b`
   are contiguous." 
  (if (and a b)
    (let [prefix (butlast a)
          suffix (rest b)
          a' (last a)
          b' (first b)]
      (concat
        prefix
        (if (= (last a') (first b'))
          [[(first a') (last b')]]
          [a' b'])
        suffix))
    (or a b)))

(defn identical-ranges
  "Returns a list of [min,max] tuples describing the segment ranges for which
  the two nodes are identical."
  [^Node n1 ^Node n2]
  (when (and n1 n2)
    (if (== (.hash n1) (.hash n2))
      [[(.min-segment n1) (.max-segment n1)]]
      (merge-ranges
        (identical-ranges (.left n1) (.left n2))
        (identical-ranges (.right n1) (.right n2))))))

(defn diff-ranges
  "Returns a list of [min,max] tuples describing the segment ranges for which
  the two nodes are different."
  [^Node n1 ^Node n2]
  (let [min (.min-segment n1)
        max (.max-segment n1)
        ranges (identical-ranges n1 n2)]
    (concat
      (when-not (= min (ffirst ranges))
        [[0 (ffirst ranges)]])
      (->> ranges
        (partition 2 1)
        (map
          (fn [[[_ l] [u _]]]
            [l u])))
      (when-not (= max (last (last ranges)))
        [[(last (last ranges)) max]]))))

(defn diffs
  "Returns a sequence of segment indices for which two nodes are different."
  [n1 n2]
  (mapcat (partial apply range)
          (diff-ranges n1 n2)))
