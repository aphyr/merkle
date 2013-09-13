(ns merkle.kv.fixed
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
   left
   right])

(defn- crc32 [s]
  (let [crc (CRC32.)]
    (doseq [x s]
      (.update crc (.hashCode ^Object x)))
    (.getValue crc)))

(defn- log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn- hash-levels
  "Returns a seq of seqs, with `output-level` elements.  Each represents a level of the hash-tree, from bottom to top.

   The `segment-seq` represents the input hashes of the underlying segments.  Elements in the hash-seq may be `nil`,
   denoting no elements within that segment."
  [output-levels num-segments segment-seq]
  (let [levels (long (p/inc (log2 num-segments)))
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
    (loop [idx 0, s hash-seq]
      (when-not (empty? s)

        (let [x (first s)]
          
          ;; update the level-0 hash
          (when x
            (let [^CRC32 c (get-crc 0)]
              (.update c x)))
          
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
  "Takes tiered sequences from `hash-levels`, and returns the root `Node` of a tree."
  [hash-levels]
  (first
    (reduce
      (fn [nodes hashes]
        (map
          (fn [hash [l r]]
            (Node. hash l r))
          hashes
          (partition 2 nodes)))
      (map #(Node. % nil nil) (first hash-levels))
      (rest hash-levels))))

(defn hash-tree
  "Returns the root `Node` of a hash-tree with `depth` levels.  The input `segment-seq` is a list of hashes for a discrete
   number of segments, which must have a cardinality which is a power of two."
  [depth num-segments segment-seq]
  (hash-levels->tree
    (hash-levels depth num-segments segment-seq)))





