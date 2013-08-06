(ns merkle.kv.linear
  "Merkle trees are a data structure used to efficiently localize differences
  between two copies of an ordered collection. Each element in the collection
  is hashed into a leaf node. Every n leaf nodes have their hashes concatenated
  and hashed to produce a supervening node. Those nodes are then combined and
  hashed again, until a single root node has hashed the entire collection.
  Given two of these trees, one can find differences between two copies of the
  collection by finding nodes with differing hashes.

  This particular implementation is aimed at a specific sub-problem: comparing
  two key-value collections where:

  1. The key set could vary between copies.
  2. The collections could be much larger than RAM.
  3. Keys are in sorted order.
  4. The keyspace and probability distribution of keys is unknown.
  5. New keys are usually added at the end.

  Because the key set varies, we need to encode some information about the keys
  in the tree nodes. Each of our nodes keeps the minimum and maximum key
  covered by its hash. We can then consider nodes equal when they have the
  same hash and the same start and end key, which makes it possible to compare
  collections with different keys.

  Since the collection is much larger than RAM (for instance, a table on disk),
  but the distribution of the keyspace is unknown, our algorithm forgoes having
  a well-balanced tree in favor of a single linear scan over the collection,
  accruing at most log(n) nodes in RAM.

  Because the key are in sorted order, we can efficiently construct this tree
  in a single linear pass, at the cost of having a much smaller right-hand
  subtree. The best-case scenario is a fully balanced tree, and the worst-case
  scenario is a root node with a fully balanced subtree on the left, and a
  single leaf node on the right. The worst-case cost for this structure, as
  compared to a balanced btree, is one additional layer of depth.

  Since new keys are added mostly to the end, two collections which are
  periodically synchronized will tend to agree on their
  first/smallest/left-hand keys and nodes. If two collections agree on the
  first N keys, their hash trees will have an identical node->key distribution
  over those N keys. In a periodically synchronized collection, this means
  we'll only differ on a few keys towards the end, and can efficiently ignore
  the bulk of the dataset. In the worst-case scenario, a single key is added to
  the beginning of the collection, and none of the hashes are aligned, forcing
  a full resync."
  (:require [merkle.range :as range]))

(defrecord Node [^long hash min-key max-key left right])

(defn node->map
  "Converts a Node to a standard Clojure map; useful for serialization."
  [^Node node]
  (when node
    {:hash (.hash node)
     :min-key (.min-key node)
     :max-key (.max-key node)
     :left    (node->map (.left node))
     :right   (node->map (.right node))}))

(defn map->node
  "Converts a map to a Node; useful for serialization."
  [m]
  (when m
    (Node. (:hash m)
           (:min-key m)
           (:max-key m)
           (map->node (:left m))
           (map->node (:right m))))) 
(defn key-range
  "The inclusive range of keys a Node covers."
  [^Node node]
  (when node
    (list (.min-key node)
          (.max-key node))))

(defn aligned?
  "Are two nodes comparable--i.e. do they have the same min-key and max-key?"
  [^Node n1 ^Node n2]
  (if (nil? n1)
    (nil? n2)
    (and n2 
         (= (.min-key n1) (.min-key n2))
         (= (.max-key n1) (.max-key n2)))))

(defn same?
  "Are two nodes equivalent; i.e. do they have the same hash, min-key, and
  max-key?"
  [^Node n1 ^Node n2]
  (if (nil? n1)
    (nil? n2)
    (and n2
         (= (.hash n1) (.hash n2))
         (= (.min-key n1) (.min-key n2))
         (= (.max-key n1) (.max-key n2)))))

(defn hash-nodes
  "Combines two nodes together."
  [^Node left ^Node right]
  (cond
    (nil? left) right
    (nil? right) left
    :else (Node. (hash (list (.hash left)
                             (.hash right)))
                 (.min-key left)
                 (.max-key right)
                 left
                 right)))

(defn assoc-or-conj!
  "Associates, and conj's on OutOfBoundsException."
  [v i value]
  (try
    (assoc! v i value)
    (catch IndexOutOfBoundsException e
      (conj! v value))))

(defn percolate
  "Takes a vector of nodes awaiting merging with their neighbors, and a new
  node to merge into position i. Returns a new vector of nodes awaiting merging
  with their neighbors."
  [levels i right]
  (let [left (try (nth levels i)
                  (catch IndexOutOfBoundsException e
                    :out-of-bounds))]
    (let [x (persistent! levels)
          levels (transient x)]
      (condp = left
        ; Fill in an empty space above
        nil            (assoc! levels i right)

        ; Add a new slot to the top
        :out-of-bounds (conj! levels right)

        ; Merge with the left node and recur upwards.
        (recur (assoc! levels i nil)
               (inc i)
               (hash-nodes left right))))))

(defn transient-seq
  "A sequence over a transient collection. Definitely not safe unless you
  realize it immediately."
  [levels]
  (->> levels
       count
       range
       (map (partial nth levels))))

(defn compact?
  "Is the given level merged into a single node--i.e. does it look like [nil
  nil nil nil node]. Levels are transient so we have to use nth."
  [levels]
  (->> levels
       transient-seq
       (remove nil?)
       (count)
       (<= 1)))

(defn compact
  "Takes a vector of nodes awaiting merging with their neighbors, and merges
  them all into a single node. This handles the (usual) case where the right
  side of the tree has fewer entries than the right, leaving us with dangling
  levels."
  [levels]
  (let [levels (persistent! levels)]
    (if (empty? levels)
      nil
      (->> levels reverse (remove nil?) (reduce hash-nodes)))))

(defn tree
  "Computes a merkle tree for a sorted kv collection. Does not retain the head.
  
  Can take two optional functions: keyfn and valfn, which extract the key and
  the value from a given element of the collection; if not given, these default
  to key and val, respectively."
  ([coll]
   (tree coll key val))
  ([coll keyfn valfn]
   ; We keep a temporary vector containing uncombined nodes in the tree as we
   ; move through the collection. The 0th entry of this vector is a node for a
   ; single element. The last entry is the Node at the highest level yet
   ; reached. Nodes percolate upwards through this vector until we have a
   ; single node at the top containing the full tree.
   (->> coll
        (reduce (fn [levels element]
                  ; Construct base-level node
                  (let [k    (keyfn element)
                        h    (hash (valfn element))
                        node (Node. h k k nil nil)]
                    (percolate levels 0 node)))
                (transient []))
        compact)))

(defn pr-node
  [^Node node]
  (if node
    (str (pr-str (.hash node)) " (" (.min-key node) " " (.max-key node) ")")
    "nil"))

(defn identical-ranges
  "Returns an ordered lazy sequence of (start-key end-key) pairs for which two
  trees are known to be identical."
  [^Node t1 ^Node t2]
;  (println "compare" (pr-node t1) (pr-node t2))

  (let [r1 (key-range t1)
        r2 (key-range t2)]
    (cond
      ; One node is nil
      (nil? t1) (list)
      (nil? t2) (list)

      ; Nodes cover the same key range.
      (= r1 r2)
      (if (= (.hash t1) (.hash t2))
        ; Nodes are identical.
        (list (list (.min-key t1) (.max-key t1)))

        ; We *know* these keys differ.
        (lazy-cat (identical-ranges (.left t1) (.left t2))
                  (identical-ranges (.right t1) (.right t2))))

      ; t1 and t2 are totally disjoint; nothing here for us.
      (range/disjoint? r1 r2)
      (list)

      ; t1 is a proper subset of t2; descend into t2 looking for t1.
      (range/subset? r1 r2)
      (lazy-cat (identical-ranges t1 (.left t2))
                (identical-ranges t1 (.right t2)))
      
      ; t2 is a proper subset of t1; descend into t1 looking for it.
      (range/subset? r2 r1)
      (lazy-cat (identical-ranges (.left t1) t2)
                (identical-ranges (.right t1) t2))

      ; t1 and t2 intersect, but neither is a subset of the other. Expand to
      ; all branches.
      (range/intersect? r1 r2)
      (lazy-cat (identical-ranges (.left  t1) (.left  t2))
                (identical-ranges (.left  t1) (.right t2))
                (identical-ranges (.right t1) (.left  t2))
                (identical-ranges (.right t1) (.right t2)))
      
      :else (throw (IllegalStateException. "Shouldn't get here.")))))

(defn diff-helper
  "Given a sorted sequence of identical ranges, and a sequence of elements from
  a local collection, yields elements which differ by riffling through both
  sequences."
  [ranges coll keyfn]
  ; [range 1]     [range 2]     [range 3]
  ;  x    x    o    x       o o           o
  (when-let [element (first coll)]
    (let [k (keyfn element)
          range (first ranges)]
        (cond
          ; We're out of ranges; return entire remaining collection.
          (nil? range)
          coll

          ; Element lies before the range and should be included.
          (< (compare k (first range)) 0)
          (cons element
                (lazy-seq (diff-helper ranges (next coll) keyfn)))

          ; Element lies within the range
          (<= (compare k (second range)) 0)
          (lazy-seq (diff-helper ranges (next coll) keyfn))

          ; Element lies after the range.
          :else
          (lazy-seq (diff-helper (next ranges) coll keyfn))))))

(defn diff
  "Given a local collection, a local tree, and a remote tree, yields a lazy
  sequence of elements from the local collection which are not known to be
  identical based on the two trees."
  ([coll local-tree remote-tree]
   (diff coll local-tree remote-tree key))
  ([coll local-tree remote-tree keyfn]
   (diff-helper (identical-ranges local-tree remote-tree) coll keyfn)))
