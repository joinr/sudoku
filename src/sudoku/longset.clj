(ns sudoku.longset
  (:import [clojure.lang Counted IPersistentSet]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;A single long integer with bits set high represents a
;;set effectively assuming you never need more than 9 entries, only want
;;to have integers in it from 0-63, etc.  For sudoku, this is more
;;than enough.
(declare longset-count longset-empty? longset-disj longset-contains? longset-cons longset
         longset-iterator)


(deftype LongsetIterator [^:unsynchronized-mutable ^long position
                          ^long set]
  tech.v2.datatype.LongIter
  (hasNext [this]
    (and (not= 0 set)
         (< position 63)
         (let [next-val (bit-shift-left 1 position)]
           (and (> next-val 0)
                (<= next-val set)))))
  (nextLong [this]
    (let [retval (.current this)]
      (loop [pos (inc position)]
        (let [cur-value (bit-shift-left 1 pos)]
          (if (or (not= 0 (bit-and cur-value set))
                  (< set 0)
                  (> cur-value set))
            (do (set! position pos)
                (.current this))
            (recur (inc pos)))))
      retval))
  (current [this]
    (if (and (>= position 0)
             (<= position 62))
      position
      -1)))


(definline inline-longset-first
  [set-val]
  `(Long/numberOfTrailingZeros ~set-val))


(deftype Longset [^long set]
  IPersistentSet
  (disjoin [this key] (longset-disj this key))
  (contains [this key] (boolean (longset-contains? this key)))
  (get [this key] (when (longset-contains? this key) key))
  (count [this] (int (longset-count this)))
  (cons [this arg] (longset-cons this arg))
  (empty [this] (longset))
  (equiv [this other]
    (boolean
     (when (instance? Longset other)
       (= set (.set ^Longset other)))))
  (seq [this]
    (when-not (= 0 set)
      (iterator-seq (.iterator this))))
  Iterable
  (iterator [this] (longset-iterator this))
  clojure.lang.IFn
  (invoke [this k] (longset-contains? this k)))


(defn longset?
  [item]
  (instance? Longset item))


(definline inline-longset-count
  [set-val]
  `(Long/bitCount ~set-val))


(defn longset-count
  [^Longset longset]
  (inline-longset-count (.set longset)))


(definline inline-longset-empty?
  [set-val]
  `(= 0 (long ~set-val)))

(defn longset-empty?
  [^Longset longset]
  (inline-longset-empty? (.set longset)))

(defn longset-iterator
  [^Longset longset]
  (let [retval (LongsetIterator. -1 (.set longset))]
    (.nextLong retval)
    retval))


(defn- check-longset-arg
  ^long [^long arg]
  (when-not (and (>= arg 0)
                 (<= arg 62))
    (throw (Exception. (format "Longset arg %s out of range [0-62]" arg))))
  arg)



(definline inline-longset-disj
  [set-val arg]
  `(bit-and (long ~set-val)
            (bit-not (bit-shift-left 1 (long ~arg)))))

(defn longset-disj
  ^Longset [^Longset longset ^long arg]
  (->Longset (inline-longset-disj (.set longset) arg)))


(definline inline-longset-contains?
  [set-val arg]
  `(not= 0 (bit-and (long ~set-val)
                    (bit-shift-left 1 (long ~arg)))))

(defn longset-contains?
  [^Longset longset ^long arg]
  (inline-longset-contains? (.set longset) (check-longset-arg arg)))


(defn longset->set
  [^Longset set]
  (apply sorted-set
         (->> (range 1 10)
              (filter #(not= 0
                             (bit-and (.set set)
                                      (bit-shift-left 1 (long %))))))))


(definline inline-longset-conj
  [set-val arg]
  `(bit-or (long ~set-val)
            (bit-shift-left 1 (long ~arg))))


(defn longset-conj
  [^Longset set arg]
  (->Longset (inline-longset-conj (.set set) (check-longset-arg arg))))


(defn longset
  [& args]
  (if args
    (->Longset
     (->> args
          (map #(bit-shift-left 1 (check-longset-arg %)))
          (apply bit-or 0)))
    (->Longset 0)))
