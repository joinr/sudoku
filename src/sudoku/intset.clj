(ns sudoku.intset)

(deftype BitSetIterator [^:unsynchronized-mutable ^long position ^java.util.BitSet set]
  tech.v2.datatype.LongIter
  (hasNext [this]  (pos? (.nextSetBit set position)))
  (nextLong [this]
    (let [res (long (.nextSetBit set position))]
      (set! position (unchecked-inc res))
     res)))

(deftype BitSet [^java.util.BitSet bs]
  java.util.Set
  (add [this k]
    (let [^long k k]
      (.set bs k)
    true))
  (addAll [this  coll]
    (when coll
      (loop [iter (.iterator ^java.lang.Iterable coll)]
        (when (.hasNext iter)
          (do (.set bs (long (.next iter)))
              (recur iter)))))
    true)
  (contains [this o]
    (let [^long o o]
      (.get bs o)))
  (clear [this]
    (do (.clear bs)
        this))
  (size [this] (.cardinality bs))
  (isEmpty [this] (zero? (.cardinality bs)))
  (remove [this k]
    (let [^long k k]
      (.set bs k false) this))
  (iterator [this] (BitSetIterator. 0 bs)))

(defn ^BitSet bitset [& entries]
  (doto (BitSet. (java.util.BitSet.))
        (.addAll entries)))


;;Still curious to see if this works better than BitSet....probably not.

;; (deftype IntSetIterator [^:unsynchronized-mutable ^long position
;;                           ^longs set ^long bound]
;;   tech.v2.datatype.LongIter
;;   (hasNext [this]
;;     (and (< position bound)
;;          (let [next-val ]
;;            )))
;;   (nextLong [this]
;;     (let [retval (.current this)]
;;       (loop [pos (unchecked-inc position)]
;;         (if (or (not (zero?  (bit-and cur-value set)))
;;                 (< set 0)
;;                 (> cur-value set))
;;           (do (set! position pos)
;;               (.current this))
;;           (recur (unchecked-inc pos)))))
;;     retval)
;;   (current [this] position))

;; (deftype IntSet [^booleans arr]
;;   java.util.Set
;;   (add [this k]
;;     (aset arr (long k) true))
;;   java.util.Iterable
;;   (iterator [this]
;;     (reify java.util.Iterator
;;       (hasNext [this]  )
;;       (next    [this]  )
;;       (remove  [this] (throw (ex-info "invalid operation" {:method 'java.util.Iterator/remove}))))))

;; (defn int-set [n]
;;   (IntSet. (boolean-array n)))
