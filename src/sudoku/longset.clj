(ns sudoku.longset
  (:import [clojure.lang Counted IPersistentSet]))


;;A single long integer with bits set high represents a
;;set effectively assuming you never need more than 9 entries, only want
;;to have integers in it from 0-63, etc.  For sudoku, this is more
;;than enough.
(declare longset-count longset-empty? longset-disj longset-contains? longset-cons longset)

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
      (->> (range 0 63)
           (filter #(.contains this %))))))


(defn longset?
  [item]
  (instance? Longset item))


(defn longset-count
  [^Longset longset]
  (Long/bitCount (.set longset)))


(defn longset-empty?
  [^Longset longset]
  (= 0 (.set longset)))


(defn- check-longset-arg
  ^long [^long arg]
  (when-not (and (>= arg 0)
                 (<= arg 63))
    (throw (Exception. (format "Longset arg %s out of range [0-63]" arg))))
  arg)


(defn longset-disj
  ^Longset [^Longset longset ^long arg]
  (->Longset (bit-and (.set longset)
                      (bit-not (bit-shift-left 1 arg)))))


(defn longset-contains?
  [^Longset longset arg]
  (not= 0 (bit-and (.set longset)
                   (bit-shift-left 1 (check-longset-arg arg)))))


(defn longset->set
  [^Longset set]
  (apply sorted-set
         (->> (range 1 10)
              (filter #(not= 0
                             (bit-and (.set set)
                                      (bit-shift-left 1 (long %))))))))


(defn longset-cons
  [^Longset set arg]
  (->Longset
   (bit-and (.set set)
            (bit-shift-left 1 (check-longset-arg arg)))))


(defn longset
  [& args]
  (->Longset
   (->> args
        (map #(bit-shift-left 1 (check-longset-arg %)))
        (apply bit-or))))
