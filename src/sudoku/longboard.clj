(ns sudoku.longboard
  (:require [sudoku.core :as core]
            [sudoku.longset :as longset]
            [tech.parallel.for :refer [serial-for]]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:import [java.util BitSet HashSet]
           [java.io Writer]
           [sudoku.longset LongsetIterator]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol PSudokuValid
  (set-invalid! [board])
  (valid? [board]))

(declare ->core-board)


(deftype SudokuBoard [^longs board
                      ^BitSet solved-pos
                      ^:volatile-mutable validboard?]

  PSudokuValid
  (set-invalid! [board] (set! validboard? false))
  (valid? [board] validboard?)
  Object
  (toString [board] (core/board->str (->core-board board) true)))


(defmethod print-method SudokuBoard
  [^SudokuBoard board ^Writer w]
  (.write w (.toString board)))


(def empty-board (SudokuBoard.
                  (long-array
                   (repeat core/n-board-elements
                           (.set ^sudoku.longset.Longset core/digits)))
                  (BitSet.)
                  true))


(defn ->core-board
  [^SudokuBoard board]
  (mapv #(longset/->Longset %) (.board board)))


(defn display-board
  [^SudokuBoard board & [display-sets?]]
  (core/display-board (->core-board board)
                      display-sets?))


(defn duplicate-board
  ^longs [^SudokuBoard old-board]
  (let [retval (long-array core/n-board-elements)]
    (System/arraycopy (.board old-board) 0 retval 0 (int core/n-board-elements))
    (SudokuBoard. retval (.clone ^BitSet (.solved-pos old-board))
                  (valid? old-board))))



(defn do-assign!
  ^java.util.List [^SudokuBoard board ^long elem-pos ^long value]
  ;;nil boards indicate invalid states
  (when (and board (valid? board))
    (let [^longs board-values (.board board)
          ^BitSet solved-values (.solved-pos board)
          elem-pos (long elem-pos)]
      (cond
        (.get solved-values elem-pos)
        (when (= value (longset/inline-longset-first
                        (aget board-values elem-pos)))
          [])
        :else
        (let [[y x] (core/index->yx elem-pos)
              ^longs affected-indexes (-> (core/get-affected-indexes y x)
                                          :affected-indexes)
              next-assignments (java.util.ArrayList.)
              n-iters (alength affected-indexes)]
          (serial-for
           idx
           n-iters
           (let [target-idx (aget affected-indexes idx)]
             (if (= target-idx elem-pos)
               (do
                 (aset board-values elem-pos (longset/inline-longset-conj 0 value))
                 (.set solved-values elem-pos true))
               (let [pos-val (aget board-values target-idx)]
                 (when (longset/inline-longset-contains? pos-val value)
                   (let [new-val (longset/inline-longset-disj pos-val value)
                         new-bits (Long/bitCount new-val)]
                     (aset board-values target-idx new-val)
                     (case new-bits
                       1 (.add next-assignments target-idx)
                       0 (do (println "constraint violation" elem-pos value new-val)
                             (set-invalid! board))
                       nil)))))))
          (when (valid? board)
            next-assignments))))))


(defn assign-constraint!
  "Perform assignments an propagate the constraint that a set with one elements
  generates a new assignment.  Keep track of all indexes assigned to."
  [^SudokuBoard board ^long elem-pos ^long value]
  (let [^java.util.List next-assignments (do-assign! board elem-pos value)
        assigned-indexes (HashSet.)]
    (when next-assignments
      (.add assigned-indexes elem-pos)
      (let [^longs board-values (.board board)
            ^BitSet solved-values (.solved-pos board)]
        (loop [idx 0
               board board]
          ;;Size check in loop is intentional
          (if (and board (< idx (.size next-assignments)))
            (let [assign-idx (.get next-assignments idx)
                  board (if-not (.get solved-values assign-idx)
                          (let [board-val (aget board-values assign-idx)
                                new-assign (do-assign! board assign-idx
                                                       (longset/inline-longset-first board-val))]
                            (when new-assign
                              (.add assigned-indexes assign-idx)
                              (.addAll next-assignments new-assign)
                              board))
                          board)]
              (recur (inc idx) board))
            (when (and board (valid? board))
              [board assigned-indexes])))))))


;;The second half of the algorithm scans for 'place' assignments; situations where
;;the assignment can happen because there is only one potential place for an item.
(def units
  (into-array Object
              (concat (map (comp long-array core/tensor->elem-seq core/row-indexes) (range 9))
                      (map (comp long-array core/tensor->elem-seq core/column-indexes) (range 9))
                      (map (comp long-array core/tensor->elem-seq)
                           (for [y [0 3 6]
                                 x [0 3 6]]
                             (core/unit-indexes y x))))))


(def elem-idx->unit-indexes
  (into-array Object (->> (range core/n-board-elements)
                          (map #(let [[y x] (core/index->yx %)
                                      y (long y)
                                      x (long x)]
                                  [y (+ 9 x) (+ 18
                                                (* 3 (quot y 3))
                                                (quot x 3))])))))



(defn elem-indexes->unit-indexes
  ^java.util.Set [^Iterable assigned-indexes]
  (let [retval (HashSet.)
        ^"[Ljava.lang.Object;" elem-idx->unit-indexes elem-idx->unit-indexes
        iter (.iterator assigned-indexes)]
    (loop [continue? (.hasNext iter)]
      (if continue?
        (let [^java.util.List data (aget elem-idx->unit-indexes (.next iter))]
          (.add retval (.get data 0))
          (.add retval (.get data 1))
          (.add retval (.get data 2))
          (recur (.hasNext iter)))
        retval))))


(defn scan-unit
  ^longs [^SudokuBoard board unit-index]
  (let [^longs unit (aget ^"[Ljava.lang.Object;" units unit-index)
        n-elems (alength unit)
        record (long-array 10 -1)
        ^longs board-values (.board board)]
    (loop [idx 0]
      (if (< idx n-elems)
        (let [entry-idx (aget unit idx)
              entry (aget board-values entry-idx)
              entry-iter (doto (longset/LongsetIterator. -1 entry)
                           (.nextLong))]
          (loop [continue? (.hasNext entry-iter)]
            (when continue?
              (let [next-val (.next entry-iter)
                    summary-val (aget record next-val)]
                (if (= summary-val -1)
                  (aset record next-val entry-idx)
                  (aset record next-val Long/MAX_VALUE)))
              (recur (.hasNext entry-iter))))
          (recur (inc idx)))
        record))))


(defn scan-for-place-assignments
  [^SudokuBoard board assigned-indexes]
  (let [unit-indexes (elem-indexes->unit-indexes assigned-indexes)
        idx-iter (.iterator ^java.util.Set unit-indexes)
        next-assignments (java.util.ArrayList.)
        ^BitSet solved-values (.solved-pos board)]
    (when-not (= (long core/n-board-elements) (.size solved-values)))
    (loop [continue? (.hasNext idx-iter)]
      (when (and (valid? board) continue?)
        (let [^longs unit-values (scan-unit board (.next idx-iter))]
          (loop [uval-idx 1]
            (when (< uval-idx 10)
              (let [uval (aget unit-values uval-idx)]
                (when (= -1 uval) (set-invalid! board))
                (when (and (not= uval Long/MAX_VALUE)
                           (not (.get solved-values uval)))
                  (.add next-assignments [uval uval-idx]))
                (recur (inc uval-idx)))))
          (recur (boolean (and (valid? board) (.hasNext idx-iter)))))))
    (when (valid? board)
      next-assignments)))


(defn assign!
  ([^SudokuBoard board ^java.util.List assignment-list]
   (let [assigned-indexes (HashSet.)]
     (loop [continue? (not (.isEmpty assignment-list))]
       (when (and continue? (boolean (valid? board)))
         (.clear assigned-indexes)
         ;;Assign everything recording which indexes actually got assigned values.
         (let [n-assignments (.size assignment-list)]
           (loop [assign-idx 0]
             (when (< assign-idx n-assignments)
               (let [[elem-idx elem-val] (.get assignment-list assign-idx)]
                 (when-let [assign-result (assign-constraint! board elem-idx elem-val)]
                   (let [[board indexes] assign-result]
                     (.addAll assigned-indexes indexes))
                   (recur (inc assign-idx))))))
           (.clear assignment-list)
           ;;Add any place assignments found through checking all the units assocated
           ;;with the assigned indexes
           (when (valid? board)
             (.addAll assignment-list
                      (scan-for-place-assignments board assigned-indexes))))
         (recur (not (.isEmpty assignment-list)))))
     (when (valid? board)
       board)))
  ([board elem-idx elem-val]
   (let [new-list (java.util.ArrayList.)]
     (.add new-list [elem-idx elem-val])
     (assign! board new-list))))


(defn parse-board
  [str-board]
  (let [ascii-zero (Character/getNumericValue \0)
        meaningful-set (set "0123456789.")
        parsed-board (->> str-board
                          (filter meaningful-set)
                          (map #(- (Character/getNumericValue (char %)) ascii-zero))
                          (map-indexed (fn [item-idx ^long item-val]
                                         (when (and (>= item-val 1)
                                                    (<= item-val 9))
                                           [item-idx item-val])))
                          (remove nil?)
                          (vec))]
    (assign! (duplicate-board empty-board)
             (java.util.ArrayList.  ^java.util.Collection parsed-board))))


(defn solved?
  [^SudokuBoard board]
  (let [^BitSet solved (.solved-pos board)]
    (= (.size solved) (long core/n-board-elements))))


(defn- minimal-len-set
  [^SudokuBoard board]
  (let [^longs board-values (.board board)
        n-elems (alength board-values)]
    (loop [elem-idx 0
           retval nil
           min-count -1]
      (if (< elem-idx n-elems)
        (let [entry (aget board-values elem-idx)
              ecount (Long/bitCount entry)
              retval (if (and (> ecount 1)
                              (or (= min-count -1)
                                  (and (< ecount min-count))))
                       [elem-idx entry]
                       retval)
              min-count (long (if retval (Long/bitCount (long (second retval))) min-count))]
          (recur (inc elem-idx) retval min-count))
        retval))))


(defn search
  [^SudokuBoard board]
  (cond
    (solved? board)
    board
    (or (nil? board) (not (valid? board)))
    nil
    :else
    (let [[item-idx set-items :as min-elem] (minimal-len-set board)
          iter (doto (LongsetIterator. -1 set-items)
                 (.nextLong))]
      (loop [continue (.hasNext iter)]
        (let [new-board
              (when-let [board (assign! (duplicate-board board)
                                        item-idx
                                        (.nextLong iter))]
                (search board))]
          (if-not new-board
            (recur (.hasNext iter))
            new-board))))))


(defn solve
  [board-str]
  (search (parse-board board-str)))


(defn solve-all
  [res-file]
  (let [num-solved (->> (-> (slurp (io/resource res-file))
                            (s/split #"\n"))
                        (map (fn [item]
                               (let [solved (solve item)]
                                 (when-not solved
                                   (throw (Exception. (str "Failed to solve " item))))
                                 solved)))
                        count)]
    (println "solved" num-solved "puzzles"))
  :ok)
