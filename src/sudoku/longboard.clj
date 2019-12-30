(ns sudoku.longboard
  (:require [sudoku.core :as core]
            [sudoku.longset :as longset]
            [tech.parallel.for :refer [serial-for]])
  (:import [java.util BitSet]
           [java.io Writer]))


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
  (toString [board] (core/board->str (->core-board board) false)))


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



(defn assign!
  [^SudokuBoard board ^long elem-pos ^long value]
  ;;nil boards indicate invalid states
  (when board
    (let [^longs board-values (.board board)
          ^BitSet solved-values (.solved-pos board)
          elem-pos (long elem-pos)]
      (cond
        (not (valid? board))
        nil
        (.get solved-values elem-pos)
        (when (= value (longset/inline-longset-first
                        (aget board-values elem-pos)))
          [board nil])
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
                       0 (set-invalid! board)
                       nil)))))))
          (when (valid? board)
            [board next-assignments]))))))
