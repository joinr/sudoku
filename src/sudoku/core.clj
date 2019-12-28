(ns sudoku.core
  (:require [libpython-clj.require :as require-python]
            [tech.v2.tensor :as dtt]
            [tech.v2.tensor.impl :as dtt-impl]
            [tech.v2.tensor.dimensions :as dtt-dims]
            [tech.v2.tensor.typecast :as dtt-typecast]
            [tech.v2.datatype :as dtype]
            [tech.v2.datatype.protocols :as dtype-proto]
            [tech.v2.datatype.typecast :as typecast]
            [tech.v2.datatype.unary-op :as unary-op]
            [tech.v2.datatype.functional :as dfn]
            [clojure.set :as c-sete]
            [clojure.java.io :as io])
  (:import [tech.v2.datatype ObjectReader]))


(set! *warn-on-reflection* true)


(def digits (apply sorted-set (range 1 10)))

(def board-shape [3 3 3 3])


(def empty-board
  (-> (dtt/new-tensor board-shape :datatype :object)
      (dtype/set-constant! digits)))


(defn index->wxyz
  [^long item-idx]
  (let [strides (:strides (dtt/tensor->dimensions empty-board))]
    (loop [strides strides
           retval []
           item-idx item-idx]
      (let [stride (first strides)]
        (if stride
          ;;define stride to be a long
          (let [stride (long stride)]
            (recur (rest strides)
                   (conj retval (quot item-idx stride))
                   (rem item-idx stride)))
          retval)))))


(def board-indexes
  (dtt/->tensor (-> (range (dtype/ecount empty-board))
                    (dtt/reshape board-shape))
                :datatype :int32))


(def norvig-indexes
  ;;This is how we index things
  (let [[outer-row outer-col inner-row inner-col] (range 4)]
    (->
     (dtt/transpose board-indexes
                    ;;This is how norvig indexes things
                    [outer-row inner-row outer-col inner-col])
     ;;Make transformation concrete
     (dtt/clone))))


(def get-affected-indexes
  (memoize
   (fn [outer-row outer-col inner-row inner-col]
     (let [row-indexes (->> (dtt/select board-indexes outer-row :all inner-row :all)
                            (dtype/->reader)
                            set)
           col-indexes (->> (dtt/select board-indexes :all outer-col :all inner-col)
                            (dtype/->reader)
                            set)
           unit-indexes (->> (dtt/select board-indexes outer-row outer-col :all :all)
                             (dtype/->reader)
                             set)]
       (c-set/union row-indexes col-indexes unit-indexes)))))


(defn display-board
  [board]
  (let [norvig-board-rows
        (->> (dtype/indexed-reader (dtype/->reader norvig-indexes)
                                   board)
             (map (fn [item]
                    (cond
                      (number? item)
                      item
                      (nil? item)
                      "-"
                      :else
                      ".")))
             (partition 9))]
    (doseq [[idx board-row] (map-indexed vector norvig-board-rows)]
      (when (= 0 (rem idx 3))
        (println "-------------------------"))
      (apply println
             (concat ["|"]
                     (->> (partition 3 board-row)
                          (mapcat (fn [row-data]
                                    (concat row-data ["|"])))))))
    (println "-------------------------")))


(defn choose
  "Returns a new board if we can choose this value and nil otherwise"
  [board [outer-row outer-col inner-row inner-col] val]
  (let [board-indexes (dtt-typecast/->object-reader board-indexes)
        board (typecast/->object-reader board)
        outer-row (int outer-row)
        outer-col (int outer-col)
        inner-row (int inner-row)
        inner-col (int inner-col)
        val (long val)
        item-idx (.tensorRead board-indexes
                              [outer-row outer-col inner-row inner-col])
        ;;These could be precalculated
        affected-indexes (get-affected-indexes outer-row outer-col
                                               inner-row inner-col)
        element-count (long (apply * board-shape))
        new-data (->> (reify
                        ObjectReader
                        (lsize [rdr] element-count)
                        (read [rdr idx]
                          (let [entry (.read board idx)]
                            (if (= idx item-idx)
                              val
                              (if (contains? affected-indexes idx)
                                (cond
                                  (number? entry)
                                  (when-not (= (long entry) val) entry)
                                  (set? entry)
                                  (when (contains? entry val)
                                    (let [retval (disj entry val)]
                                      (when-not (empty? retval)
                                        retval)))
                                  :else
                                  (throw (Exception. (str "Logic error: "
                                                          entry " " val))))
                                entry)))))
                      (dtype/make-container :java-array :object))]

    ;;When we have a valid board.
    (if-not (some nil? new-data)
      ;;propogate the constraint and return new board if possible.
      (reduce (fn [new-data chosen-one-idx]
                (when new-data
                  (choose new-data (index->wxyz chosen-one-idx)
                          (first (dtype/get-value new-data chosen-one-idx)))))
              (dtt/reshape new-data board-shape)
              ;;Filter in indexes that propagate the constraint
              (dfn/argfilter #(and (set? %1)
                                   (= 1 (count %1)))
                             new-data))
      (throw (Exception. (format "Failed constraint:
%s
--->
%s"
                                 (with-out-str (display-board board))
                                 (with-out-str (display-board new-data))))))))


(def  grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")


(defn object-tensor
  [item-shape item-seq]
  (dtt-impl/construct-tensor (vec item-seq) (dtt-dims/dimensions item-shape)))


(defn parse-board
  [str-board]
  (let [ascii-zero (short \0)
        ascii-data (->> str-board
                        (map #(- (short %) ascii-zero))
                        (map vector (dtype/->reader norvig-indexes)))]
    (reduce (fn [new-board [item-idx item-val]]
              (let [item-val (short item-val)]
                (if (and (>= item-val 1)
                         (<= item-val 9))
                  (let [new-board
                        (choose new-board (index->wxyz item-idx) item-val)]

                    new-board)
                  new-board)))
            empty-board
            ascii-data)))
