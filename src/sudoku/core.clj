(ns sudoku.core
  (:require [tech.v2.tensor :as dtt]
            [tech.v2.tensor.impl :as dtt-impl]
            [tech.v2.tensor.dimensions :as dtt-dims]
            [tech.v2.tensor.typecast :as dtt-typecast]
            [tech.v2.datatype :as dtype]
            [tech.v2.datatype.protocols :as dtype-proto]
            [tech.v2.datatype.typecast :as typecast]
            [tech.v2.datatype.unary-op :as unary-op]
            [tech.v2.datatype.functional :as dfn]
            [clojure.set :as c-set]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [tech.v2.datatype ObjectReader]
           [tech.v2.tensor LongTensorReader])
  (:gen-class))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn board->str
  [board display-sets?]
  (let [board (map (fn [item]
                     (cond
                       (number? item)
                       (str " " item)
                       (nil? item)
                       " -"
                       :else
                       (if display-sets?
                         (apply str " {" (concat item ["}"]))
                         " .")))
                   board)
        column-len (long (apply max (map count board)))
        n-total-rows (+ 7 (* 9 column-len))
        row-separator (apply str (repeat n-total-rows "-"))
        builder (StringBuilder.)]
    (doseq [[idx board-row] (map-indexed vector (partition 9 board))]
      (when (= 0 (rem (int idx) 3))
        (.append builder row-separator)
        (.append builder "\n"))
      (.append builder "|")
      (->> (partition 3 board-row)
           (map (fn [row-group]
                  (doseq [row-entry row-group]
                    (dotimes [iter (- column-len (count row-entry))]
                      (.append builder " "))
                    (.append builder row-entry))
                  (.append builder " |")))
           (dorun))
      (.append builder "\n"))
    (.append builder row-separator)
    (.toString builder)))


(defn display-board
  [board & [display-sets?]]
  (println (board->str board display-sets?)))



(def digits (apply sorted-set (range 1 10)))

(def board-shape [9 9])

(def n-board-elements (long (apply * board-shape)))

(def board-indexes
  (-> (range (apply * board-shape))
      (dtt/reshape board-shape)))


(def empty-board
  (into-array Object (repeat n-board-elements digits)))


(defn duplicate-board
  [board]
  (let [retval (object-array n-board-elements)]
    (System/arraycopy board 0 retval 0 (int n-board-elements))
    retval))


(defn board->elem-seq
  "To convert to board to a sequence of its elements the fastest way is just to
  make a reader out of it.  If you naively map over a board you will get
  a sequence of rows."
  [board]
  (dtype/->reader board))


(defn row-indexes
  [y]
  (dtt/select board-indexes y :all))


(defn column-indexes
  [x]
  (dtt/select board-indexes :all x))


(defn unit-indexes
  [^long y ^long x]
  (let [x-unit (- x (rem x 3))
        y-unit (- y (rem y 3))]
    (dtt/select board-indexes
                (range y-unit (+ y-unit 3))
                (range x-unit (+ x-unit 3)))))


(defn index->yx
  [^long elem-idx]
  [(quot elem-idx 9) (rem elem-idx 9)])


(defn yx->index
  ^long [^long y ^long x]
  (+ (* 9 y) x))


(def get-affected-indexes
  (memoize
   (fn [y x]
     (let [row-indexes (->> (row-indexes y)
                            (board->elem-seq)
                            set)
           col-indexes (->> (column-indexes x)
                            (board->elem-seq)
                            set)
           unit-indexes (->> (unit-indexes y x)
                             (board->elem-seq)
                             set)]
       (long-array
        (c-set/union row-indexes col-indexes unit-indexes))))))


(def ^:dynamic *error-on-invalid* false)



(defn choose!
  "Modify board with choice"
  [board [y x] val]
  (let [^"[Ljava.lang.Object;" board board
        val (long val)
        item-idx (yx->index y x)
        ;;These could be precalculated
        ^longs affected-indexes (get-affected-indexes y x)
        affected-idx-count (alength affected-indexes)
        propagate-constraints (java.util.ArrayList.)
        valid-board?
        (loop [affect-idx 0
               valid-board? true]
          (if (and valid-board?
                   (< affect-idx affected-idx-count))
            (let [target-idx (aget affected-indexes affect-idx)
                  entry (aget board target-idx)
                  valid-board?
                  (cond
                    (= target-idx item-idx)
                    (do
                      (aset board target-idx (Long. val))
                      true)
                    (number? entry)
                    ;;If this has already been chosen
                    (if (= (long entry) val)
                      ;;Record the invalid position
                      (do
                        (aset board target-idx nil)
                        false)
                      true)
                    (set? entry)
                    (let [retval (disj entry val)]
                      (if-not (empty? retval)
                        (do
                          (aset board target-idx retval)
                          (when (= 1 (count retval))
                            (.add propagate-constraints target-idx))
                          true)
                        (do
                          (aset board target-idx nil)
                          false)))
                    :else
                    (throw (Exception. (str "Logic error: "
                                            entry " " val))))]
              (recur (inc affect-idx) valid-board?))
            valid-board?))]

    ;;When we have a valid board.
    (if valid-board?
      ;;propogate the constraint that sets with 1 item get 'chosen'
      (reduce (fn [board chosen-one-idx]
                ;;One of these may cause a constraint violation
                (when board
                  ;;This may have been chosen as a result of a previous step
                  (let [new-value (aget ^"[Ljava.lang.Object;" board (int chosen-one-idx))]
                    (if (set? new-value)
                      (choose! board
                              (index->yx chosen-one-idx)
                              (first new-value))
                      board))))
              board
              propagate-constraints)
      (when *error-on-invalid*
        (throw (Exception. (format "Failed constraint:
%s"
                                   (board->str board true))))))))


(def grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")


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
                          (remove nil?))]
    (with-bindings {#'*error-on-invalid* true}
      (reduce (fn [new-board [item-idx item-val]]
                (choose! new-board (index->yx item-idx) item-val))
              (duplicate-board empty-board)
              parsed-board))))


(defn solved?
  [board]
  (every? number? (board->elem-seq board)))


(defn- minimal-len-set
  [board]
  (let [^"[Ljava.lang.Object;" board board
        n-elems (dtype/ecount board)]
    (loop [elem-idx 0
           retval nil
           min-count -1]
      (if (< elem-idx n-elems)
        (let [entry (aget board elem-idx)
              retval (if (and (set? entry)
                              (or (= min-count -1)
                                  (< (count entry) min-count)))
                       [elem-idx entry]
                       retval)
              min-count (long (if retval (count (second retval)) min-count))]
          (recur (inc elem-idx) retval min-count))
        retval))))


(defn search
  [board]
  (let [[item-idx set-items :as min-elem] (minimal-len-set board)]
    (if min-elem
      (->> set-items
           (map #(when-let [board (choose! (duplicate-board board)
                                           (index->yx item-idx)
                                           %)]
                   (search board)))
           (remove nil?)
           first)
      ;;If there are no sets left then the board is solved.
      board)))



(defn solve
  [str-board]
  (search (parse-board str-board)))


(def easy-group "sudoku-easy50.txt")
(def top95-group "sudoku-top95.txt")
(def hardest-group "sudoku-hardest.txt")


(defn solve-all
  [res-file]
  (let [num-solved (->> (-> (slurp (io/resource res-file))
                            (s/split #"\n"))
                        (pmap (fn [item]
                                (let [solved (solve item)]
                                  (when-not solved
                                    (throw (Exception. (str "Failed to solve " item))))
                                  solved)))
                        count)]
    (println "solved" num-solved "puzzles"))
  :ok)


(defn -main
  [& args]
  (println "warming up")
  (solve-all easy-group)
  (println "solving easy group")
  (time (solve-all easy-group))
  (println "solving top95 group")
  (time (solve-all top95-group))
  (println "solving hardest group")
  (time (solve-all hardest-group))
  (println "Solving really hard one...please wait")
  (let [results (time (solve hard1))]
    (display-board results))
  (shutdown-agents))