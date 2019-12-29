(ns sudoku.core
  (:require [tech.v2.tensor :as dtt]
            [tech.v2.datatype :as dtype]
            [clojure.set :as c-set]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [sudoku.longset :refer [longset longset?]])
  (:import [clojure.lang IPersistentCollection])
  (:gen-class))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn board->str
  "Rendering the board.  Convert board to strings, keep
  track of the longest column and render board as a row-major
  2d plane."
  [board display-sets?]
  (let [board (map (fn [item]
                     (cond
                       (number? item)
                       (str " " item)
                       (nil? item)
                       " -"
                       :else
                       (cond
                         (= 1 (count item))
                         (str " " (first item))
                         display-sets?
                         (apply str " {" (concat item ["}"]))
                         :else
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


(def digits (apply longset (range 1 10)))

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


(defn tensor->elem-seq
  "To convert to tensor to a sequence of its elements the fastest way is just to
  make a reader out of it.  If you naively map over a 2d tensor you will get
  a sequence of rows."
  [tens]
  (dtype/->reader tens))


;;Computing which indexes are affected by any move
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


(definline fast-first [x]
  `(.first ^clojure.lang.ISeq (seq ~x)))

(definline fast-count [s]
  (let [s (with-meta s {:tag 'clojure.lang.Counted})]
    `(.count  ~s)))

(definline fast-nth [s idx]
  (let [s (with-meta s {:tag 'clojure.lang.Indexed})]
    `(.nth ~s ~idx)))

(definline fast-set-disj [s val]
  (let [s (with-meta s {:tag 'clojure.lang.IPersistentSet})]
    `(.disjoin ~s ~val)))


(definline index->yx
  [elem-idx]
  `(let [elem-idx# (long ~elem-idx)]
     [(quot elem-idx# 9) (rem elem-idx# 9)]))


(definline yx->index
  [items]
  `(let [^java.util.List items# ~items
         y# (long (.get items# 0))
         x# (long (.get items# 1))]
     (+ (* 9 y#) x#)))


;;4x faster than clojure.core/memoize...
;;we can do better with a macro, but I haven't sussed it out.
;;This is a much as we probably need for now though, meh.
(defn memo-2 [f]
  (let [xs (java.util.concurrent.ConcurrentHashMap.)]
    (fn [x y]
      (if-let [^java.util.concurrent.ConcurrentHashMap ys (.get xs x)]
        (if-let [res (.get ys y)]
          res
          (let [res (f x y)]
            (do (.putIfAbsent ys y res)
                res)))
        (let [res     (f x y)
              ys    (doto (java.util.concurrent.ConcurrentHashMap.)
                      (.putIfAbsent y res))
              _     (.putIfAbsent xs x ys)]
          res)))))


(def get-affected-indexes
  (memo-2
   (fn [y x]
     (let [row-indexes (->> (row-indexes y)
                            (tensor->elem-seq)
                            set)
           col-indexes (->> (column-indexes x)
                            (tensor->elem-seq)
                            set)
           unit-indexes (->> (unit-indexes y x)
                             (tensor->elem-seq)
                             set)
           elem-idx (yx->index [y x])]
       ;;We take advantage of the fact that these are long arrays
       {:units [row-indexes
                col-indexes
                unit-indexes]
        :affected-indexes
        (long-array
         (c-set/union row-indexes col-indexes unit-indexes))
        :peers
        (long-array (fast-set-disj (c-set/union row-indexes
                                                col-indexes
                                                unit-indexes)
                                   elem-idx))}))))


(defn peers
  [[y x :as yx-tuple]]
  (:peers (get-affected-indexes y x)))


(defn units
  [[y x :as yx-tuple]]
  (:units (get-affected-indexes y x)))


(defn- entry-set-contains-value?
  [^"[Ljava.lang.Object;" board ^long value elem-idx]
  (let [entry (aget board elem-idx)]
    (when (set? entry)
      (contains? entry value))))

(defn- value-possible-in-unit?
  [^"[Ljava.lang.Object;" board ^long value unit-set]
  (some #(entry-set-contains-value? board value %) unit-set))

(defn- places-for-value
  [^"[Ljava.lang.Object;" board ^long value unit]
  (filterv #(let [entry (aget board (int %))]
              (and (set? entry)
                   (contains? entry value)))
           unit))

(declare eliminate!)

(defn assign!
  "Eliminate all the other values (except d) from values[s] and propagate.
    Return values, except return False if a contradiction is detected."
  [^"[Ljava.lang.Object;" board yx-tuple value]
  (let [other-values (fast-set-disj (aget board (yx->index yx-tuple)) value)]
    (when (every? #(eliminate! board yx-tuple %) other-values)
      board)))


(defn eliminate!
  "Eliminate d from values[s]; propagate when values or places <= 2.
    Return values, except return False if a contradiction is detected."
  [^"[Ljava.lang.Object;" board yx-tuple value]
  (let [entry (aget board (yx->index yx-tuple))]
    (if (not (contains? entry value))
      board
      (let [new-values (fast-set-disj entry value)
            n-vals (count new-values)
            _ (aset board (yx->index yx-tuple) new-values)
            board (cond
                    (= 0 n-vals)
                    nil
                    (= 1 n-vals)
                    (let [set-val (fast-first new-values)]
                      (when (every? #(eliminate! board (index->yx %) set-val)
                                    (peers yx-tuple))
                        board))
                    :else
                    board)]
        (when board
          (reduce (fn [board unit]
                    (when board
                      (let [dplaces (places-for-value board value unit)
                            n-dplaces (fast-count dplaces)]
                        (case n-dplaces
                          0
                          ;;constraint-violation
                          nil
                          1
                          (assign! board (index->yx (fast-first dplaces)) value)
                          board))))
                  board
                  (units yx-tuple)))))))


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
    (reduce (fn [new-board [item-idx item-val]]
              (assign! new-board (index->yx item-idx) item-val))
            (duplicate-board empty-board)
            parsed-board)))


(defn solved?
  [board]
  (every? number? (tensor->elem-seq board)))


(defn- minimal-len-set
  [board]
  (let [^"[Ljava.lang.Object;" board board
        n-elems (dtype/ecount board)]
    (loop [elem-idx 0
           retval nil
           min-count -1]
      (if (< elem-idx n-elems)
        (let [entry (aget board elem-idx)
              retval (if (and (> (count entry) 1)
                              (or (= min-count -1)
                                  (and (< (count entry) min-count))))
                       [elem-idx entry]
                       retval)
              min-count (long (if retval (count (second retval)) min-count))]
          (recur (inc elem-idx) retval min-count))
        retval))))


(defn search
  [board]
  (cond
    (every? #(= 1 (count %)) board)
    board
    (nil? board)
    board
    :else
    (let [[item-idx set-items :as min-elem] (minimal-len-set board)]
      (->> set-items
           (map #(when-let [board (assign! (duplicate-board board)
                                           (index->yx item-idx)
                                           %)]
                   (search board)))
           (remove nil?)
           first))))



(defn solve
  [str-board]
  (let [board (parse-board str-board)]
    (search board)))


(def easy-group "sudoku-easy50.txt")
(def top95-group "sudoku-top95.txt")
(def hardest-group "sudoku-hardest.txt")


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
