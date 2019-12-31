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

(definline fast-next [coll]
  (let [coll (with-meta coll {:tag 'clojure.lang.ISeq})]
    `(.next ~coll)))

(defn fast-aux [pred coll]
  (if-let [s coll]
    (if (pred (fast-first s)) (recur pred (fast-next s))
        false)
    true))

(defn fast-every?
  [pred coll]
  (fast-aux pred (seq coll)))

#_(definline fast-contains? [s val]
  (let [s (with-meta s {:tag 'clojure.lang.IPersistentSet})]
    `(.contains ~s ~val)))

(definline index->yx
  [elem-idx]
  `(let [elem-idx# (long ~elem-idx)]
     [(quot elem-idx# 9) (rem elem-idx# 9)]))

;;6-7x faster if we don't have to unpack....
(definline rc->index [r c]
  `(+ (* 9 (long ~r)) (long ~c)))

;;could be much faster to work on longs directly, vs
;;a vector...
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

;;3x faster than memo-2 if we have a finite domain of keys and can
;;avoid hashing.
(defn memo-2-row-col [f rows cols]
  (let [arr (object-array (* (long rows) (long cols)))
        _   (doseq [i (range rows)
                    j (range cols)]
              (aset arr (rc->index i j) (f i j)))]
    (fn [^long y ^long x]
      (aget arr ^long (rc->index y x)))))

;;we can probably store this even better in an object array.
;;Rather than using a hashtable...precompute all of these
;;into an array lookup table.
#_(def get-affected-indexes
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
           elem-idx (yx->index [y x])
           all-indexes (c-set/union row-indexes
                                    col-indexes
                                    unit-indexes)]
       ;;We take advantage of the fact that these are long arrays
       {:units [row-indexes
                col-indexes
                unit-indexes]
        :affected-indexes (long-array all-indexes)
        :peers (long-array (fast-set-disj all-indexes elem-idx))}))))

;;not much of a difference.
(def get-affected-indexes
  (memo-2-row-col (fn [y x]
                    (let [row-indexes (->> (row-indexes y)
                                           (tensor->elem-seq)
                                           set)
                          col-indexes (->> (column-indexes x)
                                           (tensor->elem-seq)
                                           set)
                          unit-indexes (->> (unit-indexes y x)
                                            (tensor->elem-seq)
                                            set)
                          elem-idx (yx->index [y x])
                          all-indexes (c-set/union row-indexes
                                                   col-indexes
                                                   unit-indexes)]
                      ;;We take advantage of the fact that these are long arrays
                      {:units [row-indexes
                               col-indexes
                               unit-indexes]
                       :affected-indexes (long-array (sort all-indexes))
                       :peers (long-array (fast-set-disj all-indexes elem-idx))})) 9 9))

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
       (entry value))))

(defn- value-possible-in-unit?
  [^"[Ljava.lang.Object;" board ^long value unit-set]
  (some #(entry-set-contains-value? board value %) unit-set))

(defn- places-for-value
  [^"[Ljava.lang.Object;" board ^long value unit]
  (filterv #(let [entry (aget board (int %))]
              (and (set? entry)
                    (entry value)))
           unit))

(declare eliminate!)

;;other-values returns a longset.
;;we then invoke fast-every? on it, which uses naive
;;first/next seq recursion.
;;seq implementation of longset uses filter on a range..
;; (seq [this]
;;      (when-not (= 0 set)
;;        (->> (range 0 63)
;;             (filter #(.contains this %)))))
;;Possible bottleneck...we can probably do faster by coll-reduce.
;;We're looking at <= 9 values though, so maybe not a big deal?

(defn assign!
  "Eliminate all the other values (except d) from values[s] and propagate.
    Return values, except return False if a contradiction is detected."
  [^"[Ljava.lang.Object;" board yx-tuple value]
  (let [other-values (fast-set-disj (aget board (yx->index yx-tuple)) value)]
    (when-not (= 0 (count other-values))
      (println "assign!" (yx->index yx-tuple) value (count other-values)))
    (when (fast-every? #(eliminate! board yx-tuple %) other-values)
      board)))


;;lol, counterintuively, this is faster for traversal over vectors than
;;reduce....
(defn ireduce [f init v]
  (let [^java.util.Iterator i  (.iterator ^Iterable v)]
    (loop [acc init]
      (if (.hasNext i)
        (let [res (f acc (.next i))]
          (if (reduced? res) @res
              (recur res)))
        acc))))

;;this is where propogation comes in....
(defn eliminate!
  "Eliminate d from values[s]; propagate when values or places <= 2.
    Return values, except return False if a contradiction is detected."
  [^"[Ljava.lang.Object;" board yx-tuple value]
  (let [idx   (yx->index yx-tuple)
        entry (aget board idx)]
    (if (not  (entry value))
      board
      (let [new-values (fast-set-disj entry value) ;;disj a longset.
            n-vals (count new-values)
            _      (aset board idx new-values)
            board (cond
                    (= 0 n-vals)
                    (do
                      (println "constraint violation" idx value new-values)
                      nil)
                    (= 1 n-vals)
                    (let [set-val (fast-first new-values)]
                      (when (fast-every? #(eliminate! board (index->yx %) set-val)
                                    (peers yx-tuple))
                        board))
                    :else
                    board)]
        (when board
          (ireduce (fn [board unit]
                    (when board
                      (let [dplaces (places-for-value board value unit)
                            n-dplaces (fast-count dplaces)]
                        (case n-dplaces
                          0
                          ;;constraint-violation
                          (do
                            (println "0pos constraint violation" idx value new-values)
                            nil)
                          1
                          (do
                            (assign! board (index->yx (fast-first dplaces)) value))
                          board))))
                  board
                  (units yx-tuple)))))))


(def grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")

(def impossible ".....5.8....6.1.43..........1.5........1.6...3.......553.....61........4.........")


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
  (fast-every? number? (tensor->elem-seq board)))


(defn- minimal-len-set
  [board]
  (let [^"[Ljava.lang.Object;" board board
        n-elems (dtype/ecount board)]
    (loop [elem-idx 0
           retval nil
           min-count -1]
      (if (< elem-idx n-elems)
        (let [entry (aget board elem-idx)
              ecount (fast-count entry)
              retval (if (and (> ecount 1)
                              (or (= min-count -1)
                                  (and (< ecount min-count))))
                       [elem-idx entry]
                       retval)
              min-count (long (if retval (fast-count (second retval)) min-count))]
          (recur (inc elem-idx) retval min-count))
        retval))))


(defn search
  [board]
  (cond
    (fast-every? #(= 1 (fast-count %)) board)
    board
    (nil? board)
    board
    :else
    (let [[item-idx set-items :as min-elem] (minimal-len-set board)]
      (->> set-items
           (map #(do
                   (println "assigning!" item-idx %)
                   (when (and (= item-idx 3)
                              (= % 3))
                     (display-board board false))
                   (when-let [board (assign! (duplicate-board board)
                                             (index->yx item-idx)
                                             %)]
                     (search board))))
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

(defn run-them []
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
    (display-board results)))
