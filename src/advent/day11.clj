(ns advent.day11
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent.util :as util]
            [clojure.java.io :as io]))

(def demo-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def real-input (slurp (io/resource "input11")))

(def input demo-input)

(def waiting-area
  (->> input
       str/split-lines
       (mapv vec)))

(def wa-width (count (first waiting-area)))
(def wa-height (count waiting-area))

(defn occupied-neighbor-count [wa coords]
  (count
   (filter
    (fn [[x y]]
      (= \# (get-in wa [y x])))
    (remove #{coords}
            (apply combo/cartesian-product
                   (map (juxt dec identity inc) coords))))))

(defn step [wa]
  (vec
   (map-indexed
    (fn [y row]
      (vec
       (map-indexed
        (fn [x pos]
          (cond
            (and (= pos \L)
                 (zero? (occupied-neighbor-count wa [x y])))
            \#

            (and (= pos \#)
                 (<= 4 (occupied-neighbor-count wa [x y])))
            \L
            :else pos))
        row)))
    wa)))

(->> waiting-area
     (util/fixed-point step)
     flatten
     (filter #{\#})
     count)
;; => 2243
