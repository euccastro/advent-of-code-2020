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

(def demo-input2 ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")

(def demo-input3 ".............
.L.L.#.#.#.#.
.............")

(def demo-input4 ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")

(def real-input (slurp (io/resource "input11")))

(def input real-input)

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

(defn solution [step-fn]
  (->> waiting-area
       (util/fixed-point step-fn)
       flatten
       (filter #{\#})
       count))

;(solution step)
;; => 2243


;;; part 2


(def directions
  (remove
   #{[0 0]}
   (combo/cartesian-product
    [-1 0 1]
    [-1 0 1])))

(defn find-seat [wa pos dir]
  (->> pos
       (iterate (partial map + dir))
       rest
       (drop-while #(= (get-in wa (reverse %)) \.))
       first
       reverse
       (get-in wa)
       (= \#)))

(defn occupied-visible-seat-count [wa coords]
  (count
   (filter #(find-seat wa coords %)
           directions)))

(defn step2 [wa]
  (vec
   (map-indexed
    (fn [y row]
      (vec
       (map-indexed
        (fn [x pos]
          (cond
            (and (= pos \L)
                 (zero? (occupied-visible-seat-count wa [x y])))
            \#

            (and (= pos \#)
                 (<= 5 (occupied-visible-seat-count wa [x y])))
            \L

            :else pos))
        row)))
    wa)))

(solution step2)
;; => 2027
