(ns advent.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def real-input (slurp (io/resource "input3")))

(def input real-input)

(def lines (str/split-lines input))

(def map-width (count (first lines)))
(def map-height (count lines))

(defn slope-trees [[dx dy]]
  (let [xs (iterate #(mod (+ dx %) map-width) 0)
        ys (take-while #(< % map-height) (iterate #(+ dy %) 0))]
    (count
     (filter
      #{\#}
      (map (fn [x y]
             (get-in lines [y x]))
           xs
           ys)))))

(slope-trees [3 1])
;; => 230

;;; part two

(def slopes
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

(->> slopes
     (map slope-trees)
     (apply *))
;; => 9533698720
