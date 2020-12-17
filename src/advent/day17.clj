(ns advent.day17
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


(def demo-input
".#.
..#
###")


;; sparse grid representation: set of [x y z] active cubes.
(defn initial-state [input]
  (into #{}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)
              :when (= char \#)]
          [x y 0])))

(comment
  (initial-state demo-input)
  )


(defn cell-neighbors [[x y z]]
  (for [x' [(dec x) x (inc x)]
        y' [(dec y) y (inc y)]
        z' [(dec z) z (inc z)]
        :when (not= [x' y' z'] [x y z])]
    [x' y' z']))

(comment
  (cell-neighbors [1 1 1])
  (count (cell-neighbors [0 0 0]))
  )


(defn neighbor-counts [state]
  (frequencies (mapcat cell-neighbors state)))

(comment
  (neighbor-counts (initial-state demo-input)))


(defn cycle [state]
  (let [ncs (neighbor-counts state)]
    (set
     (concat
      (filter #(#{2 3} (get ncs % 0)) state)
      (keep (fn [[cell n]]
              (when (and (not (state cell))
                         (= n 3))
                cell))
            ncs)))))

(comment
  (def state (initial-state demo-input))
  (def ncs (neighbor-counts state))
  (state [1 3 1])
  (cycle state)
  (ns-unmap *ns* 'state)
  )

(defn cells-after-6-cycles [input]
  (->> input
       initial-state
       (iterate cycle)
       (drop 6)
       first
       count))

(time (cells-after-6-cycles demo-input))
;; => 112

(def real-input (slurp (io/resource "input17")))


(time (cells-after-6-cycles real-input))
;; => 207


;;; second part; generalize the functions where we assume 3 dimensions

(defn initial-state
  [dimensions input]
  (into #{}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)
              :when (= char \#)]
          (into [x y] (repeat (- dimensions 2) 0)))))

(comment
  (initial-state 4 demo-input)
  )


(defn cell-neighbors [cell]
  (remove #{cell}
          (apply combo/cartesian-product
                 (for [coord cell] [(dec coord) coord (inc coord)]))))

(comment

  (def cell [0 0 0 0])
  (cell-neighbors cell)
  (count (cell-neighbors cell))
  (ns-unmap *ns* 'cell)
  )

(defn cells-after-6-cycles [dimensions input]
  (->> input
       (initial-state dimensions)
       (iterate cycle)
       (drop 6)
       first
       count))


(time (cells-after-6-cycles 3 demo-input))
;; => 112


(time (cells-after-6-cycles 4 demo-input))
;; => 848


(time (cells-after-6-cycles 4 real-input))
;; => 2308
