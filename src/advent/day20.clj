(ns advent.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input (slurp (io/resource "demo20")))
(def real-input (slurp (io/resource "input20")))


;;; In part 1 I don't bother matching the tiles; I just look for tiles
;;; with 2 unique borders.

(defn tiles [input]
  (str/split input #"\R\R"))

(defn tile-number [tile]
  (Long. (re-find #"\d+" tile)))

(defn normalize-border [line]
  (->> line
       ((juxt identity #(apply str (reverse %))))
       sort
       first))

(defn borders [tile-lines]
  (->> tile-lines
       ((juxt first
              last
              #(apply str (map first %))
              #(apply str (map last %))))
       (map normalize-border)))

(defn border->tiles [input]
  (group-by first
            (for [t (tiles input)
                  :let [n (tile-number t)]
                  b (borders (rest (str/split-lines t)))]
              [b n])))

(defn solution1 [input]
  (->> input
       border->tiles
       (filter #(= 1 (count (second %))))
       (map (fn [[_ [[_ x]]]] x))
       frequencies
       (keep (fn [[tile unique-border-count]]
               (when (= 2 unique-border-count)
                 tile)))
       (apply *)))

(solution1 demo-input)
;; => 20899048083289

(solution1 real-input)
;; => 7901522557967


;;; Alas, this solution seems pretty useless for part two.


(comment

  (borders ["abc" "def" "ghi"])
  (normalize-border "cyabc")
  (tile-number (first (tiles demo-input)))
  (borders (rest (str/split-lines (first (tiles demo-input)))))



  (keep (fn [[tile unique-border-count]]
          (when (= 2 unique-border-count)
            tile))
        (frequencies
         (map (fn [[_ [[_ x]]]] x)
              (filter #(= 1 (count (second %)))
                      (border->tiles demo-input)))))
  )
