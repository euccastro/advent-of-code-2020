(ns advent.day2
  (:require [miracle.save :as ms]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def real-input (slurp (io/resource "input2")))

(defn parse-line [line]
  (let [[_ a b [letter] text]
        (re-matches #"^(\d+)-(\d+) (\w+): (\w+)$"
                    line)
        [a b] (map read-string [a b])]
    [a b letter text]))

(defn line-valid? [line]
  (let [[min max letter text] (parse-line line)]
    (<= min
        (count (filter #{letter} text))
        max)))

(defn solution1 [input]
  (count (filter line-valid? (str/split-lines input))))

(solution1 demo-input)
;; => 2

(solution1 real-input)
;; => 603


;;; part 2

(defn line-valid-2? [line]
  (let [[a b letter text] (parse-line line)]
    (not= (= (nth text (dec a)) letter)
          (= (nth text (dec b)) letter))))

(defn solution2 [input]
  (count (filter line-valid-2? (str/split-lines input))))

(solution2 demo-input)
;; => 1
(solution2 real-input)
;; => 404
