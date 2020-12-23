(ns advent.day23
  (:require [miracle.save :as ms]))

(def demo-input "389125467")
(def real-input "326519478")

(defn initial-circle1 [input]
  (read-string (str "(" (apply str (interpose " " input)) ")")))

(defn step [[current p0 p1 p2 & remainder]]
  (let [smaller (filter #(< % current) remainder)
        dest (apply max (or (seq smaller) remainder))]
    (concat
     (flatten
      (replace {dest (list dest p0 p1 p2)}
               remainder))
     (list current))))

(defn solution1 [input]
  (->> input
       initial-circle1
       (iterate step)
       (drop 100)
       first
       cycle
       (drop-while #(not= % 1))
       rest
       (take (dec (.length input)))
       (apply str)))

(solution1 real-input)
;; => "25368479"

(defn initial-circle2 [input]
  (let [first-few (initial-circle1 input)]
    (concat first-few (range (inc (apply max first-few))
                             1000001))))

(defn solution2 [input]
  (->> input
       initial-circle2
       (iterate step)
       (drop 10)
       first
       cycle
       (drop-while #(not= % 1))
       (take 2)
       (apply *)))


(time (solution2 demo-input))
