(ns advent.day9
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(def demo-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def real-input (slurp (io/resource "input9")))

(def input real-input)

(def preamble-len 25)

(def numbers (reverse (map read-string (re-seq #"\d+" input))))

(def invalid-number
  (some
   (fn [[n & window]]
     (when (not-any?
            #(= n (apply + %))
            (combo/combinations window 2))
       n))
   (partition (inc preamble-len) 1 numbers)))

invalid-number
;; => 14360655

(some
 (fn [nums]
   (some
    (fn [[slice sum]]
      (when (and (<= 2 (count slice))
                 (= sum invalid-number))
        (+ (apply min slice)
           (apply max slice))))
    (reductions (fn [[v acc] n]
                  (let [new-acc (+ acc n)]
                    (if (> new-acc invalid-number)
                      (reduced nil)
                      [(conj v n) new-acc])))
                [[] 0]
                nums)))
 (iterate rest numbers))
;; => 1962331
