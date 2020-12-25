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

(some
 (fn [[n & window]]
   (when (not-any?
          #(= n (apply + %))
          (combo/combinations window 2))
     n))
 (partition (inc preamble-len) 1 numbers))
;; => 14360655
