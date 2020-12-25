(ns advent.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "input5")))

(def ->digits {\F \0
               \B \1
               \L \0
               \R \1})

(defn parse-number [s]
  (->> s
       (map ->digits)
       (apply str "2r")))

(def numbers
  (->> input
       str/split-lines
       (map parse-number)
       (map read-string)))

(def max-number
  (apply max numbers))

max-number
;; => 871

(def taken? (set numbers))

(some (fn [n]
        (and
         (taken? (dec n))
         (not (taken? n))
         (taken? (inc n))
         n))
      (range (inc max-number)))
;; => 640
