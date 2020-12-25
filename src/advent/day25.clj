(ns advent.day25
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (-> (io/resource "input25")
      slurp
      str/split-lines
      (->> (map read-string))))

(defn transform-step [subject-number n]
  (rem (* n subject-number) 20201227))

(defn transform [subject-number loop-size]
  (nth
   (iterate (partial transform-step subject-number) 1)
   loop-size))

(defn crack [target-number]
  (first
   (keep-indexed
    (fn [idx n]
      (when (= n target-number)
        idx))
    (iterate (partial transform-step 7) 1))))

(crack 5764801)
;; => 8

(crack 17807724)
;; => 11

(transform (second input) (crack (first input)))
;; => 1478097
