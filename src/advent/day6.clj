(ns advent.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def demo-input "abc

a
b
c

ab
ac

a
a
a
a

b")

(def real-input (slurp (io/resource "input6")))

(def input real-input)

(defn any-answer-count [group]
  (->> group
       (re-seq #"[a-z]")
       distinct
       count))

(->> (str/split input #"\R\R")
     (map any-answer-count)
     (apply +))
;; => 6885

;;; part 2

(defn all-answer-count [group]
  (->> group
       (str/split-lines)
       (map set)
       (apply set/intersection)
       count))

(->> (str/split input #"\R\R")
     (map all-answer-count)
     (apply +))
;; => 3550
