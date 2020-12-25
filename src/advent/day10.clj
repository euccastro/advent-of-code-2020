(ns advent.day10
  (:require [clojure.java.io :as io]))

(def demo-input-1 "16
10
15
5
1
11
7
19
6
12
4")

(def demo-input-2 "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def real-input (slurp (io/resource "input10")))

(def input real-input)

(->> input
     (re-seq #"\d+")
     (map read-string)
     ((juxt identity (constantly 0) #(+ 3 (apply max %))))
     (apply conj)
     sort
     (partition 2 1)
     (map (fn [[a b]] (- b a)))
     frequencies
     vals
     (apply *))
;; => 1700
