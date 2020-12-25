(ns advent.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(def differences
  (->> input
       (re-seq #"\d+")
       (map read-string)
       ((juxt identity (constantly 0) #(+ 3 (apply max %))))
       (apply conj)
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(->> differences
     frequencies
     vals
     (apply *))
;; => 1700

;;; part two

(def one-diff-run-arrangements
  (memoize
   (fn [num-ones]
     (case num-ones
       (0 1) 1
       2 2
       3 4
       (- (* (one-diff-run-arrangements (- num-ones 1))
             2)
        1)))))

(->> differences
     (apply str)
     (#(str/split % #"3"))
     (map count)
     (map one-diff-run-arrangements)
     (apply *))
;; => 12401793332096
