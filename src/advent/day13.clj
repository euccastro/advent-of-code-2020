(ns advent.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent.util :as util]))

(def demo-input "939
7,13,x,x,59,x,31,19")

(def real-input (slurp (io/resource "input13")))

(def input real-input)

(let [[ed & xs] (map read-string (re-seq #"\d+" input))]
  (def earliest-departure ed)
  (def buses xs))

(defn wait-time [bus]
  (- bus (mod earliest-departure bus)))

(def earliest-bus (apply min-key wait-time buses))

(-> input
    str/split-lines
    second
    (str/split #",")
    (->> (keep-indexed
          (fn [i v]
            (when (not= v "x")
              [i (read-string v)])))))


;;; part 2, correct but too slow version
;; {bus-id -> remainder}
(def bus-departures
  (-> input
      str/split-lines
      second
      (str/split #",")
      (->> (keep-indexed
            (fn [i v]
              (when (not= v "x")
                (let [bus (read-string v)]
                  [bus (mod (- bus i) bus)]))))
           (into {}))))

(def least-frequent-bus (apply max buses))

(def other-departures (dissoc bus-departures least-frequent-bus))


#_
(loop [n (bus-departures least-frequent-bus)]
  (if (every?
       (fn [[bus remainder]]
         (= (mod n bus) remainder))
       other-departures)
    n
    (recur (+ n least-frequent-bus))))

;; I had already seen this spoiler, and the fact that bus IDs are pairwise
;; coprime:
;;
;; https://en.wikipedia.org/wiki/Chinese_remainder_theorem
;;
;; So I'm using this algorithm:
;;
;; https://brilliant.org/wiki/chinese-remainder-theorem/

(def N (apply * buses))
(def ys (map #(quot N %) buses))
(def zs (map util/mul-inv ys buses))

(mod
 (apply
  +
  (map
   (fn [n y z]
     (* (bus-departures n) y z))
   buses
   ys
   zs))
 N)
;; => 672754131923874
