(ns advent.day13
  (:require [clojure.java.io :as io]))

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

(* earliest-bus (wait-time earliest-bus))
;; => 3269
