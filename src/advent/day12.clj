(ns advent.day12
  (:require [advent.util :as util]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "F10
N3
F7
R90
F11")

(def real-input (slurp (io/resource "input12")))

(def input real-input)

(defn rot90 [{:keys [x y]}]
  {:x (- y) :y x})

(defn add-vec [v w]
  (merge-with + v w))

(defn mul-vec [v n]
  (util/map-vals #(* % n) v))

(def moves
  {"N" (fn N [state n] (update-in state [:pos :y] #(+ % n)))
   "S" (fn S [state n] (update-in state [:pos :y] #(- % n)))
   "E" (fn E [state n] (update-in state [:pos :x] #(+ % n)))
   "W" (fn W [state n] (update-in state [:pos :x] #(- % n)))
   "L" (fn W [state n] (nth
                        (iterate #(update % :dir rot90) state)
                        (quot (mod n 360) 90)))
   "R" (fn W [state n] (nth
                        (iterate #(update % :dir rot90) state)
                        (quot (mod (- n) 360) 90)))
   "F" (fn F [{:keys [dir] :as state} n]
         (update state :pos #(add-vec (mul-vec dir n) %)))})

(def move-pat
  (re-pattern
   (str "("
        (str/join "|" (keys moves))
        ")"
        "(\\d+)")))

(->> input
     (re-seq move-pat)
     (map (fn [[_ k v]]
            [k (read-string v)]))
     (reduce
      (fn [state [op arg]]
        ((moves op) state arg))
      {:pos {:x 0 :y 0} :dir {:x 1 :y 0}})
     :pos
     vals
     (map #(Math/abs %))
     (apply +))
;; => 364


;;; part two
