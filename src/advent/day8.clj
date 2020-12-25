(ns advent.day8
  (:require [miracle.save :as ms]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def real-input (slurp (io/resource "input8")))

(def input demo-input)

(def ops
  {"acc"
   (fn [[a pc] arg]
     [(+ a arg) (inc pc)])
   "jmp"
   (fn [[a pc] arg]
     [a (+ pc arg)])
   "nop"
   (fn [[a pc] _]
     [a (inc pc)])})

(def program
  (->> input
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv #(update % 1 read-string))))

(defn run [program]
  (loop [[a pc :as state] [0 0]
         seen #{}]
    (prn [a pc])
    (cond
      (seen pc) [:loop a]
      (= pc (count program)) [:halt a]
      :else (let [[op arg] (program pc)]
              (prn [op arg])
              (recur ((ops op) state arg)
                     (conj seen pc))))))

(second (run program))
;; => 2051

;;; part 2

(def generate-programs [program]
  )
(comment

  (ms/ld :a)
  )
