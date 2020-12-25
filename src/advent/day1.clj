(ns advent.day1
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def demo-input "1721
979
366
299
675
1456")

(def real-input (slurp (io/resource "input1")))

(defn solution [input entry-count]
  (let [numbers
        (-> input
            str/split-lines
            (->> (map read-string)))]
    (some
     (fn [entries]
       (when (= (apply + entries) 2020)
         (apply * entries)))
     (apply combo/cartesian-product
            (repeat entry-count numbers)))))

(solution demo-input 2)
;; => 514579

(solution real-input 2)
;; => 633216

;;; part 2


(solution demo-input 3)
;; => 241861950

(solution real-input 3)
;; => 68348924
