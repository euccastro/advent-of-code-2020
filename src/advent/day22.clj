(ns advent.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

(def input (slurp (io/resource "input22")))

(defn parse-long [x]
  (Long/parseLong x))

(defn initial-decks [input]
  (->> (str/split input #"\R\R")
       (mapv #(->> %
                   (re-seq #"\d+")
                   rest                 ; skip player number
                   (map parse-long)))))

(defn turn [decks]
  (let [[[a & as] [b & bs]] decks]
    (assert (not= a b))
    (if (< a b)
      [as (concat bs (list b a))]
      [(concat as (list a b)) bs])))

(defn solution1 [input]
  (->> input
       initial-decks
       (iterate turn)
       (drop-while (partial every? seq))
       first
       (filter seq)
       first
       reverse
       (map * (iterate inc 1))
       (reduce +)))

(solution1 input)
;; => 33680
