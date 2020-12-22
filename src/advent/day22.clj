(ns advent.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

(def input (slurp (io/resource "input22")))

(def input "Player 1:
43
19

Player 2:
2
29
14")

(defn parse-long [x]
  (Long/parseLong x))

(defn initial-decks [input]
  (->> (str/split input #"\R\R")
       (mapv #(->> %
                   (re-seq #"\d+")
                   rest                 ; skip player number
                   (map parse-long)))))

(defn nonrecursive-turn [[[a & as] [b & bs]]]
  (assert (not= a b))
  (if (< a b)
    [as (concat bs (list b a))]
    [(concat as (list a b)) bs]))

(defn play-nonrecursive-combat [decks]
  (->> decks
       (iterate nonrecursive-turn)
       (drop-while (partial every? seq))
       first))

(defn score [result]
  (->> result
       reverse
       (map * (iterate inc 1))
       (reduce +)))

(defn solution1 [input]
  (->> input
       initial-decks
       play-nonrecursive-combat
       (filter seq)
       first
       score))

(solution1 input)
;; => 33680

(defn recursive-turn [seen
                      [[a & as :as a-deck]
                       [b & bs :as b-deck]
                       :as decks]]
  (cond
    (seen decks)
    [:a a-deck]

    (every? #(>= (first %) (count (rest %))) decks)
    ()
    (recursive-turn seen as bs)
    )
  )
