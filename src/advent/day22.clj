(ns advent.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "input22")))

(defn parse-long [x]
  (Long/parseLong x))

(defn initial-decks [input]
  (->> (str/split input #"\R\R")
       (mapv #(->> %
                   (re-seq #"\d+")
                   rest                 ; skip player number
                   (map parse-long)))))

(defn transfer-cards [[[a & as] [b & bs]] a-wins?]
  (if a-wins?
    [(concat as (list a b)) bs]
    [as (concat bs (list b a))]))

(defn nonrecursive-turn [[[a] [b] :as decks]]
  (assert (not= a b))
  (transfer-cards decks (> a b)))

(defn play-nonrecursive-game [decks]
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
       play-nonrecursive-game
       (filter seq)
       first
       score))

(solution1 input)
;; => 33680

(declare play-recursive-game)

(defn recursive-turn [[[a & as]
                       [b & bs] :as decks]]
  (transfer-cards
   decks
   (if (and (<= a (count as)) (<= b (count bs)))
     (= 0 (:winner (play-recursive-game [(take a as) (take b bs)])))
     (> a b))))

(defn play-recursive-game [decks]
  (loop [seen #{}
         [a-deck b-deck :as decks] decks]
    (cond
      (seen decks) {:winner 0
                    :deck (first decks)}
      (empty? a-deck) {:winner 1
                       :deck b-deck}
      (empty? b-deck) {:winner 0
                       :deck a-deck}
      :else (recur (conj seen decks)
                   (recursive-turn decks)))))

(defn solution2 [input]
  (-> input
      initial-decks
      play-recursive-game
      :deck
      score))

(time (solution2 input))
;; => 33683
