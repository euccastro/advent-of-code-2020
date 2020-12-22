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
   (if (every? #(<= (first %) (count (rest %))) decks)
     (= 0 (:winner (play-recursive-game [(take a as) (take b bs)])))
     (> a b))))

(def play-recursive-game
  (memoize
   (fn [decks]
     (loop [seen #{}
            decks decks]
       (cond
         (seen decks) {:winner 0
                       :deck (first decks)}
         (some empty? decks) (first
                              (keep-indexed
                               (fn [i deck]
                                 (when (seq deck)
                                   {:winner i
                                    :deck deck}))
                               decks))
         :else (recur (conj seen decks)
                      (recursive-turn decks)))))))

(defn solution2 [input]
  (-> input
      initial-decks
      play-recursive-game
      :deck
      score))

(def decks (initial-decks input))

(solution2 input)
;; => 33683
