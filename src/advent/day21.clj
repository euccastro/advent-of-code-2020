(ns advent.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [miracle.save :as ms]
            [clojure.java.io :as io]))

(def demo-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defn parse-ingredient-line [s]
  (->> s
       (re-matches #"([\w ]+)(?: \(contains ([\w, ]+)\))?" )
       rest
       (map #((fnil re-seq nil "") #"\w+" %))
       (map set)
       #_(zipmap [:ingredients :allergens])))

(defn map-vals [f m]
  (into {}
        (map (fn [[k v]] [k (f v)])
             m)))

(defn parse-input [input]
  (mapv parse-ingredient-line (str/split-lines input)))

(defn solution1 [input]
  (let [i+a (parse-input input)
        all-is (reduce set/union (map first i+a))
        all-as (reduce set/union (map second i+a))
        possible-allergens
        (reduce set/union
                (map (fn [a]
                       (reduce set/intersection
                               (keep
                                (fn [[is as]]
                                  (when (as a) is))
                                i+a)))
                     all-as))
        impossible-allergens
        (set/difference all-is possible-allergens)]
    (count (for [i impossible-allergens
                 [is] i+a
                 :when (is i)]
             1))))

(solution1 demo-input)
;; => 5

(def real-input (slurp (io/resource "input21")))

(solution1 real-input)
;; => 2170

;;; part 2

(defn solution2 [input]
  (let [i+a (parse-input input)
        all-as (reduce set/union (map second i+a))]
    (loop [resolved {}
           a->possible-is
           (into {}
                 (map (fn [a]
                        [a
                         (reduce set/intersection
                                 (keep
                                  (fn [[is as]]
                                    (when (as a) is))
                                  i+a))])
                      all-as))]
      (if (= (count resolved) (count all-as))
        (str/join "," (map resolved (sort (keys resolved))))
        (let [[resolved-a resolved-i]
              (some (fn [[a [i & rest]]]
                      (when (empty? rest)
                        [a i]))
                    a->possible-is)]
          (recur
           (assoc resolved resolved-a resolved-i)
           (-> a->possible-is
               (dissoc resolved-a)
               (->> (map-vals #(disj % resolved-i))))))))))

(solution2 demo-input)
;; => "mxmxvkd,sqjhc,fvjkl"

(solution2 real-input)
;; => "nfnfk,nbgklf,clvr,fttbhdr,qjxxpr,hdsm,sjhds,xchzh"
