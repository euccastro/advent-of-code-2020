(ns advent.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [advent.util :as util]))

(def demo-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def demo-input-2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(def real-input (slurp (io/resource "input7")))

(def input real-input)

(defn collect-by-first [xs]
  (->> xs
       (group-by first)
       (map (fn [[k vs]] [k (into #{} (map second vs))]))
       (into {})))

(defn parse-rule [s]
  (let [[k vs] (str/split s #" contain ")]
    [(re-find #"\w+ \w+" k)
     (into []
           (for [item-str (str/split vs #", ")
                 [_ n bag-type] (re-seq #"(\d+) (.*) bag" item-str)]
             [(read-string n) bag-type]))]))

(def contained-in
  (->>
   (for [[container bags] (map parse-rule (str/split-lines input))
         [_ bag] bags]
     [bag container])
   collect-by-first))


(defn expand-transitive-containers [set]
  (apply set/union
         set
         (map contained-in set)))

(defn transitive-containers [x]
  (util/fixed-point
   expand-transitive-containers
   (contained-in x)))

(count (transitive-containers "shiny gold"))
;; => 101

;;; part 2

(def contain
  (->> (str/split-lines input)
       (map parse-rule)
       (into {})))

(def contained-bag-count
  (memoize
   (fn [bag]
     (apply +
            (for [[n contained-bag] (contain bag)]
              (* n (inc (contained-bag-count contained-bag))))))))

(contained-bag-count "shiny gold")
;; => 108636
