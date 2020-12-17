(ns advent.day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def demo-input "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")


(defn parse-long [x]
  (Long/parseLong x))


(defn extents [input]
  (for [[_ & extents]
        (re-seq #"(\d+)-(\d+)" input)]
    (mapv parse-long extents)))


(defn nearby-tickets [input]
  (-> input
      (str/split #"nearby tickets:\n")
      second
      (str/split #"\R")
      (->> (map #(str/split % #","))
           (map #(mapv parse-long %)))))


(defn nearby-ticket-numbers [input]
  (flatten (nearby-tickets input)))


(defn valid-number? [n extents]
  (first (for [[begin end] extents
               :when (<= begin n end)]
           n)))


(defn invalid-numbers [input]
  (let [extents (extents input)]
    (remove #(valid-number? % extents) (nearby-ticket-numbers input))))


(defn solution [input]
  (apply + (invalid-numbers input)))


(solution demo-input)
;; => 71


(def real-input (slurp (io/resource "input16")))


(solution real-input)
;; => 21978

;; part 2


(defn attr-extents [input]
  (for [[_ attr & extents]
        (re-seq #"(\w+\s*\w*):\s+(\d+)-(\d+)\s+or\s+(\d+)-(\d+)" input)]
    (apply vector attr (map parse-long extents))))

(comment

  (attr-extents demo-input)

  )

(defn valid-ticket? [numbers extents]
  (every? #(valid-number? % extents) numbers))


(defn valid-nearby-tickets [input]
  (let [extents (extents input)]
    (filter #(valid-ticket? % extents) (nearby-tickets input))))


(comment
  (def valid-tickets* (valid-nearby-tickets demo-input (extents demo-input)))
  (def attr-extents* (attr-extents demo-input))
  (def num-positions (count (first valid-tickets*)))
  )


(defn attr-matches-position? [[_ begin1 end1 begin2 end2] position tickets]
  (every?
   #(or (<= begin1 % end1) (<= begin2 % end2))
   (map #(nth % position) tickets)))


(defn initial-state [aexts tickets]
  (vec
   (for [pos (range (count aexts))]
     (set (keep
           (fn [[attr :as attr-ext]]
             (when (attr-matches-position?
                    attr-ext
                    pos
                    tickets)
               attr))
           aexts)))))


(defn resolve-positions [initial-state]
  (let [num-positions (count initial-state)]
    (loop [state initial-state]
      (let [resolved (map
                      first
                      (filter #(= (count %) 1)
                              state))
            resolved-set (set resolved)]
        (if (= (count resolved) num-positions)
          resolved
          (recur
           (map
            (fn [x]
              (if (= (count x) 1)
                x
                (remove resolved-set x)))
            state)))))))


(def resolved
  (resolve-positions
   (initial-state
    (attr-extents real-input)
    (valid-nearby-tickets real-input))))


(defn my-ticket [input]
  (-> input
      (str/split #"ticket")
      second
      (->> (re-seq #"\d+")
           (map parse-long))))

(def m (zipmap resolved (my-ticket real-input)))


(apply * (vals (select-keys m (filter #(str/starts-with? % "departure") (keys m)))))
;; => 1053686852011
