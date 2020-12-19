(ns advent.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def demo-input "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")


(def pat (first (str/split-lines demo-input)))


(defn parse-pattern [s]
  (let [[_ index pattern] (re-matches #"(\d+): (.*)" s)]
    [(read-string index)
     (read-string (str "(" pattern ")"))]))


(defn parse-patterns [pattern-str]
  (into {} (map parse-pattern (str/split-lines pattern-str))))


(defn solvable? [resolved pat]
  (every? resolved (filter number? pat)))


(defn solve [resolved pat]
  (cond
    (= [java.lang.String] (map type pat)) (first pat)
    (every? number? pat) (apply str (map resolved pat))
    :else (let [[a [_ & b]] (split-with (complement #{'|}) pat)]
            (str "("
                 (solve resolved a)
                 "|"
                 (solve resolved b)
                 ")"))))


(defn build-re [patterns]
  (loop [resolved {}
         unresolved patterns]
    (if-let [s (resolved 0)]
      (re-pattern (str "^" s "$"))
      (let [solvable (filter #(solvable? resolved (second %)) unresolved)]
        (recur (into resolved (map
                               (fn [[idx pat]]
                                 [idx (solve resolved pat)])
                               solvable))
               (apply dissoc unresolved (map first solvable)))))))


(defn count-matches [input]
  (let [[patterns strings] (str/split input #"\R\R")
        regex (build-re (parse-patterns patterns))]
    (count (filter (partial re-matches regex) (str/split-lines strings)))))


(count-matches demo-input)

(def real-input (slurp (io/resource "input19")))

(count-matches real-input)
