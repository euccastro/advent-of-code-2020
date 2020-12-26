(ns advent.day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;;; Look ma, no instaparse!

(def demo-simple-input "1 + 2 * 3 + 4 * 5 + 6")

(def demo-nested-input "1 + (2 * 3) + (4 * (5 + 6))")

(def real-input (slurp (io/resource "input18")))

(defn parse-long [x]
  (Long/parseLong x))


(def operators {"+" + "*" *})


(defn eval-simple-expression [s]
  (let [first-term (re-find #"\d+" s)
        ops (re-seq #"\s+([+*])\s+(\d+)" (subs s (.length first-term)))]
    (reduce
     (fn [accum [_ operator operand]]
       ((operators operator) accum (parse-long operand)))
     (parse-long first-term)
     ops)))


(defn leaf-paren-expr [s]
  (re-find #"\([^\(\)]+\)" s))


(defn eval [s]
  (loop [s s]
    (if-let [lpe (leaf-paren-expr s)]
      (recur (str/replace
              s
              lpe
              (str (eval-simple-expression
                    (subs lpe 1 (dec (.length lpe)))))))
      (eval-simple-expression s))))

(time (apply + (map eval (str/split-lines real-input))))
;; => 5019432542701


;;; part two


(defn eval-non-mul [s]
  (apply + (map parse-long (str/split s #" \+ "))))


(defn eval-simple-expression [s]
  (apply * (map eval-non-mul (str/split s #" \* "))))


(time (apply + (map eval (str/split-lines real-input))))
;; => 70518821989947
