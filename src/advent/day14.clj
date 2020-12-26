(ns advent.day14
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [clojure.java.io :as io]))

(def demo-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def demo-input-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(def real-input (slurp (io/resource "input14")))

(def input demo-input-2)

(defn decimal-str->binary-str [s]
  (->> s
       read-string
       (cl-format nil "~36,'0',B")))

(defn mask [s msk]
  (->> s
       (map
        (fn [mbit sbit]
          (if (= mbit \X) sbit mbit))
        msk)
       (apply str)))

(defn eval-decimal [s msk]
  (-> s
      decimal-str->binary-str
      (mask msk)
      (->> (str "2r0")
           read-string)))

(->> input
     str/split-lines
     (reduce
      (fn [state line]
        (if-let [[_ mask] (re-matches #"mask = ([01X]+)" line)]
          (assoc state :mask mask)
          (let [[addr val] (re-seq #"\d+" line)]
            (update state :mem assoc addr (eval-decimal val (:mask state))))))
      {:mem {}})
     :mem
     vals
     (apply +))
;; => 9296748256641
