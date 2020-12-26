(ns advent.day14
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def demo-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def demo-input-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(def real-input (slurp (io/resource "input14")))

(def input real-input)

(defn decimal-str->binary-str [s]
  (->> s
       read-string
       (cl-format nil "~36,'0',B")))

(defn mask-val [s msk]
  (->> s
       (map
        (fn [mbit sbit]
          (if (= mbit \X) sbit mbit))
        msk)
       (apply str)))

(defn eval-decimal [s mask]
  (-> s
      decimal-str->binary-str
      (mask-val mask)
      (->> (str "2r0")
           read-string)))

(defn solution [input mem-update-fn]
  (->> input
       str/split-lines
       (reduce
        (fn [state line]
          (if-let [[_ mask] (re-matches #"mask = ([01X]+)" line)]
            (assoc state :mask mask)
            (let [[addr val] (re-seq #"\d+" line)]
              (mem-update-fn state addr val))))
        {:mem {}})
       :mem
       vals
       (apply +)))

(defn update-mem-1 [state addr val]
  (update state :mem assoc addr (eval-decimal val (:mask state))))

(solution input update-mem-1)
;; => 9296748256641

;;; part 2

(defn mask-addr [s mask]
  (->> s
       decimal-str->binary-str
       (map
        (fn [mbit sbit]
          (case mbit
            \0 [sbit]
            \1 [1]
            \X [0 1]))
        mask)
       (apply combo/cartesian-product)
       (map (partial apply str))))

(defn update-mem-2 [{:keys [mask] :as state} addr val]
  (->> (mask-addr addr mask)
       (map (fn [addr'] [addr' (read-string val)]))
       (into {})
       (update state :mem merge)))

(time (solution input update-mem-2))
;; => 4877695371685
