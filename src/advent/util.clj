(ns advent.util
  (:require
   [clojure.math.numeric-tower :as math]) )

(defn map-vals [f m]
  (into {}
        (map (fn [[k v]] [k (f v)])
             m)))

(defn fixed-point [f start]
  (->> start
       (iterate f)
       (partition 2 1)
       (some (fn [[prev this]]
               (when (= prev this) this)))))

;; Taken from https://rosettacode.org/wiki/Modular_inverse#Clojure
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul-inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (if (= (first egcd) 1)
        (mod (second egcd) b)
        (str "No inverse since gcd is: " (first egcd)))))
