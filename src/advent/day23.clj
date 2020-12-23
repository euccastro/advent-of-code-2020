(ns advent.day23
  (:require [miracle.save :as ms]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (:import [com.google.common.primitives Ints]))

(def demo-input "389125467")
(def real-input "326519478")

(defn initial-circle1 [input]
  (read-string (str "(" (apply str (interpose " " input)) ")")))

;; correct but naive, would take some 80 days for part 2.
(defn step [[current p0 p1 p2 & remainder]]
  (let [smaller (filter #(< % current) remainder)
        dest (apply max (or (seq smaller) remainder))]
    (concat
     (flatten
      (replace {dest (list dest p0 p1 p2)}
               remainder))
     (list current))))

(defn solution1 [^String input]
  (->> input
       initial-circle1
       (iterate step)
       (drop 100)
       first
       cycle
       (drop-while #(not= % 1))
       rest
       (take (dec (.length input)))
       (apply str)))

(solution1 real-input)
;; => "25368479"

;;; part 2

(def circle-size 1000000)

(defn initial-circle2 [input]
  (let [first-few (initial-circle1 input)]
    (concat first-few (range (inc (apply max first-few))
                             (inc circle-size)))))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn pick-dest [n picks]
  (cond
    (= n 0) (recur circle-size picks)
    (some #{n} picks) (recur (dec n) picks)
    :else n))

(defn step2 [^ints a]
  ;; See `step` for the naive and even less efficient way to do this. At each
  ;; step we shuffle
  ;;
  ;;     current | three-next | ... A ... | dest | ... B ...
  ;; to
  ;;     ... A ... | dest | three-next |... B ... | current
  ;;
  ;; so the new `current` for the next step will again be at the start. This
  ;; reduces the number of special cases we need to handle.
  (let [current (aget a 0)
        picks [(aget a 1) (aget a 2) (aget a 3)]
        dest (pick-dest (dec current) picks)
        dest-idx (Ints/indexOf a dest)
        cs (long circle-size)]
    (System/arraycopy a 4 a 0 (- dest-idx 3))
    (aset-int a (- dest-idx 3) (nth picks 0))
    (aset-int a (- dest-idx 2) (nth picks 1))
    (aset-int a (- dest-idx 1) (nth picks 2))
    (System/arraycopy a (inc dest-idx) a dest-idx (- cs dest-idx 1))
    (aset a (long (dec cs)) current)))

;; takes 33 minutes :P
(time
 (let [array (int-array circle-size (initial-circle2 real-input))
       _ (doseq [_ (range 10000000)]
           (step2 array))
       i1 (Ints/indexOf array 1)]
   (* (aget array (inc i1))
      (aget array (inc (inc i1))))))
;; => 44541319250

(comment
  (let [arr (int-array circle-size (initial-circle2 demo-input))]
    (print
     (tufte/format-pstats
      (second (profiled
               {}
               (time (doseq [_ (range 100000)]
                       (step2 arr))))))))
  )
