(ns advent.day15)

(def demo-input "0,3,6")

(def real-input "15,5,1,4,7,0")

(def input real-input)

(def input-numbers
  (->> input
       (re-seq #"\d+")
       (mapv read-string)))

(def state
  {:turn (count input-numbers)
   :spoken (last input-numbers)
   :seen (->> (butlast input-numbers)
              (map vector (rest (range)))
              (mapv (comp vec reverse))
              (into {}))})

(defn step [{:keys [turn spoken seen]}]
  {:turn (inc turn)
   :spoken (if-let [last-seen (seen spoken)]
                  (- turn last-seen)
                  0)
   :seen (assoc seen spoken turn)})

(defn solution [turns]
  (->>
   (iterate step state)
   (drop-while #(< (:turn %) turns))
   first
   :spoken))

(solution 2020)

(solution 30000000)
