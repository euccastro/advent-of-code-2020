(ns advent.util)

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
