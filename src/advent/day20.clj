(ns advent.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input (slurp (io/resource "demo20")))
(def real-input (slurp (io/resource "input20")))

(defn map-vals [f m]
  (into {}
        (map (fn [[k v]] [k (f v)])
             m)))

(defn tile-id->tile-lines [input]
  (->> (str/split input #"\R\R")
       (map str/split-lines)
       (map (fn [[l & ls]]
              [(Long. (re-find #"\d+" l))
               (vec ls)]))
       (into {})))

(defn reverse-line [l]
  (apply str (reverse l)))

(defn normalize-border
  "Borders are unique even considering reversal. This function picks one
  canonical representation for a border so we can more easily match possibly
  flipped borders across tiles."
  [line]
  (->> line
       ((juxt identity reverse-line))
       sort
       first))

(def top-border first)

(def bottom-border last)

(defn left-border [lines]
  (apply str (map first lines)))

(defn right-border [lines]
  (apply str (map last lines)))

(defn borders [tile-lines]
  (->> tile-lines
       ((juxt left-border
              top-border
              right-border
              bottom-border))
       (mapv normalize-border)))

(defn tile-id->borders
  "takes the result of tile-id->tile-lines"
  [m]
  (map-vals borders m))

(defn border->tile-ids
  "takes the result of tile-id->borders"
  [m]
  (->> (for [[tid bs] m
             b bs]
         [b tid])
       (group-by first)                    ; => {b [[b tid] ...]}
       (map-vals (partial map second))))   ; => {b [tid ...]}

(defn tile-id->unique-borders
  "takes the result of tile-id->borders"
  [tile-id->borders]
  (let [unshared-borders
        (set (for [[border tiles] (border->tile-ids tile-id->borders)
                   :when (= 1 (count tiles))]
               border))]
    (map-vals (partial filter unshared-borders) tile-id->borders)))

(defn corner-tiles
  "takes the result of tile-id->unique-borders"
  [m]
  (->> m
       (map-vals count)
       (keep (fn [[tile unique-border-count]]
               (when (= 2 unique-border-count)
                 tile)))))

(defn solution1 [input]
  (->> input
       tile-id->tile-lines
       tile-id->borders
       tile-id->unique-borders
       corner-tiles
       (apply *)))

(solution1 demo-input)
;; => 20899048083289

(solution1 real-input)
;; => 7901522557967

(defn fliph [lines]
  (vec (reverse lines)))

(defn flipv [lines]
  (mapv reverse-line lines))

(defn transpose [lines]
  (vec (apply (partial map str) lines)))

(def variations
  ;; XXX: we could obviously avoid repeating some ops here
  (juxt
   identity
   fliph
   flipv
   transpose
   (comp fliph flipv)
   (comp fliph transpose)
   (comp flipv transpose)
   (comp fliph flipv transpose)))

(defn arrange-tile
  "return a (possibly rotated or flipped) version of tile that matches the given
  left and top borders"
  [lines left top]
  (some #(and (= (left-border %) left)
              (= (top-border %) top)
              %)
        (variations lines)))

(def monster-lines (str/split-lines "                  #
#    ##    ##    ###
 #  #  #  #  #  #   "))

(def monster-width (count (first monster-lines)))

(def monster-height (count monster-lines))

(def monster-kernel
  (for [[y line] (map-indexed vector monster-lines)
        [x char] (map-indexed vector line)
        :when (= char \#)]
    [x y]))

(def monster-dash-count (count monster-kernel))

(defn scan [map-lines]
  (let [map-width (count (first map-lines))
        map-height (count map-lines)]
    (seq
     (for [scan-x (range (inc (- map-width monster-width)))
           scan-y (range (inc (- map-height monster-height)))
           :when (every?
                  (fn [[x y]]
                    (= (get-in map-lines [(+ scan-y y) (+ scan-x x)]) \#))
                  monster-kernel)]
       [scan-x scan-y]))))

(defn remove-borders [lines]
  (let [end (dec (count (first lines)))]
    (-> (mapv #(subs % 1 end) lines)
        (subvec 1 end))))

(defn solution2 [input]
  ;; Pick any corner, transform it to become the top left corner, then add all
  ;; other tiles one line at a time.
  ;;
  ;; We can do this in one pass because borders are unique, so once we place any
  ;; tile the identity of its neighbors is completely determined.
  ;;
  ;; To find the right transformation of each tile, we take advantage of the
  ;; fact that we have already placed its top neighbor and/or its left neighbor.
  ;; If we're missing a neighbor (e.g., in the first row, or in the first column
  ;; of subsequent rows) we just use the tile's unique border to match against,
  ;; trying both orientations.
  (let [tid->lines (tile-id->tile-lines input)
        width (long (Math/sqrt (count tid->lines)))
        tid->borders (tile-id->borders tid->lines)
        border->tids (border->tile-ids tid->borders)
        tid->unique-borders (tile-id->unique-borders tid->borders)
        placed (let [tid (first (corner-tiles tid->unique-borders))
                     [left top] (tid->unique-borders tid)
                     lines (tid->lines tid)
                     arr-lines (or (arrange-tile lines left top)
                                   (arrange-tile lines left (reverse-line top))
                                   (arrange-tile lines (reverse-line left) top)
                                   (arrange-tile lines (reverse-line left) (reverse-line top)))]
                 {[0 0] {:tid tid :lines arr-lines}})
        place-tile (fn place-tile [placed [x y]]
                     (let [[neighbor border-fn]
                           (if (zero? x)
                             [(placed [x (dec y)]) bottom-border]
                             [(placed [(dec x) y]) right-border])
                           [tid] (remove #{(:tid neighbor)}
                                         (border->tids (normalize-border (border-fn (:lines neighbor)))))
                           unique-border-variations (mapcat (juxt identity reverse-line) (tid->unique-borders tid))]
                       (assoc placed [x y]
                              {:tid tid
                               :lines (some
                                       identity
                                       (for [left (if-let [{:keys [lines]} (placed [(dec x) y])]
                                                    [(right-border lines)]
                                                    unique-border-variations)
                                             top (if-let [{:keys [lines]} (placed [x (dec y)])]
                                                   [(bottom-border lines)]
                                                   unique-border-variations)]
                                         (arrange-tile (tid->lines tid) left top)))})))
        placed (reduce place-tile
                       placed
                       (rest  ; we've already placed [0 0]
                        (for [row (range width)
                              col (range width)]
                          [col row])))
        stitched-map (vec
                      (apply
                       concat
                       (for [row (range width)]
                         (apply
                          map
                          str
                          (for [col (range width)]
                            (remove-borders (:lines (placed [col row]))))))))
        map-dash-count (count (filter #{\#} (str/join stitched-map)))
        matches (some scan (variations stitched-map))]
    (- map-dash-count (* (count matches) monster-dash-count))))

(time (solution2 real-input))
;; => 2476
