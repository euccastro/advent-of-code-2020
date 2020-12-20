(ns advent.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [miracle.save :as ms]))

(def demo-input (slurp (io/resource "demo20")))
(def real-input (slurp (io/resource "input20")))


(defn tiles [input]
  (str/split input #"\R\R"))


(defn tile-number [s]
  (Long. (re-find #"\d+" s)))


(defn tile-id->tile-lines [input]
  (->> (tiles input)
       (map str/split-lines)
       (map (fn [[l & ls]]
              [(tile-number l)
               (vec ls)]))
       (into {})))

(defn reverse-line [l]
  (apply str (reverse l)))

(defn normalize-border [line]
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
  (into {}
        (map (fn [[tile-id tile-lines]]
               [tile-id (borders tile-lines)])
             m)))

(defn border->tile-ids
  "takes the result of tile-id->borders"
  [m]
  (->> (for [[tid bs] m
             b bs]
         [b tid])
       (group-by first)
       (map (fn [[b matches]]
              [b (map second matches)]))
       (into {})))

(defn unique-borders
  "takes the result of border->tile-ids"
  [m]
  (filter #(= 1 (count (second %)))
          m))

(defn tile-id->unique-borders
  "takes the result of tile-id->borders and unique-borders"
  [tid->borders unique-borders]
  (let [s (set (map first unique-borders))]
    (into {}
          (map (fn [[k v]] [k (filter s v)])
               tid->borders))))

(defn unique-border-frequencies
  "takes the result of tile-id->unique-borders"
  [m]
  (->> m
       (map (comp first second))
       frequencies))

(defn tiles-with-n-unique-borders
  "takes the result of unique-border-frequencies and a number of sides"
  [m n]
  (->> m
       (keep (fn [[tile unique-border-count]]
               (when (= n unique-border-count)
                 tile)))))

(defn side-tiles
  "takes the result of unique-border-frequencies"
  [m]
  (tiles-with-n-unique-borders m 1))

(defn corner-tiles
  "takes the result of unique-border-frequencies"
  [m]
  (tiles-with-n-unique-borders m 2))

(defn solution1 [input]
  (->> input
       tile-id->tile-lines
       tile-id->borders
       border->tile-ids
       unique-borders
       unique-border-frequencies
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

(def monster "                  #
#    ##    ##    ###
 #  #  #  #  #  #   ")

(def monster-variations (variations (str/split-lines monster)))

(defn monster-kernel [monster-lines]
  {:monster-width (count (first monster-lines))
   :monster-height (count monster-lines)
   :monster-coords
   (for [[y line] (map-indexed vector monster-lines)
         [x char] (map-indexed vector line)
         :when (= char \#)]
     [x y])})

(def monster-kernels (map monster-kernel monster-variations))

(def monster-dash-count (count (:monster-coords (first monster-kernels))))

(defn swipe [map-lines {:keys [monster-width monster-height monster-coords]}]
  (let [map-width (count (first map-lines))
        map-height (count map-lines)]
    (seq
     (for [scan-x (range (inc (- map-width monster-width)))
           scan-y (range (inc (- map-height monster-height)))
           :when (every?
                  (fn [[x y]]
                    (= (get-in map-lines [(+ scan-y y) (+ scan-x x)]) \#))
                  monster-coords)]
       [scan-x scan-y]))))

(comment

  (swipe [".####"
          "#...#"
          "###.#"]
         {:monster-width 2
          :monster-height 2
          :monster-coords [[0 0] [1 1]]})

  )

(defn solution2 [input]
  (let [tid->lines (tile-id->tile-lines input)
        width (long (Math/sqrt (count tid->lines)))
        tile-width (count (first (vals tid->lines)))
        tid->borders (tile-id->borders tid->lines)
        border->tids (border->tile-ids tid->borders)
        uniq-borders (unique-borders border->tids)
        tid->unique-borders (tile-id->unique-borders tid->borders uniq-borders)
        uniq-freqs (unique-border-frequencies uniq-borders)
        sides (side-tiles uniq-freqs)
        placed (let [tid (second (corner-tiles uniq-freqs))
                     [left top] (tid->unique-borders tid)
                     lines (tid->lines tid)
                     arr-lines (or (arrange-tile lines left top)
                                   (arrange-tile lines left (reverse-line top))
                                   (arrange-tile lines (reverse-line left) top)
                                   (arrange-tile lines (reverse-line left) (reverse-line top)))]
                 {[0 0] {:tid tid :lines arr-lines}})]
    (letfn [(place-tile [placed [x y]]
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
                                  (arrange-tile (tid->lines tid) left top)))})))]
      (let [placed (reduce place-tile
                           placed
                           (for [row (range width)
                                 col (range width)
                                 :when (not= row col 0)]
                             [col row]))
            stitched-map (vec
                          (apply
                           concat
                           (for [row (range width)]
                             (apply map str
                                    (for [col (range width)]
                                      (subvec
                                       (mapv #(subs % 1 (dec tile-width))
                                             (:lines (placed [col row])))
                                       1 (dec tile-width)))))))
            map-dash-count (count (filter #{\#} (str/join stitched-map)))
            matches
            (some #(swipe stitched-map %) monster-kernels)]
        (ms/save :a)
        (- map-dash-count (* (count matches) monster-dash-count))))))


(comment

  width
  tile-width
  (* 12 8)
  (count (tiles demo-input))
  (count (first (str/split-lines (tiles (demo-input)))))
  (count (tiles real-input))
  (Math/sqrt *1)
  (map #(count (swipe stitched-map %)) monster-kernels)
  map-dash-count
  (count (filter #{\#} (str/join stitched-map)))

  (def demo-stitched (vec (str/split-lines ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###")))
  (def kernel (monster-kernel (str/split-lines monster)))
  (count (:monster-coords kernel))
  (count (filter #{\#} (str/join demo-stitched)))
  (- 303 30)
  (swipe )
  (solution2 real-input)
;; => 2686
  (ms/ld :a)

  (count stitched-map)
  (count (first stitched-map))
  stitched-map
  (count matches)
  (first monster-kernels)
  (some #(swipe % kernel) (variations demo-stitched))
  (= (nth (variations stitched-map) 6) demo-stitched)

  (some identity (map #(swipe % kernel) (variations stitched-map)))
  (some #(swipe % kernel) (variations stitched-map))
  (swipe (nth (variations stitched-map) 6) kernel)

  (count (variations stitched-map))


  (= "")
  (count stitched-map)
  (count (first stitched-map))
  (first stitched-map)
  (apply map str
       (for [col (range width)]
         (:lines (placed [col 0]))))
  (for [row (range width)]
    (for [col (range width)]
      (:lines (placed [col row]))))
  stitched-map

  placed

  (count placements)

  top-left

  (def next-id (first ))

  (arrange-tile (tid->lines next-id) (right-border (:lines top-left)) (reverse-line (first (tid->unique-borders next-id))))



  (-> placed
      (place-tile 1 0)
      (place-tile 2 0))

  ()
  (border->tids (normalize-border (left-border (:lines top-left))))
  ()

  (map str ["abc" "def" "hij"] ["123" "456" "789"])
  (fliph ["abc" "def" "hij"])
  (flipv ["abc" "def" "hij"])
  (transpose ["abc" "def" "hij"])
  (variations ["abc" "def" "hij"])
  (corner-tiles uniq-freqs)
  (def top-left (first (corner-tiles uniq-freqs)))

  (def ub (tid->unique-borders top-left))
  ub
  (def top (first ub))

  (def left (second ub))
  (arrange-tile (tid->lines top-left) (reverse-line top) left)

  border->tids
  (tid->unique-borders )
  (defn variations [lines]
    )


  (apply arrange-tile (tid->lines top-left) (reverse (tid->unique-borders top-left)))

  (arrange-tile )
  width
  top-left
  (tid->unique-borders tid)
  (def tid top-left)
  sides

  (sides)


  (defn arrange-tile

    [tid [x y] placed]
    )
  (->> demo-input
       tile-id->tile-lines
       tile-id->borders
       border->tile-ids
       tile-id->unique-borders
       corners)
  (border->tile-ids
   (tile-id->borders
    (tile-id->tile-lines demo-input)))
  (->> demo-input
       border->tiles
       (map (fn [[_ [[_ x]]]] x))
       frequencies
       (keep (fn [[tile unique-border-count]]
               (when (= 2 unique-border-count)
                 tile)))
       (apply *))

  (-> demo-input
      tile-id->tile-lines
      count)
  (count (tiles real-input))

  (unique-border-frequencies demo-input)

  (sides demo-input)
  (corners demo-input)
  (borders ["abc" "def" "ghi"])
  (normalize-border "cyabc")
  (tile-number (first (tiles demo-input)))
  (borders (rest (str/split-lines (first (tiles demo-input)))))



  (keep (fn [[tile unique-border-count]]
          (when (= 2 unique-border-count)
            tile))
        (frequencies
         (map (fn [[_ [[_ x]]]] x)
              (filter #(= 1 (count (second %)))
                      (border->tiles demo-input)))))
  )
