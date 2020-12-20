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
  (tap> {:lines lines :left left :top top})
  (some #(and (= (right-border %) left)
              (= (top-border %) top)
              %)
        (variations lines)))

(defn solution2 [input]
  (let [tid->lines (tile-id->tile-lines input)
        width (long (Math/sqrt (count tid->lines)))
        tid->borders (tile-id->borders tid->lines)
        border->tids (border->tile-ids tid->borders)
        uniq-borders (unique-borders border->tids)
        tid->unique-borders (tile-id->unique-borders tid->borders uniq-borders)
        uniq-freqs (unique-border-frequencies uniq-borders)
        sides (side-tiles uniq-freqs)
        top-left (some
                  #(when-let [lines (apply arrange-tile (tid->lines %) (tid->unique-borders %))]
                     {:tid % :lines lines})
                  (corner-tiles uniq-freqs))]

    (ms/save :a)
    #_(letfn [(arrange-tile [tid [x y placed]]
              ,,,)
            (place-tile [placed [x y]]
              (let [tid (if-let [{:keys [tid lines]}
                                 (get placed [(dec x) y])]
                          (first (remove tid (border->tids (normalize (right-border lines))))))]))]
      (let [placements (reduce place-tile
                               {[0 0] (arrange-tile top-left [0 0] {})}
                               (for [row (range width)
                                     col (range width)]
                                 [row col]))]
        placements))))



(comment
  (solution2 demo-input)
  (ms/ld :a)

  top-left

  (fliph ["abc" "def" "hij"])
  (flipv ["abc" "def" "hij"])
  (transpose ["abc" "def" "hij"])
  (variations ["abc" "def" "hij"])

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
