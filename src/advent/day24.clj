(ns advent.day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def real-input (slurp (io/resource "input24")))

;; the trick is to project the hex grid into a more familiar rectangular one.
;;
;; https://www.redblobgames.com/grids/hexagons/#coordinates-axial
(def directions
  {"e" [1 0]
   "ne" [1 1]
   "nw" [0 1]
   "w" [-1 0]
   "sw" [-1 -1]
   "se" [0 -1]})

(def dir-regex (re-pattern (str/join "|" (keys directions))))

(defn line-destination [line]
  (->> line
       (re-seq dir-regex)
       (map directions)
       (reduce (partial map +))))

(defn set-toggle [s x]
  ((if (s x) disj conj) s x))

(defn place-tiles [input]
  (->> input
       str/split-lines
       (map line-destination)
       (reduce set-toggle #{})))

(defn solution1 [input]
  (count (place-tiles input)))

(solution1 real-input)
;; => 386

;; part 2

(defn neighbors [tile]
  (map (partial map + tile) (vals directions)))

(defn step [blacks]
  (->> blacks
       (mapcat neighbors)
       frequencies
       (merge-with + (zipmap blacks (repeat 0)))
       (filter (fn [[tile n]]
                 (if (blacks tile)
                   (<= 1 n 2)
                   (= n 2))))
       (map first)
       (into #{})))

(defn solution2 [input]
  (->> input
       place-tiles
       (iterate step)
       (drop 100)
       first
       count))

(time (solution2 demo-input))
;; => 2208

;; runs in 2secs
(time (solution2 real-input))
;; => 4214
