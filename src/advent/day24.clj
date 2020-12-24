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

(def directions
  {\e [1 0]
   \n [1 1]
   \N [0 1]
   \w [-1 0]
   \S [-1 -1]
   \s [0 -1]})

(defn line-destination [line]
  (-> line
      (str/replace "nw" "N")
      (str/replace "ne" "n")
      (str/replace "sw" "S")
      (str/replace "se" "s")
      (->>
       (map directions)
       (reduce (partial map +)))))

(defn place-tiles [input]
  (->> input
       (str/split-lines)
       (map line-destination)
       (reduce (fn [blacks pos]
                 ((if (blacks pos) disj conj)
                  blacks
                  pos))
               #{})))

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

(time (solution2 real-input))
;; => 4214
