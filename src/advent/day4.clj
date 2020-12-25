(ns advent.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def demo-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def real-input (slurp (io/resource "input4")))

(def input real-input)

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn parse-passport [passport-str]
  (->> (str/split passport-str #"\s")
       (map #(str/split % #":"))
       (into {})))

(def passports (map parse-passport (str/split input #"\R\R")))

(defn valid? [passport]
  (= required-fields (set/intersection required-fields (set (keys passport)))))

(count (filter valid? passports))
;; => 254
