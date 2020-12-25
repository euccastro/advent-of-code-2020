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

(defn valid1? [passport]
  (= required-fields (set/intersection required-fields (set (keys passport)))))

(count (filter valid1? passports))
;; => 254

;;; part 2

(defn four-digits [s]
  (re-matches #"\d{4}" s))

(def rules
  {"byr" (fn byr [s]
           (and (four-digits s)
                (<= 1920 (read-string s) 2002)))
   "iyr" (fn byr [s]
           (and (four-digits s)
                (<= 2010 (read-string s) 2020)))
   "eyr" (fn eyr [s]
           (and (four-digits s)
                (<= 2020 (read-string s) 2030)))
   "hgt" (fn hgt [s]
           (let [[_ n units] (re-matches #"^(\d+)(cm|in)$" s)]
             (case units
               "cm" (<= 150 (read-string n) 193)
               "in" (<= 59 (read-string n) 76)
               false)))
   "hcl" (fn hcl [s]
           (re-matches #"^#[0-9a-f]{6}" s))
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (fn pid [s]
           (re-matches #"\d{9}" s))})

(defn valid2? [passport]
  (and (valid1? passport)
       (->> passport
            (merge-with #(%1 %2) rules)
            vals
            (every? identity))))

(count (filter valid2? passports))
;; => 184
