(ns advent.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def demo-input "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")


(def pat (first (str/split-lines demo-input)))


(defn parse-pattern [s]
  (let [[_ index pattern] (re-matches #"(\d+): (.*)" s)]
    [(read-string index)
     (read-string (str "(" pattern ")"))]))


(defn parse-patterns [pattern-str]
  (into {} (map parse-pattern (str/split-lines pattern-str))))


(defn solvable? [resolved pat]
  (every? resolved (filter number? pat)))


(defn solve [resolved pat]
  (cond
    (= [java.lang.String] (map type pat)) (first pat)
    (every? number? pat) (apply str (map resolved pat))
    :else (let [[a [_ & b]] (split-with (complement #{'|}) pat)]
            (str "("
                 (solve resolved a)
                 "|"
                 (solve resolved b)
                 ")"))))


(defn build-re [patterns]
  (loop [resolved {}
         unresolved patterns]
    (if-let [s (resolved 0)]
      (re-pattern (str "^" s "$"))
      (let [solvable (filter #(solvable? resolved (second %)) unresolved)]
        (recur (into resolved (map
                               (fn [[idx pat]]
                                 [idx (solve resolved pat)])
                               solvable))
               (apply dissoc unresolved (map first solvable)))))))


(defn count-matches [input]
  (let [[patterns strings] (str/split input #"\R\R")
        regex (build-re (parse-patterns patterns))]
    (count (filter (partial re-matches regex) (str/split-lines strings)))))


(count-matches demo-input)

(def real-input (slurp (io/resource "input19")))

(count-matches real-input)


;;; for part 2 I give up on regexes

(def demo-input2 "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(defn compile-rule [xs]
  (cond

    (string? (first xs)) [:char (ffirst xs)]

    (some #{'|} xs) (let [[a [_ & b]]
                          (split-with (complement #{'|}) xs)]
                      [:or (compile-rule a) (compile-rule b)])
    :else [:cat xs]))


(defmulti match (fn [[rule-type _] & _]
                  rule-type))


(defmethod match :char
  [[_ char] _ s]
  (when (= char (first s))
    (list (subs s 1))))


(defmethod match :or
  [[_ a b] rules s]
  (lazy-cat (match a rules s) (match b rules s)))


(defmethod match :cat
  [[_ indices] rules s]
  (if (empty? indices)
    (list s)
    (let [[m & alts]
          (match (rules (first indices)) rules s)
          match-rest #(match [:cat (rest indices)] rules %) ]
      (cond->> (mapcat match-rest alts)
        m (concat (match-rest m))))))


(defmethod match :default
  [& args]
  (throw (ex-info "wat" args)))


(defn matches? [rules s]
  (some #{""} (match (rules 0) rules s)))


(defn matching-strings [input]
  (let [[patterns strings] (map str/split-lines (str/split input #"\R\R"))
        patterns (merge (into {} (map parse-pattern patterns))
                        {8 '(42 | 42 8)
                         11 '(42 31 | 42 11 31)})
        rules (into {} (map (fn [[idx pat]] [idx (compile-rule pat)])
                            patterns))]
    (filter (partial matches? rules) strings)))

(count (matching-strings demo-input2))
;; => 12

(time (count (matching-strings real-input)))
;; => 309
