(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of all of the calibration values?
(defn numeric? [c]
  (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))

(defn sum-calibrations [input]
  (->> (read-lines input)
       (map #(filter numeric? %))
       (map #(list (first %) (last %)))
       (map (fn [c] (map #(Character/digit % 10) c)))
       (map #(+ (* 10 (first %)) (second %)))
       (apply +)))


;; (sum-calibrations small-input)
;; 142

;; (sum-calibrations large-input)
;; 55834



;; Part 2
;; Some of the numbers are spelled out.
(def num-re "zero|one|two|three|four|five|six|seven|eight|nine|0|1|2|3|4|5|6|7|8|9")

(def first-re (re-pattern (str "^.*?(" num-re ").*$")))

(def last-re (re-pattern (str "^.*(" num-re ").*?$")))

(defn parse-num [s]
  ({"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
    "zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9} s))

(defn sum-text-calibrations [input]
  (->> (read-lines input)
       (map (fn [s] (list (last (re-find first-re s)) (last (re-find last-re s)))))
       (map (fn [coll] (map #(parse-num %) coll)))
       (map #(+ (* 10 (first %)) (second %)))
       (apply +)))

;; (sum-text-calibrations small-input-2)
;; 281

;; (sum-text-calibrations large-input)
;; 53221
