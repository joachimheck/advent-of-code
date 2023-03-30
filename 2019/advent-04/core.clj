(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 4: Secure Container

;; Part 1
;; How many different passwords within the range given in your puzzle input meet the criteria?
(defn digits [n]
  (if (< n 10)
    [n]
    (let [quotient (quot n 10)
          remainder (rem n 10)]
      (conj (digits quotient) remainder))))

(defn two-adjacent-same? [[a b c d e f]]
  (or (= a b) (= b c) (= c d) (= d e) (= e f)))

(defn never-decreasing? [[a b c d e f]]
  (and (<= a b) (<= b c) (<= c d) (<= d e) (<= e f)))

(defn meets-criteria? [n]
  (let [n-digits (digits n)]
    (and (two-adjacent-same? n-digits)
         (never-decreasing? n-digits))))

(defn find-matching-values [fn? n-min n-max]
  (reduce (fn [acc n] (if (fn? n) (conj acc n) acc))
          []
          (range n-min (inc n-max))))

(defn count-matching-values [fn? n-min n-max]
  (count (find-matching-values fn? n-min n-max)))

;; (count-matching-values meets-criteria? 372037 905157)
;; 481



;; Part 2
;; The two matching digits are not part of a larger group of matching digits.
(defn two-adjacent-same-minimum-group? [[a b c d e f]]
  (or (and (= a b) (not= b c))
      (and (= b c) (not= a b) (not= c d))
      (and (= c d) (not= b c) (not= d e))
      (and (= d e) (not= c d) (not= e f))
      (and (= e f) (not= d e))))

(defn meets-criteria-2? [n]
  (let [n-digits (digits n)]
    (and (two-adjacent-same-minimum-group? n-digits)
         (never-decreasing? n-digits))))

;; 270
;; ---> answer <---

;; Typo; was checking the wrong digits. Compared the two lists and found a wrong value to track down the bug.

;; (count-matching-values meets-criteria-2? 372037 905157)
;; 299
