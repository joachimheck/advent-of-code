(ns day-24.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(defn next-number [n] (mod (* 252533 n) 33554393))

(defn sum-to [n]
  (let [half (quot n 2)]
    (+ (* half (inc n))
       (if (even? n)
         0
         (inc half)))))

(defn column-1-val [row]
  (+ 1 (sum-to (dec row))))

;; Find the line y = -x + n that goes through the given point [i,j]
;; This is y = -x + i + j
;; It intercepts the X axis at y = i + j
;; Get the column-1 value for that y, and add x to move along the diagonal.
(defn num-at [row col]
  (let [intercept (dec (+ row col))
        i-val (column-1-val intercept)]
    (+ (dec col)
       (+ 1 (sum-to (dec intercept))))))

(defn code-at [row col]
  (nth (iterate next-number 20151125) (dec (num-at row col))))

;; (time (code-at 2947 3029))
;; => 19980801
;; "Elapsed time: 1046.028 msecs"

