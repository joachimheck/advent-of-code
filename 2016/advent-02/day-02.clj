(ns day-02.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Bathroom keypad code
(def buttons {1 {"R" 2 "D" 4} 2 {"L" 1 "R" 3 "D" 5} 3 {"L" 2 "D" 6}
              4 {"U" 1 "R" 5 "D" 7} 5 {"U" 2 "R" 6 "D" 8 "L" 4} 6 {"U" 3 "L" 5 "D" 9}
              7 {"U" 4 "R" 8} 8 {"L" 7 "U" 5 "R" 9} 9 {"L" 8 "U" 6}})
(def test-input
  '("ULL"
    "RRDDD"
    "LURDL"
    "UUUUD"))

(defn follow-path [start buttons path]
  (reduce
   (fn [btn dir]
     (let [new-btn (get (get buttons btn) (str dir))]
       (if new-btn new-btn btn)))
   start
   path))

(defn get-code [buttons directions]
  (str/join
   (rest
    (reduce
     (fn [code path] (conj code (follow-path (last code) buttons path)))
     [5]
     directions))))

(get-code buttons test-input)

(get-code buttons (str/split-lines (slurp "input-02.txt")))
;; => "97289"

;; Part 2
(def real-buttons {1 {"D" 3}
                   2 {"R" 3 "D" 6} 3 {"L" 2 "U" 1 "R" 4 "D" 7} 4 {"L" 3 "D" 8}
                   5 {"R" 6} 6 {"L" 5 "U" 2 "R" 7 "D" \A} 7 {"L" 6 "U" 3 "R" 8 "D" \B}
                   8 {"L" 7 "U" 4 "R" 9 "D" \C} 9 {"L" 8}
                   \A {"U" 6 "R" \B} \B {"L" \A "U" 7 "R" \C "D" \D} \C {"L" \B "U" 8}
                   \D {"U" \B}})
(get-code real-buttons test-input)
(get-code real-buttons (str/split-lines (slurp "input-02.txt")))
;; => "9A7DC"
