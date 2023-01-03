(ns day-18.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; How many lights are on after 100 steps?

(def test-size 6)

(def test-input
  '(".#.#.#"
    "...##."
    "#....#"
    "..#..."
    "#.#..#"
    "####.."))

(def real-size 100)

(defn read-file [f]
  (str/split-lines (slurp f)))

(defn parse-input [lines]
 (->> lines
      (map-indexed list)
      (mapcat (fn [[i line]]
                (map #(vector % i)
                     (->> line
                          (map-indexed list)
                          (keep (fn [[i c]] (when (= \# c) i)))))))
      set))

(defn neighbors [[x y]]
  (list [(dec x) (dec y)]
        [x (dec y)]
        [(inc x) (dec y)]
        [(inc x) y]
        [(inc x) (inc y)]
        [x (inc y)]
        [(dec x) (inc y)]
        [(dec x) y]))

(defn get-lit-neighbors [grid p]
  (remove nil? (map (fn [np] (get grid np)) (neighbors p))))


(defn process [[grid size]]
  (list
   (set
    (remove nil?
            (for [y (range size)
                  x (range size)]
              (let [lit-neighbors (count (get-lit-neighbors grid [x y]))]
                (if (some? (get grid [x y]))
                  (when (<= 2 lit-neighbors 3) [x y])
                  (when (= 3 lit-neighbors) [x y]))))))
   size))


;; (time (first (nth (iterate process (list (parse-input test-input) test-size)) 4)))
;; => #{[2 2] [2 3] [3 3] [3 2]}
;; "Elapsed time: 18.0575 msecs"

;; (time (count (first (nth (iterate process (list (parse-input (read-file "input-18.txt")) real-size)) 100))))
;; => 1061
;; "Elapsed time: 4201.5724 msecs"



;; Part 2
;; The corner lights are always on.
(defn process-2 [[grid size]]
  (list
   (set
    (remove nil?
            (concat
             (list [0 0] [0 (dec size)] [(dec size) 0] [(dec size) (dec size)])
             (for [y (range size)
                   x (range size)]
               (let [lit-neighbors (count (get-lit-neighbors grid [x y]))]
                 (if (some? (get grid [x y]))
                   (when (<= 2 lit-neighbors 3) [x y])
                   (when (= 3 lit-neighbors) [x y])))))))
   size))

(def test-input-2
  '("##.#.#"
    "...##."
    "#....#"
    "..#..."
    "#.#..#"
    "####.#"))

;; (time (count (first (nth (iterate process-2 (list (parse-input test-input-2) test-size)) 5))))
;; => 17
;; "Elapsed time: 2.133 msecs"


;; (time (count (first (nth (iterate process-2 (list (parse-input (read-file "input-18.txt")) real-size)) 100))))
;; => 1006
;; "Elapsed time: 4229.9029 msecs"
