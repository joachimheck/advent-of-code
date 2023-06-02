(ns advent-24.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])
(require '[clojure.math.numeric-tower :as math])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 24: Planet of Discord

;; Part 1
;; What is the biodiversity rating for the first layout that appears twice?

(defn parse-input [f]
  (mapv vec (read-lines f)))

(defn neighbors [[x y]]
  (list [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]))

(defn get-space [grid [x y]]
  (get-in grid [y x]))

(defn process-grid [grid]
  (vec
   (for [j (range 5)]
     (vec
      (for [i (range 5)]
        (let [current (get-space grid [i j])
              neighbor-bugs (count (filter #{\#} (map #(get-space grid %) (neighbors [i j]))))]
          ;; (println "current" current "neighbor-bugs" neighbor-bugs)
          (cond (and (= current \#) (not= 1 neighbor-bugs))
                \.
                (and (= current \.) (<= 1 neighbor-bugs 2))
                \#
                :else
                current)))))))

(defn draw-grid [grid]
  (str/join
   "\n"
   (for [j (range 5)]
     (str/join
      (for [i (range 5)]
        (get-space grid [i j]))))))

(defn compute-biodiversity [grid]
  (reduce (fn [acc c]
            (+ (* 2 acc) (if (= c \#) 1 0)))
          0
          (reverse (flatten grid))))

(defn process-until-repeat [grid]
  (loop [grid grid
         previous #{}]
    (let [biodiversity (compute-biodiversity grid)]
      (if (some #{biodiversity} previous)
        (do
          (println (draw-grid grid))
          biodiversity)
        (recur (process-grid grid) (conj previous biodiversity))))))

;; (time (process-until-repeat (parse-input small-input)))
;; .....
;; .....
;; .....
;; #....
;; .#...
;; "Elapsed time: 21.8248 msecs"
;; 2129920
;; (time (process-until-repeat (parse-input large-input)))
;; #.###
;; #....
;; #.#..
;; #....
;; #...#
;; "Elapsed time: 10.1229 msecs"
;; 17863741



;; Part 2
