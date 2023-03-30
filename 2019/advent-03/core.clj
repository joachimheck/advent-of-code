(ns advent-03.core)

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

;; Day 3: Crossed Wires

;; Part 1
;; What's the distance to the closest point at which wires cross?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/split % #","))))

(defn get-points [[x y] move]
  (let [[_ dir dist] (re-matches #"([A-Z])(\d+)" move)]
    (for [i (range 1 (inc (parse-long dist)))]
      (case dir
        "U" [x (- y i)]
        "D" [x (+ y i)]
        "L" [(- x i) y]
        "R" [(+ x i) y]))))

(defn follow-path [path]
  (reduce (fn [acc move]
            (let [start (if (last acc) (last acc) [0 0])]
              (apply conj acc (get-points start move))))
          []
          path))

(defn find-intersections [[a b :as paths]]
  (let [point-sets (map set (map follow-path paths))]
    (apply set/intersection point-sets)))

(defn distance [[x y] [a b]]
  (+ (abs (- x a))
     (abs (- y b))))

(defn find-closest-crossing [paths]
  (apply min (map #(distance [0 0] %) (find-intersections paths))))

;; (find-closest-crossing (parse-input small-input))
;; 6

;; (find-closest-crossing (parse-input large-input))
;; 260



;; Part 2
;; What's the fewest combined steps the wires must take to reach an intersection?
(defn find-crossing-fewest-steps [paths]
  (let [[points-a points-b] (map follow-path paths)
        intersections (find-intersections paths)
        distances (reduce (fn [acc intersection]
                            (conj acc (+ 2 (.indexOf points-a intersection) (.indexOf points-b intersection))))
                          '()
                          intersections)]
    (apply min distances)))

;; (find-crossing-fewest-steps (parse-input small-input))
;; 30

;; (find-crossing-fewest-steps (parse-input large-input))
;; 15612
