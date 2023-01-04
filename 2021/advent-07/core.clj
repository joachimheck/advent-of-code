(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Determine the position at which the crabs can most cheaply align.
(defn parse-input [f]
  (load-string (str/join (list "[" (first (read-lines f)) "]"))))

(defn linear-fuel [crabs dest]
  (apply + (map (fn [pos] (abs (- pos dest))) crabs)))

(defn optimal-alignment-position [f fuel-fn]
  (let [crabs (parse-input f)]
    (->> (range (apply min crabs) (apply max crabs))
         (map (fn [dest] (fuel-fn crabs dest)))
         (keep-indexed list)
         (sort-by second)
         (first))))

;; (time (optimal-alignment-position small-input linear-fuel))
;; "Elapsed time: 1.075599 msecs"
;; (2 37)

;; (time (optimal-alignment-position large-input linear-fuel))
;; "Elapsed time: 293.7616 msecs"
;; (342 325528)



;; Part 2
;; Each movement costs one more than the previous
(defn exponential-fuel [crabs dest]
  (apply + (map (fn [pos] (apply + (range (inc (abs (- pos dest)))))) crabs)))


;; (time (optimal-alignment-position small-input exponential-fuel))
;; "Elapsed time: 1.748301 msecs"
;; (5 168)

;; (time (optimal-alignment-position large-input exponential-fuel))
;; "Elapsed time: 50238.0512 msecs"
;; (460 85015836)

(defn exponential-fuel-2 [crabs dest]
  (apply + (map (fn [pos] (let [n (abs (- pos dest))]
                            (/ (* n (inc n)) 2)))
                crabs)))

;; (time (optimal-alignment-position small-input exponential-fuel-2))
;; "Elapsed time: 0.925801 msecs"
;; (5 168)

;; (time (optimal-alignment-position large-input exponential-fuel-2))
;; "Elapsed time: 463.324201 msecs"
;; (460 85015836)
