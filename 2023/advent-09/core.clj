(ns advent-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of the extrapolated values at the ends of the sequences?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #" "))
       (map #(map parse-long %))))

(defn differences [s]
  (map (fn [[a b]] (- b a)) (partition 2 1 s)))

(defn compute-differences [s]
  (loop [current s
         sequences [s]]
    (let [next (differences current)]
      (if (every? zero? next)
        (conj sequences next)
        (recur next (conj sequences next))))))

(defn extrapolate [s]
  (let [differences (compute-differences s)]
    (reduce (fn [acc s] (+ acc (last s)))
            0
            (reverse differences))))

(defn sum-extrapolations [input]
  (apply + (map extrapolate (parse-input input))))

;; (sum-extrapolations small-input)
;; 114

;; (sum-extrapolations large-input)
;; 1834108701



;; Part 2
;; Extrapolate backwards
(defn extrapolate-back [s]
  (let [differences (compute-differences s)]
    (reduce (fn [acc s] (- (first s) acc))
            0
            (reverse differences))))

(defn sum-extrapolations-back [input]
  (apply + (map extrapolate-back (parse-input input))))

;; (sum-extrapolations-back small-input)
;; 2

;; (sum-extrapolations-back large-input)
;; 993
