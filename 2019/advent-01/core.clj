(ns advent-01.core)

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

;; Day 1: The Tyranny of the Rocket Equation

;; Part 1
;; What's the total fuel requirement for the rocket?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map parse-long)))

;; (let [modules (parse-input large-input)]
;;   (apply + (map #(- (quot % 3) 2) modules)))
;; 3188480



;; Part 2
;; Include fuel for the mass of the fuel.
(defn compute-fuel [mass]
  (let [fuel (max 0 (- (quot mass 3) 2))]
    (if (> fuel 0)
      (+ fuel (compute-fuel fuel))
      fuel)))

(defn compute-total-fuel [masses]
  (apply + (map compute-fuel masses)))

;; (compute-total-fuel (parse-input large-input))
;; 4779847
