(ns day-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def puzzle-input (slurp "puzzle-input.txt"))

(defn read-input [input]
  (map (fn [v] (vec (map (fn [e] (Long/parseLong e))
                         v)))
       (map (fn [s] (str/split s #"x"))
            (str/split-lines input))))

(read-input puzzle-input)

;; Part 1
;; Compute required amount of wrapping paper.

(defn compute-paper-amount [box]
  (let [[l w h] (sort box)]
    (+ (* 3 l w)
       (* 2 w h)
       (* 2 h l))))

;; (time (reduce + (map compute-paper-amount (read-input puzzle-input))))
;; => 1586300
;; "Elapsed time: 3.2094 msecs"


      
;; Part 2
;; Compute required amount of ribbon.

(defn compute-ribbon-amount [box]
  (let [[l w h] (sort box)]
    (+ (+ l l w w)
       (* l w h))))

;; (time (reduce + (map compute-ribbon-amount (read-input puzzle-input))))
;; => 3737498
;; "Elapsed time: 4.7589 msecs"

