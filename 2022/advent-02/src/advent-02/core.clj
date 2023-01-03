(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")
(def shape-scores {"X" 1 "Y" 2 "Z" 3})

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn- parse-shapes [s] (list (subs s 0 1) (subs s 2 3)))

;; Rock, Paper, Scissors

;; Part 1
;; What score would I get by following my strategy guide?
(def results {"X" {"A" 3 "B" 0 "C" 6} "Y" {"A" 6 "B" 3 "C" 0} "Z" {"A" 0 "B" 6 "C" 3}})

(defn- score [[them-shape us-shape]]
  (+ (get (get results us-shape) them-shape)
     (get shape-scores us-shape)))

(defn- score-strategy [f]
  (reduce + (map score (map parse-shapes (read-lines f)))))

;; (score-strategy small-input)
;; 15
;; (score-strategy large-input)
;; 13009


;; Part 2
;; X = lose, Y = draw, Z = win.
(def results-2 {"R" {"A" 3 "B" 0 "C" 6} "P" {"A" 6 "B" 3 "C" 0} "S" {"A" 0 "B" 6 "C" 3}})
(def counter-shapes {"A" {"X" "S" "Y" "R" "Z" "P"} "B" {"X" "R" "Y" "P" "Z" "S"} "C" {"X" "P" "Y" "S" "Z" "R"}})
(def strategy-scores {"X" 0 "Y" 3 "Z" 6})
(def shape-scores-2 {"R" 1 "P" 2 "S" 3})

(defn- score-correctly [[shape strategy]]
  (let [guide (list shape strategy)
        counter-shape (get-in counter-shapes guide)]
    (+ (get shape-scores-2 counter-shape) (get strategy-scores strategy))))

;; (reduce + (map score-correctly (map parse-shapes (read-lines small-input))))
;; 12
;; (reduce + (map score-correctly (map parse-shapes (read-lines large-input))))
;; 10398


