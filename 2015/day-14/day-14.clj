(ns day-14.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))


(def test-input
  '("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
    "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."))
(def pattern #"(\w+) can fly (\d+) km/s for (\d+) seconds.+for (\d+) seconds.")

(def test-reindeer (map parse-line test-input))

(defn read-file [f]
  (map parse-line (str/split-lines (slurp f))))

(def real-reindeer (read-file "input-14.txt"))

(defn parse-line [line]
  (let [[_ name speed time rest] (re-matches pattern line)]
    {:name name
     :speed (Long/parseLong speed)
     :time (Long/parseLong time)
     "rest" (Long/parseLong rest)}
    ))

(defn distance [reindeer time]
  (let [cycle-time (+ (:time reindeer) (:rest reindeer))
        cycles (quot time cycle-time)
        time-in-cycle (mod time cycle-time)
        base-distance (* cycles (:speed reindeer) (:time reindeer))]
    (+ base-distance (* (min (:time reindeer) time-in-cycle) (:speed reindeer)))))

(defn distances [reindeers time]
  (->> reindeers
   (map (fn [reindeer]
          (list (:name reindeer) (distance reindeer time))))
   (sort-by second)
   ))

;; (time (last (distances (map parse-line test-input) 1000)))
;; => ("Comet" 1120)
;; "Elapsed time: 0.1911 msecs"


;; (time (last (distances (read-file "input-14.txt") 2503)))
;; => ("Rudolph" 2640)
;; "Elapsed time: 0.9523 msecs"



;; Part 2
;; Award a point to the winning reindeer(s) every second.
(defn award-points [reindeers time points]
 (let []
   (-> reindeers
       (distances time)
       (->>
        (group-by second)
        (sort-by first)
        reverse
        first
        second
        (reduce
         (fn [pts-map [name _]] (update pts-map name #(if (nil? %) 1 (inc %))))
         points)
        ))))

(defn scores [reindeers time]
  (reduce
   (fn [pts-map time]
     (award-points reindeers time pts-map))
   {}
   (take time (iterate inc 1))))

 


;; (time (last (sort-by second (scores test-reindeer 1000))))
;; => ["Dancer" 689]
;; "Elapsed time: 8.1778 msecs"

;; (time (last (sort-by second (scores real-reindeer 2503))))
;; => ["Donner" 1102]
;; "Elapsed time: 9.2817 msecs"
