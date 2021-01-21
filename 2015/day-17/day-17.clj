(ns day-17.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Divide egg nog into containers.

(def test-amount 25)

(def test-containers [20 15 10 5 5])

(def real-amount 150)

(def real-containers [50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40])

;; [20 15 10 5 5] 25
;; separate first element
;; 20 [15 10 5 5] 25
;; Now we need two paths
;; 1. Find the numbers in the rest of the vector that sum to 25-20 = 5
;;    Return 20 + each of those sets
;;    map (list 20 %) (recurse [15 10 5 5] 5)
;;    15 [10 5 5] 5 - 15 > 5 so don't do part 1
;; 2. Find the numbers in the rest of the vector that sum to 25
;;    (recurse [15 10 5 5] 25)

(defn group-containers [[container & rest-c :as containers] amount]
  (if (= container amount) (concat (list amount) (group-containers rest-c amount))
      (if (or (nil? containers) (empty? containers)) '()
          (concat
           (if (< amount container) '()
               (map #(flatten (list container %)) (group-containers rest-c (- amount container))))
           (group-containers rest-c amount)))))

;; (time (count (group-containers containers test-amount)))
;; => ((20 5) (20 5) (15 10) (15 (5 5)))
;; "Elapsed time: 0.1705 msecs"

;; (time (count (group-containers (reverse (sort real-containers)) real-amount)))
;; => 654
;; "Elapsed time: 5.5469 msecs"



;; Part 2
;; How many ways are there to use the minimum number of containers?
(-> real-containers
    sort
    reverse
    (group-containers real-amount)
    (->>
     (group-by count)
     first ;; count + items
     second ;; items
     count))

;; (time (count (second (first 
;;                       (group-by count
;;                                 (group-containers (reverse (sort real-containers)) real-amount))))))
;; => 57
;; "Elapsed time: 20.0843 msecs"

