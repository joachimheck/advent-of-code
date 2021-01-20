(ns day-16.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Divide egg nog into containers.

(def test-amount 25)

(def test-containers [20 15 10 5 5])

;; [20 15 10 5 5]
;; separate first element
;; 20 [15 10 5 5]
;;     Check > target? I don't think I need to
;; Remaining amount = target - first = 5
;; Recurse for each set remaining after dropping the first item


(defn group-containers [[container & rest-c :as containers] amount]
  (if (<= amount 0) '() 
      (concat
       (list amount)
       (if (<= container amount)
         (for [i (range (count rest-c))]
           (group-containers (drop i rest-c) (- amount container))
           )
         '())
       (for [i (range (count rest-c))]
         (group-containers (drop i rest-c) amount)
         )
       ))
  )

(group-containers test-containers test-amount)



(filter #(= test-amount (apply + %)) (group-containers test-containers test-amount))
