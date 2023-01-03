(ns day-24.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(defn read-input [f]
  (map #(Long/parseLong %) (str/split-lines (slurp f))))

(def real-input (read-input "input-24.txt"))

(def test-input (concat (range 1 6) (range 7 12)))

;; I need to find a set of packages that has one third the weight of the total
;; Starting with 1 package, then moving on to two, etc.
;; Starting from the largest, I should be able to short-circuit

(defn make-groups
  "Divides packages into all possible groups of size."
  [packages size]
  (if (= 0 size) '(())
      (let [packages packages]
        (for [i packages
              more (make-groups (filter #(< % i) packages) (dec size))]
          (concat (list i) more)
          ))))

(defn find-possible-arrangements
  "Finds groups of packages that sum to one third of the total."
  [packages groups size]
  (for [group (make-groups packages size)
        :let  [other (remove (set group) packages)
               group-sum (apply + group)
               other-sum (apply + other)]
        :when (= other-sum (* groups group-sum))]
    group))


;; Apparently I don't need this - is that just luck?
(defn can-balance? [packages]
  (seq?
   (first
    (for [i (range 1 (inc (quot (inc (count packages)) 2)))
          group (make-groups packages i)
          :let [more (remove (set group) packages)
                group-sum (apply + group)
                more-sum (apply + more)]
          :when (= group-sum more-sum)]
      (list group more)))))

;; for i from 1 to num-packages/2
;; find-possible-arrangements
;; filter by can-balance?
;; if 1 left, return it
;; if more than 1, sort-by quantum entanglement
(defn find-arrangement [packages groups]
  (first
   (sort-by second
            (first
             (drop-while empty?
                         (for [i (range 1 (inc (quot (inc (count packages)) 2)))]
                           (for [possible (find-possible-arrangements packages groups i)]
                             [possible (apply * possible)])))))))

;; (time (find-arrangement real-input 2))
;; => [(113 109 107 101 89 1) 11846773891]
;; "Elapsed time: 4665.143099 msecs"

;; (time (find-arrangement real-input 3))
;; => [(113 109 107 61) 80393059]
;; "Elapsed time: 172.523099 msecs"
