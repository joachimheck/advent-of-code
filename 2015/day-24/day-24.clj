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

(defn split-weights
  ([weights]
   (distinct (flatten
     (for [i (range 1 (count weights))]
       (split-weights weights i)))))
  ([weights level]
   (if (= 0 level)
     '(())
     (if (not (seq weights))
       (list weights)
       (for [w weights
             more (split-weights (remove #(= w %) weights) (dec level))]
         (set (concat (list w) more)))
       ))))

(split-weights '(1 2 3 4))



 
