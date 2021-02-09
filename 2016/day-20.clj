(ns day-20.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Find the first value not in any of the ranges.
(def test-input
  '("5-8"
    "0-2"
    "4-7"))

(defn parse-line [line]
  (let [[_ min max] (re-matches #"(\d+)-(\d+)" line)]
    [(Long/parseLong min) (Long/parseLong max)]))

(map parse-line test-input)

(defn collapse [pairs]
  (loop [pairs (sort-by first pairs)]
    (let [collapsed
          (:out (reduce
                 (fn [acc [[l1 h1] [l2 h2]]]
                   (let [do-merge (or (<= l2 l1 (inc h2)) (<= l1 l2 (inc h1)))]
                     (if (:merged acc)
                       (assoc acc :merged false)
                       (assoc acc
                              :out (concat (:out acc)
                                           (if do-merge
                                             (list [(min l1 l2) (max h1 h2)])
                                             (list [l1 h1])))
                              :merged do-merge))))
                 {:out '() :merged false}
                 (partition 2 1 '([-1 -1]) (sort-by first pairs))))]
      (if (= collapsed pairs)
        collapsed
        (recur collapsed)))))

;; (inc (second (first (collapse (map parse-line (str/split-lines (slurp "input-20.txt")))))))
;; => 19449262



;; Part 2
;; How many values are between the ranges?
;; (apply +
;;  (map
;;   (fn [[[_ h1] [l2 _]]] (- l2 (inc h1)))
;;   (partition 2 1 (collapse (map parse-line (str/split-lines (slurp "input-20.txt")))))))
;; => 119
