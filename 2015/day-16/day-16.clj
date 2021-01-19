(ns day-16.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Which Aunt Sue got me a gift?


(defn parse-line [line]
 (let [pattern #"(Sue (\d+))|(([a-z]+): ([\d]+))+"
       matches (re-seq pattern line)]
   (assoc {}
    (nth (first matches) 2)
    (reduce (partial apply assoc) {} (map #(list (nth % 4) (nth % 5)) (rest matches))))))

(defn read-input [f]
  (map parse-line (str/split-lines (slurp f))))

(read-input "input-16.txt")

