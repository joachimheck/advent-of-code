(ns day-10.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Run length encoding.

(def test-input '("1" "11" "21" "1211" "111221"))

(defn run-length-encode [s]
  (->> s
       (partition-by identity)
       (map #(str (count %) (first %)))
       (apply concat)
       (apply str)
       ))

(defn repeat-encode [s n]
  (nth (iterate run-length-encode s) n))

;; (time (count (repeat-encode "1113122113" 40)))
;; => 360154
;; "Elapsed time: 1228.1741 msecs"


;; Part 2
;; (time (count (repeat-encode "1113122113" 50)))
;; => 5103798
;; "Elapsed time: 16846.7282 msecs"
