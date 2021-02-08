(ns day-18.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(defn trap? [prev-tiles] (#{"^^." ".^^" "^.." "..^"} prev-tiles))

(defn next-row [row]
  (let [full-row (str/join (list "." row "."))]
    (str/join
     (map
      (fn [prev-tiles] (if (trap? (str/join prev-tiles)) "^" ".")) 
      (partition 3 1 full-row)))))

(defn safe-tiles [start-row rows]
  (count (filter #{\.} (apply concat (take rows (iterate next-row start-row))))))

(def test-input ".^^.^.^^^^")

(def real-input "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^..")


;; (safe-tiles real-input 40)
;; => 1963



;; Part 2
;; A much larger space.
;; (safe-tiles real-input 400000)
;; => 20009568
