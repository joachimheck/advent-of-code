(ns day-03.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Identify invalid triangles.
(defn read-input [f]
  (map
   (fn [line] (let [[_ a b c] (re-matches #"\s*(\d+)\s+(\d+)\s+(\d+)\s*" line)]
                [(Long/parseLong a) (Long/parseLong b) (Long/parseLong c)]
                ))
   (str/split-lines (slurp f))))


(defn valid? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ c a) b)))

(reduce (fn [cnt triangle] (if (valid? triangle) (inc cnt) cnt)) 0 (read-input "input-03.txt"))
;; => 1050


;; Part 2
;; Triangles are listed vertically
(def test-input
  '([101 301 501] 
    [102 302 502] 
    [103 303 503] 
    [201 401 601] 
    [202 402 602] 
    [203 403 603]))


(defn read-vertically [f]
  (mapcat
   (fn [[[a d g] [b e h] [c f i]]]
     (list [a b c] [d e f] [g h i]))
   (partition 3 (read-input f))))

(reduce (fn [cnt triangle] (if (valid? triangle) (inc cnt) cnt)) 0 (read-vertically "input-03.txt"))
;; => 1921
