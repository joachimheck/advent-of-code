(ns day-16.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Compute some random-ish data, and its checksum.
(def test-inputs
  '("1" "0" "11111" "111100001010"))

(def real-input "11011110011011101")

(defn dragon [input]
  (let [a input
        b (-> a
              str/reverse
              (str/replace "0" "x")
              (str/replace "1" "0")
              (str/replace "x" "1"))]
    (str/join (list a "0" b))))

(defn checksum-once [input]
  (str/join
   (map (fn [[a b]] (if (= a b) "1" "0"))
        (partition 2 input))))

(defn checksum [s]
  (loop [input s]
    (if (odd? (count input)) input
        (recur (checksum-once input)))))

(defn dragon-fill [seed length]
  (loop [val seed length length]
    (if (>= (count val) length)
      (subs val 0 length)
      (recur (dragon val) length))))

;; (checksum (dragon-fill real-input 272))
;; => "00000100100001100"



;; Part 2
;; Fill a much bigger disk with data.
;; (checksum (dragon-fill real-input 35651584))
;; => "00011010100010010"
