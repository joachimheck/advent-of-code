(ns day-06.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Decipher the noisy message from Santa.
(def test-input '("eedadn"
                  "drvtee"
                  "eandsr"
                  "raavrd"
                  "atevrs"
                  "tsrnev"
                  "sdttsa"
                  "rasrtv"
                  "nssdts"
                  "ntnada"
                  "svetve"
                  "tesnvt"
                  "vntsnd"
                  "vrdear"
                  "dvrsen"
                  "enarar"))

(defn extract-message [lines]
  (str/join
   (for [i (range (count (first lines)))]
     (first (first (reverse (sort-by second (frequencies (map #(nth % i) lines)))))))))

;; (extract-message test-input)
;; => "easter"

;; (extract-message (str/split-lines (slurp "input-06.txt")))
;; => "qrqlznrl"



;; Part 2
(defn extract-message-2 [lines]
  (str/join
   (for [i (range (count (first lines)))]
     (first (first (sort-by second (frequencies (map #(nth % i) lines))))))))

;; (extract-message-2 test-input)
;; => "advent"

;;(extract-message-2 (str/split-lines (slurp "input-06.txt")))
;; => "kgzdfaon"
