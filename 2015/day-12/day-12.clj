(ns day-12.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Find all the numbers in the list of expressions, and sum them.

(def test-input
  '("[1,2,3]"
    "{\"a\":2,\"b\":4}"
    "[[[3]]]"
    "{\"a\":{\"b\":4},\"c\":-1}"
    "{\"a\":[-1,1]}"
    "[-1,{\"a\":1}]"
    "[]"
    "{}"
    ))

(def test-input-with-sum
  '(("[1,2,3]" 6)
    ("{\"a\":2,\"b\":4}" 6)
    ("[[[3]]]" 3)
    ("{\"a\":{\"b\":4},\"c\":-1}" 3)
    ("{\"a\":[-1,1]}" 0)
    ("[-1,{\"a\":1}]" 0)
    ("[]" 0)
    ("{}" 0)
    ))

(defn eval-json [json-string]
  (load-string (str/replace json-string ":" " ")))

(defn get-all-values [exp]
  (cond (number? exp) (list exp)
        (string? exp) (list 0)
        (vector? exp) (flatten (for [e exp]
                                 (get-all-values e)))
        (map? exp) (flatten
                    (for [[k v] exp]
                      (get-all-values v)))))

(defn sum-all-numbers [getter lines]
  (->> lines
       (map eval-json)
       (map getter)
       (reduce concat)
       (reduce +)
       ))

(sum-all-numbers get-all-values test-input)

;; (time (sum-all-numbers get-all-values (list (slurp "input-12.txt"))))
;; => 119433
;; "Elapsed time: 32.8037 msecs"

(def test-input-2 '("[1,2,3]"
                    "[1,{\"c\":\"red\",\"b\":2},3]"
                    "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"
                    "[1,\"red\",5]"))

(def test-input-2-with-sum '(("[1,2,3]" 6)
                             ("[1,{\"c\":\"red\",\"b\":2},3]" 4)
                             ("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" 0)
                             ("[1,\"red\",5]" 6)))

(defn get-all-values-but-red [exp]
  (cond (number? exp) (list exp)
        (string? exp) '(0)
        (vector? exp) (flatten (for [e exp]
                                 (get-all-values-but-red e)))
        (map? exp)
        (if (some #{"red"} (vals exp))
          '(0)
          (flatten
           (for [[k v] exp]
             (get-all-values-but-red v))))
        ))

(->> test-input-2
     (map eval-json)
     (map get-all-values-but-red)
     )

(if (some #{"red"} (vals {"c" "red" "b" 2})) "true" "false")

(sum-all-numbers get-all-values-but-red test-input-2)
;; (time (sum-all-numbers get-all-values-but-red (list (slurp "input-12.txt"))))
;; => 68466
;; "Elapsed time: 39.336201 msecs"
