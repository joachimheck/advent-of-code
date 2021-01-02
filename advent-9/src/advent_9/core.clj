(ns advent-9.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "/home/runner/advent-9/small-input.txt")
(def large-input "/home/runner/advent-9/large-input.txt")

;; Part 1
;; Find a number that is not the sum of the n numbers before it.
(defn- read-numbers
  "Returns a vector containing the numbers."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce conj [] (map #(Long/parseLong %) (line-seq rdr)))))

(defn- break-up-numbers [coll preamble-size]
  (if (<= (count coll) preamble-size)
      nil
      (vector (subvec coll 0 preamble-size)
              (get coll preamble-size)
              (subvec coll (+ 1 preamble-size)))))

(defn- sum-of-two?
  "True if n is the sum of any two numbers in coll."
  [n coll]
  (if (empty? coll) false
    (let [plus-first (map #(+ (first coll) %) (rest coll))
          filtered (filter #{n} plus-first)]
      (if (seq filtered) true
          (sum-of-two? n (rest coll))))))

(defn find-non-sum
  "Finds the first number in coll that is not the sum of two of the
  <preamble-size> numbers before it."
  [coll preamble-size]
;  (println coll preamble-size)
  (let [broke-up (break-up-numbers coll preamble-size)]
    (if (nil? broke-up) nil
      (let [preamble (first broke-up)
            num (second broke-up)]
        (if (sum-of-two? num preamble)
          (find-non-sum (vec (rest coll)) preamble-size)
          num
          )))))



;; Part 2
;; Find consecutive numbers that sum to a number.
;; 127
(defn sum-of-first [n coll] (apply + (take n coll)))

(defn find-n-numbers-that-sum-to [x n coll]
  (if (> n (count coll))
      nil
      (if (= x (sum-of-first n coll)) (take n coll)
          (find-n-numbers-that-sum-to x n (rest coll)))))

(defn find-numbers-that-sum-to
  ([x coll] (find-numbers-that-sum-to x 2 coll))
  ([x n coll]
   (let [result (find-n-numbers-that-sum-to x n coll)]
     (if (seq result) result
         (find-numbers-that-sum-to x (inc n) coll)))))

(defn find-encryption-weakness [f preamble-size]
  (let [numbers (read-numbers f)
        bad-number (find-non-sum numbers preamble-size)
        sum-sequence (find-numbers-that-sum-to bad-number numbers)]
    (println "bad number" bad-number "sequence" sum-sequence)
    (+ (apply min sum-sequence) (apply max sum-sequence))))

;  (find-encryption-weakness large-input 25)
;bad number 22477624 sequence (895445 1081046 994874 1039612 1071827 1143075
;                              1239647 1385449 1217317 1331670 1346104 1438234
;                              1422106 1472136 2084599 1485663 1828820)
;=> 2980044
