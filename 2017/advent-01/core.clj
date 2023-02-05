(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 1: Inverse Captcha

;; Part 1
;; Find the sum of all digits that match the next digit (wrapping).
(defn parse-number [s]
  (let [digits (->> s
                    vec
                    (map str)
                    (mapv parse-long))]
    (conj digits (first digits))))

(defn parse-input [f]
  (parse-number (first (read-lines f))))

(defn sum-matching-digits [digits]
  (->> digits
       (partition 2 1)
       (filter (fn [[a b]] (= a b)))
       (map first)
       (apply +)))

;; (time (sum-matching-digits (parse-input large-input)))
;; "Elapsed time: 4.2055 msecs"
;; 1029



;; Part 2
;; Match each digit to the one halfway around instead of the next one.
(defn parse-number-2 [s]
  (->> s
       vec
       (map str)
       (mapv parse-long)))

(defn parse-input-2 [f]
  (parse-number-2 (first (read-lines f))))

(defn sum-matching-digits-2 [digits]
  (let [half (/ (count digits) 2)]
    (->> digits
         (map-indexed (fn [n digit] (list digit (get digits (mod (+ n half) (count digits))))))
         (filter (fn [[a b]] (= a b)))
         (map first)
         (apply +))))

;; (time (sum-matching-digits-2 (parse-input-2 large-input)))
;; "Elapsed time: 3.388699 msecs"
;; 1220
