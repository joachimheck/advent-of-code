(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Flawed Frequency Transmission

;; Part 1
;; After 100 phases of FFT, what are the first eight digits in the final output list?
(defn parse-line [line]
  (map parse-long (map str (vec line))))

(defn parse-input [f]
  (parse-line (first (read-lines f))))

(def base-pattern [0 1 0 -1])

(defn generate-pattern [length output-digit]
  (take length (drop 1 (apply concat (map #(repeat output-digit %) (apply concat (repeat base-pattern)))))))

(defn apply-pattern [signal pattern]
  (parse-long (str (last (str (apply + (map * signal pattern)))))))

(defn run-fft-phase [signal]
  (for [i (range (count signal))]
    (apply-pattern signal (generate-pattern (count signal) (inc i)))))

(defn run-fft [signal phases]
  (nth (iterate run-fft-phase signal) phases))

(defn first-8-after-fft [signal phases]
  (parse-long
   (str/join
    (map str
         (take 8
               (run-fft signal phases))))))

(deftest test-first-8-after-fft
  (is (= 1029498 (first-8-after-fft (parse-line "12345678") 4)))
  (is (= 24176176 (first-8-after-fft (parse-line "80871224585914546619083218645595") 100)))
  (is (= 73745418 (first-8-after-fft (parse-line "19617804207202209144916044189917") 100)))
  (is (= 52432133 (first-8-after-fft (parse-line "69317163492948606335995924319873") 100))))

;; (first-8-after-fft (parse-line "12345678") 4)
;; 1029498
;; (first-8-after-fft (parse-input large-input) 100)
;; 94960436



;; Part 2
;; 
