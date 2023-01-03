(ns advent-03.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Compute power consumption from gamma and epsilon rates.

(defn parse-line [line]
  (mapv #(str %) (flatten (partition 1 1 line))))

(defn transpose [v]
  (apply mapv vector v))

(defn parse-input [f]
  (transpose (map parse-line (read-lines f))))

(defn most-common-value [v]
  (let [freqs (frequencies v)]
    (if (> (get freqs "1") (get freqs "0")) "1" "0")))

(defn get-rates [f]
  (let [gamma-digits (map most-common-value (parse-input f))
        epsilon-digits (replace {"1" "0" "0" "1"} gamma-digits)]
    (list (Long/parseLong (str/join gamma-digits) 2)
          (Long/parseLong (str/join epsilon-digits) 2))))


;; (apply * (get-rates small-input))
;; 198
;; (apply * (get-rates large-input))
;; 3429254



;; Part 2
;; Compute the oxygen generator and CO2 scrubber ratings.
(defn most-common-value-oxygen [v]
  (let [freqs (conj {"0" 0 "1" 0} (frequencies v))]
    (if (>= (get freqs "1") (get freqs "0")) "1" "0")))

(defn most-common-value-co2 [v]
  (let [freqs (conj {"0" 0 "1" 0} (frequencies v))]
    (if (<= (get freqs "0") (get freqs "1")) "0" "1")))

(defn compute-oxygen-rating [f]
  (Long/parseLong
   (loop [digit 0
          input-lines (read-lines f)]
     (let [inputs (map parse-line input-lines)
           transposed-inputs (transpose inputs)
           mcvs (mapv most-common-value-oxygen transposed-inputs)
           new-inputs (filter #(= (subs % digit (inc digit)) (mcvs digit)) input-lines)]
       (if (= (count new-inputs) 1)
         (first new-inputs)
         (recur (inc digit)
                new-inputs))))
   2))

(defn compute-co2-rating [f]
  (Long/parseLong
   (loop [digit 0
          input-lines (read-lines f)]
     (let [inputs (map parse-line input-lines)
           transposed-inputs (transpose inputs)
           mcvs (mapv most-common-value-co2 transposed-inputs)
           new-inputs (filter #(= (subs % digit (inc digit)) (mcvs digit)) input-lines)]
       (if (= (count new-inputs) 1)
         (first new-inputs)
         (recur (inc digit)
                new-inputs))))
   2))

;; (* (compute-oxygen-rating small-input) (compute-co2-rating small-input))
;; 230
;; (* (compute-oxygen-rating large-input) (compute-co2-rating large-input))
;; 5410338

