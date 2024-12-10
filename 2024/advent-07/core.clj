(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn parse-input [input]
  (->> input
       (read-lines)
       (map #(str/split % #":"))
       (map (fn [split] (conj (map first (re-seq #"(\d+)" (second split))) (first split))))
       (map #(map parse-long %))))

;; Part 1
;; Sum the test values from the equations that could be true
;; if * and + operators are placed in the spaces.
(defn build-expressions [numbers]
  (if (= 1 (count numbers))
    (list (list (first numbers)))
    (concat
     (map #(concat [(first numbers) \+] %) (build-expressions (rest numbers)))
     (map #(concat [(first numbers) \*] %) (build-expressions (rest numbers))))))

(defn compute-values [symbols]
  (let [[a op b] (take 3 symbols)
        result (case op
                 \+ (+ a b)
                 \* (* a b))]
    (if (= 3 (count symbols))
      result
      (compute-values (conj (drop 3 symbols) result)))))

(defn sum-valid-equation-test-values [input]
  (->> input
       (parse-input)
       (map #(list (first %) (map compute-values (build-expressions (rest %)))))
       (filter #(some #{(first %)} (second %)))
       (map first)
       (apply +)))


;; (time (sum-valid-equation-test-values small-input))
;; "Elapsed time: 1.1262 msecs"
;; 3749
;; (time (sum-valid-equation-test-values large-input))
;; "Elapsed time: 2980.3827 msecs"
;; 42283209483350


;; Part 2
;; Also consider the || concatenation operator.
(defn build-expressions-2 [numbers]
  (if (= 1 (count numbers))
    (list (list (first numbers)))
    (concat
     (map #(concat [(first numbers) \+] %) (build-expressions-2 (rest numbers)))
     (map #(concat [(first numbers) \*] %) (build-expressions-2 (rest numbers)))
     (map #(concat [(first numbers) \|] %) (build-expressions-2 (rest numbers))))))

(defn compute-values-2 [symbols]
  (let [[a op b] (take 3 symbols)
        result (case op
                 \+ (+ a b)
                 \* (* a b)
                 \| (parse-long (format "%d%d" a b)))]
    (if (= 3 (count symbols))
      result
      (compute-values-2 (conj (drop 3 symbols) result)))))

(defn sum-valid-equation-test-values [input]
  (->> input
       (parse-input)
       (map #(list (first %) (map compute-values-2 (build-expressions-2 (rest %)))))
       (filter #(some #{(first %)} (second %)))
       (map first)
       (apply +)))


;; (time (sum-valid-equation-test-values small-input))
;; "Elapsed time: 2.0217 msecs"
;; 11387
;; (time (sum-valid-equation-test-values large-input))
;; "Elapsed time: 150717.367 msecs"
;; 1026766857276279

