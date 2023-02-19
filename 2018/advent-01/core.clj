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

;; Day 1: Chronal Calibration

;; Part 1
;; What is the frequency after the specified changes have been applied?
(defn parse-input [f]
 (->> f
      (read-lines)
      (map parse-long)))

(defn frequency-after-changes [changes]
  (apply + changes))

;; (frequency-after-changes (parse-input large-input))
;; 445



;; Part 2
;; What is the first frequency the device reaches twice?
(defn first-repeat [changes]
  (reduce (fn [[seen sum] f]
            (let [new-sum (+ sum f)]
              (if (some seen (list new-sum))
                (reduced new-sum)
                [(conj seen new-sum) new-sum])))
          [#{0} 0]
          (apply concat (repeat changes))))

;; (time (first-repeat (parse-input large-input)))
;; "Elapsed time: 246.5818 msecs"
;; 219
