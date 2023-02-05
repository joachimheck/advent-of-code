(ns advent-02.core)

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

;; Day 2: Corruption Checksum

;; Part 1
;; Determine the checksum of the spreadsheet - the sum of the differences of the maxes and mins.
(defn parse-input [f]
  (->> f
       read-lines
       (map #(str/join (list "[" % "]")))
       (map load-string)))

(defn compute-checksum [rows]
  (->> rows
       (map #(list (apply max %) (apply min %)))
       (map #(apply - %))
       (apply +)))

;; (time (compute-checksum (parse-input small-input)))
;; "Elapsed time: 1.7242 msecs"
;; 18

;; (time (compute-checksum (parse-input large-input)))
;; "Elapsed time: 8.9967 msecs"
;; 44216



;; Part 2
;; Sum the evenly divisible numbers of each row.
(defn pairs [coll]
  (cond (empty? coll) nil
        (= 1 (count coll)) nil
        (= 2 (count coll)) (list coll)
        :else (concat (map #(list (first coll) %) (rest coll)) (pairs (rest coll)))))

(defn pairs-and-inverses [coll]
  (let [pairs (pairs coll)]
    (concat pairs (map (fn [[a b]] (list b a)) pairs))))

(def small-input-2 '([5 9 2 8]
                     [9 4 7 3]
                     [3 8 6 5]))

(defn compute-checksum-2 [rows]
  (->> rows
       (mapcat pairs-and-inverses)
       (map (fn [[a b]] (/ a b)))
       (filter integer?)
       (apply +)))

;; (time (compute-checksum-2 (parse-input large-input)))
;; "Elapsed time: 21.3718 msecs"
;; 320
