(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn- partition-lines [f]
  (filter #(not (= % '(""))) (partition-by empty? (read-lines f))))

(defn- sum-numstrings [l]
  (reduce + (map #(Long/parseLong %) l)))

;; Part 1
;; How many calories are carried by the elf carrying the most calories?

;; (apply max (map sum-numstrings (partition-lines small-input)))
;; 24000

;; (apply max (map sum-numstrings (partition-lines large-input)))
;; 68923


;; Part 2
;; How many calories are carried by the three elves carrying the most calories?

;; (reduce + (take 3 (sort > (map sum-numstrings (partition-lines small-input)))))
;; 45000

;; (reduce + (take 3 (sort > (map sum-numstrings (partition-lines large-input)))))
;; 200044
