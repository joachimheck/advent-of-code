(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many times does a depth measurement increase from the previous one?

;; (count (filter true? (map (fn [[a b]] (> b a)) (partition 2 1 (map #(Long/parseLong %) (read-lines small-input))))))
;; 7

;; (count (filter true? (map (fn [[a b]] (> b a)) (partition 2 1 (map #(Long/parseLong %) (read-lines large-input))))))
;; 1462



;; Part 2
;; How many three-measurement sliding window sums increased?
(defn sliding-windows [f size]
  (map #(apply + %) (partition size 1 (map #(Long/parseLong %) (read-lines f)))))

;; (count (filter true? (map (fn [[a b]] (> b a)) (partition 2 1 (sliding-windows small-input 3)))))
;; 5

;; count (filter true? (map (fn [[a b]] (> b a)) (partition 2 1 (sliding-windows large-input 3)))))
;; 1497
