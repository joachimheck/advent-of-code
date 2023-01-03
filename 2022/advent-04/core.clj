(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many elf pairs have one assignment fully contained within another?
(defn match-assignments [assignments]
  (let [assignment-regex #"(\d+)-(\d+),(\d+)-(\d+)"
        match (re-matcher assignment-regex assignments)]
    (re-find match)
    (partition 2 (map #(Long/parseLong %) (drop 1 (re-groups match))))))

(defn fully-contains? [[[a1 b1] [a2 b2]]]
  (or (and (<= a1 a2) (>= b1 b2)) (and (<= a2 a1) (>= b2 b1))))

;; (count (filter true? (map fully-contains? (map match-assignments (read-lines small-input)))))
;; 2
;; (count (filter true? (map fully-contains? (map match-assignments (read-lines large-input)))))
;; 560


;; Part 2
;; How many elf pairs have overlapping assignments?
(defn overlaps? [[[a1 b1] [a2 b2]]]
  (or (and (>= a1 a2) (<= a1 b2))
      (and (>= b1 a2) (<= b1 b2))
      (and (>= a2 a1) (<= a2 b1))
      (and (>= b2 a1) (<= b2 b1))))

;; (count (filter true? (map overlaps? (map match-assignments (read-lines small-input)))))
;; 4
;; (count (filter true? (map overlaps? (map match-assignments (read-lines large-input)))))
;; 839
