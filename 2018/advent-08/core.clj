(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 8: Memory Manouver

;; Part 1
;; What is the sum of all the metadata entries?
(defn parse-input [f]
  (mapv parse-long (str/split (first (read-lines f)) #" ")))

(defn build-tree [t-vec]
  (let [child-count (first t-vec)
        meta-count (second t-vec)]
    (loop [rest-vec (drop 2 t-vec) i 0 children []]
      (if (= i child-count)
        (list (list children (take meta-count rest-vec)) (drop meta-count rest-vec))
        (let [[node new-rest-vec] (build-tree rest-vec)]
          (recur new-rest-vec (inc i) (conj children node)))))))

(defn count-metadata [t-vec]
  (apply + (flatten (build-tree t-vec))))

;; (time (count-metadata (parse-input small-input)))
;; "Elapsed time: 0.9546 msecs"
;; 138

;; (time (count-metadata (parse-input large-input)))
;; "Elapsed time: 71.1256 msecs"
;; 46578



;; Part 2
;; Find the value of the root node.
(defn value [[children metadata :as node]]
  (if (empty? children)
    (apply + metadata)
    (let [child-count (count children)
          indices (filter #(< -1 % child-count) (map dec metadata))]
      (apply + (map #(value (nth children %)) indices)))))

;; (time (value (first (build-tree (parse-input small-input)))))
;; "Elapsed time: 0.8314 msecs"
;; 66

;; (time (value (first (build-tree (parse-input large-input)))))
;; "Elapsed time: 17.2722 msecs"
;; 31251
