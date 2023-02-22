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
    (loop [rest-vec (drop 2 t-vec) i 0 children '()]
      (if (= i child-count)
        (list child-count meta-count children rest-vec)
        (let [subtree (build-tree rest-vec)]
          (recur (last subtree) (inc i) (conj children subtree)))))))
