(ns advent-1.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn- read-numbers [f]
  (map #(Long/parseLong %) (read-lines f)))

;; Part 1
;; Find two entries that sum to 2020
;; Part 2
;; Find three entries that sum to 2020

(defn find-matching-tuple [ns total tuple-size]
;;  (println "find" ns total tuple-size)
  (if (empty? ns) nil
      (if (= 1 tuple-size) (some #{total} ns)
          (let [match (find-matching-tuple (rest ns) (- total (first ns)) (- tuple-size 1))]
            (if match
              (flatten (list (first ns) match))
              ;; Try with the next number
              (find-matching-tuple (rest ns) total tuple-size))))))

(defn multiply-matching-tuple [ns total tuple-size]
  (let [tuple (find-matching-tuple ns total tuple-size)
        product (list (reduce * 1 tuple))]
    (println tuple '(=>) product)
    product))




;; Use filtering (like https://www.reddit.com/r/adventofcode/comments/k4e4lm/2020_day_1_solutions/gexfshn)
(defn find-tuple-by-filtering [ns total tuple-size]
  (if (= 2 tuple-size)
    (filter #(some #{(- total %)} ns) ns)
    (filter #(seq (find-tuple-by-filtering ns (- total %) (- tuple-size 1))) ns)))
