(ns advent-21.core)

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

;; Day 21: Fractal Art

;; Part 1
;; How many pixels are lit after five iterations?
(def initial-pattern [(vec ".#.")
                      (vec "..#")
                      (vec "###")])

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"(.+) => (.+)" %))
       (map rest)
       (map vec)
       (into {})))

(defn split-pattern [pattern]
  (let [pattern-size (count (first pattern))
        new-size (if (= 0 (mod pattern-size 2)) 2 3)
        rows-split (mapv #(partition new-size %) pattern)
        steps (range (quot pattern-size new-size))]
    (apply concat
     (for [j steps]
       (for [i steps]
         (vec
          (for [k (range new-size)]
            (vec (nth (nth rows-split (+ (* j new-size) k)) i)))))))))

(defn flip-pattern [pattern]
  (map vec (list pattern
                 (reverse pattern)
                 (map vec (map reverse pattern))
                 (map vec (map reverse (reverse pattern))))))

(defn rotate-pattern [pattern]
  (let [pattern-size (count (first pattern))]
    (if (= pattern-size 2)
      (list pattern
            [[(get-in pattern [1 0]) (get-in pattern [0 0])]
             [(get-in pattern [1 1]) (get-in pattern [0 1])]]
            [[(get-in pattern [1 1]) (get-in pattern [1 0])]
             [(get-in pattern [0 1]) (get-in pattern [0 0])]]
            [[(get-in pattern [0 1]) (get-in pattern [1 1])]
             [(get-in pattern [0 0]) (get-in pattern [1 0])]])
      (list pattern
            [[(get-in pattern [2 0]) (get-in pattern [1 0]) (get-in pattern [0 0])]
             [(get-in pattern [2 1]) (get-in pattern [1 1]) (get-in pattern [0 1])]
             [(get-in pattern [2 2]) (get-in pattern [1 2]) (get-in pattern [0 2])]]
            [[(get-in pattern [2 2]) (get-in pattern [2 1]) (get-in pattern [2 0])]
             [(get-in pattern [1 2]) (get-in pattern [1 1]) (get-in pattern [1 0])]
             [(get-in pattern [0 2]) (get-in pattern [0 1]) (get-in pattern [0 0])]]
            [[(get-in pattern [0 2]) (get-in pattern [1 2]) (get-in pattern [2 2])]
             [(get-in pattern [0 1]) (get-in pattern [1 1]) (get-in pattern [2 1])]
             [(get-in pattern [0 0]) (get-in pattern [1 0]) (get-in pattern [2 0])]]))))

(defn rotate-and-flip [pattern]
  (distinct (apply concat (map rotate-pattern (flip-pattern pattern)))))

;; TODO: stitch sub-patterns back together into a larger pattern.

(defn enhance [pattern rules]
  )
