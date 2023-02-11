(ns advent-15.core)

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

;; Day 15: Dueling Generators

;; Part 1
;; How many times do the two generators produce matching numbers?
(def small-input {\A 65 \B 8921})
(def large-input {\A 699 \B 124})

(def factors {\A 16807 \B 48271})

(defn generate-number [[name prev]]
  [name (mod (* prev (get factors name)) Integer/MAX_VALUE)])

(defn generate-numbers [input]
  {\A (mod (* (get input \A) (get factors \A)) Integer/MAX_VALUE)
   \B (mod (* (get input \B) (get factors \B)) Integer/MAX_VALUE)})

(defn lowest-bits-equal? [a b]
  (= (bit-and 0xffff a) (bit-and 0xffff b)))

(defn count-matching-numbers [input generator-fn iterations]
  (loop [i 0 prevs input matches 0]
    (if (= i iterations)
      matches
      (let [new-numbers (generator-fn prevs)
            lowest-equal (apply lowest-bits-equal? (vals new-numbers))
            new-matches (if lowest-equal (inc matches) matches)]
        (recur (inc i) new-numbers new-matches)))))

;; (time (count-matching-numbers small-input generate-numbers 40000000))
;; "Elapsed time: 40541.2683 msecs"
;; 588

;; (time (count-matching-numbers large-input generate-numbers 40000000))
;; "Elapsed time: 41812.0805 msecs"
;; 600



;; Part 2
;; How many matching numbers are there using the new generator logic?
(def multiples {\A 4 \B 8})

(defn compute-next-number [[prev factor]]
  [(mod (* prev factor) Integer/MAX_VALUE) factor])

(defn even-multiple [n x]
  (integer? (/ n x)))

(defn even-multiple-bits [n x]
  (if (= x 4)
    (= 0 (bit-and n 3))
    (= 0 (bit-and n 7))))

(defn find-next-multiple [prev factor multiple]
  (first (first (drop-while #(not (even-multiple-bits (first %) multiple)) (drop 1 (iterate compute-next-number (list prev factor)))))))

(defn generate-numbers-2 [input]
  {\A (find-next-multiple (get input \A) (get factors \A) (get multiples \A))
   \B (find-next-multiple (get input \B) (get factors \B) (get multiples \B))})

;; (time (count-matching-numbers small-input generate-numbers-2 5000000))
;; "Elapsed time: 51322.767199 msecs"
;; 309

;; (time (count-matching-numbers large-input generate-numbers-2 5000000))
;; "Elapsed time: 51756.6964 msecs"
;; 313
