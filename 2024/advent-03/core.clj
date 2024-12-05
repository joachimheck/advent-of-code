(ns advent-03.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")
(def small-input-2 "small-input-2.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (read-lines input))


;; Part 1
;; Sum the results of the multiplications in the corrupted memory.
(defn parse-line [l]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" l))

(defn multiply-memory [input]
  (->> input
       (parse-input)
       (map parse-line)
       (apply concat)
       (map rest)
       (map #(map parse-long %))
       (map #(apply * %))
       (apply +)))


;; (time (multiply-memory small-input))
;; "Elapsed time: 0.5204 msecs"
;; 161
;; (time (multiply-memory large-input))
;; "Elapsed time: 5.7034 msecs"
;; 187825547


;; Part 2
;; Take into account the do() and don't() instructions.
(defn parse-line-2 [l]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|(do)\(\)|(don't)\(\)" l))

(defn remove-donts [l]
  (second
   (reduce (fn [[do? muls] val]
             (cond (= "do()" (first val))
                   [true muls]
                   (= "don't()" (first val))
                   [false muls]
                   :else
                   (if do?
                     [do? (conj muls val)]
                     [do? muls])))
           [true []]
           l)))

(defn multiply-memory-2 [input]
  (->> input
       (parse-input)
       (map parse-line-2)
       (apply concat)
       (remove-donts)
       (map #(subvec % 1 3))
       (map #(map parse-long %))
       (map #(apply * %))
       (apply +)))

;; (time (multiply-memory-2 small-input-2))
;; "Elapsed time: 0.7994 msecs"
;; 48
;; (time (multiply-memory-2 large-input))
;; "Elapsed time: 4.1374 msecs"
;; 85508223
