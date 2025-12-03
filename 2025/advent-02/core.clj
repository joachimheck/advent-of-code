(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

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
  (->> (str/split (first (read-lines input)) #",")
       (map #(re-matches #"(\d+)-(\d+)" %))
       (map rest)
       (map #(map parse-long %))))

;; Part 1
;; What is the sum of the invalid id numbers in the given ranges?
(defn is-invalid-id? [id]
  (let [id-string (format "%d" id)
        id-length (count id-string)]
    (if (odd? id-length)
      false
      (let [half-length (quot id-length 2)
            [start end] (partition half-length id-string)]
        (= start end)))))

(defn count-invalid-ids [input]
  (->> (parse-input input)
       (map #(range (first %) (inc (second %))))
       (apply concat)
       (filter is-invalid-id?)
       (apply +)))


;; (time (count-invalid-ids small-input))
;; "Elapsed time: 1.1148 msecs"
;; 1227775554
;; (time (count-invalid-ids large-input))
;; "Elapsed time: 3455.7184 msecs"
;; 28146997880
