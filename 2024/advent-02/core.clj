(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

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
  (->> (read-lines input)
       (map #(str/split % #" +"))
       (map #(mapv parse-long %))))


;; Part 1
;; How many reactor reports are safe?
(defn is-safe? [levels]
  (let [pairs (partition 2 1 levels)
        all-increasing? (every? #(> (second %) (first %)) pairs)
        all-decreasing? (every? #(< (second %) (first %)) pairs)
        diff? (every? #(< 0 (abs (- (second %) (first %))) 4) pairs)]
    ;; (list levels all-increasing? all-decreasing? diff? (and (or all-increasing? all-decreasing?) diff?))
    (and (or all-increasing? all-decreasing?) diff?)))

(defn count-safe-reports [reports]
  (->> (parse-input reports)
       (map #(is-safe? %))
       (filter true?)
       (count)))


;; (time (count-safe-reports small-input))
;; "Elapsed time: 0.7231 msecs"
;; 2
;; (time (count-safe-reports large-input))
;; "Elapsed time: 18.0603 msecs"
;; 472



;; Part 2
;; Count again, taking into account the Problem Dampener, which allows
;; a single unsafe level to be ignored.
(defn dampen-levels [levels]
  (for [n (range (count levels))] (apply conj (subvec levels 0 n) (subvec levels (inc n)))))

(defn is-safe-dampened? [levels]
  (or (is-safe? levels)
      (some true? (map #(is-safe? %) (dampen-levels levels)))))

(defn count-safe-reports-dampened [reports]
  (->> (parse-input reports)
       (map #(is-safe-dampened? %))
       (filter true?)
       (count)))

;; (time (count-safe-reports-dampened small-input))
;; "Elapsed time: 0.9736 msecs"
;; 4
;; (time (count-safe-reports-dampened large-input))
;; "Elapsed time: 45.3845 msecs"
;; 520
