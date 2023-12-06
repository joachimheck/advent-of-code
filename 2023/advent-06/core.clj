(ns advent-06.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def large-input-2 "large-input-2.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Determine the number of ways you could beat the record in each race.
;; What do you get if you multiply these numbers together?
(defn parse-input [input]
  (let [lines (read-lines input)
        times (map parse-long (rest (str/split (first (filter #(str/starts-with? % "Time") lines)) #" +")))
        distances (map parse-long (rest (str/split (first (filter #(str/starts-with? % "Distance") lines)) #" +")))]
    (map list times distances)))

(defn get-distances [time]
  (for [t (range (inc time))]
    [t (* t (- time t))]))

(defn count-ways-to-win [input]
  (let [state (parse-input input)]
    (apply *
           (for [[t d] state]
             (count (filter #(> (second %) d) (get-distances t)))))))

;; (count-ways-to-win small-input)
;; 288

;; (count-ways-to-win large-input)
;; 1413720



;; Part 2
;; There's only one race - all the numbers are to be concatenated.
;; Time: 45988373
;; Distance: 295173412781210

;; (* t (- time t)) >= d
;; (- (* t time) (* t t)) >= d
;; (* t time) >= (+ (* t t) d)
;; 0 >= (+ (* t t) (- (* t time)) d)

;; quadratic formula:
;; (/ (* 2 a))
(defn quadratic [a b c]
  [(/ (+ (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
   (/ (- (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))])

(defn find-winning-range [time distance]
  (let [roots (quadratic -1 time (- (inc distance)))]
    [(int (Math/ceil (first roots))) (int (Math/floor (second roots)))]))

(defn ways-to-win-product [input]
  (let [state (parse-input input)]
    (apply *
           (for [[t d] state
                 :let [[lo hi] (find-winning-range t d)]]
             (- (inc hi) lo)))))

;; (ways-to-win-product small-input)
;; 288

;; (ways-to-win-product large-input)
;; 1413720

;; (ways-to-win-product large-input-2)
;; 30565288
