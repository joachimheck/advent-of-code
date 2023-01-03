(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; At how many points to lines of hydrothermal vents overlap?
(defn parse-line [line]
  (let [[x1 y1 x2 y2] (->> line
                           (re-find #"(\d+),(\d+) \-\> (\d+),(\d+)")
                           rest
                           (mapv parse-long))]
    (list [x1 y1] [x2 y2])))

(defn parse-input [f]
  (map parse-line (read-lines f)))

(defn non-diagonal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(def diagonal? (complement non-diagonal?))

(defn get-diff [a b]
  (cond (= a b) 0
        (< a b) 1
        (> a b) -1))

(defn get-points [[[x1 y1] [x2 y2] :as line]]
  (let [x-diff (get-diff x1 x2)
          y-diff (get-diff y1 y2)]
      (loop [[x y] [x1 y1]
             points []]
        (if (= [x y] [x2 y2])
          (conj points [x y])
          (recur [(+ x x-diff) (+ y y-diff)] (conj points [x y]))))))

(defn count-overlap-points [f]
  (->> (parse-input f)
       (filter non-diagonal?)
       (map (fn [[p1 p2]] (get-points [p1 p2])))
       (apply concat)
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (count)))

;; (time (count-overlap-points small-input))
;; "Elapsed time: 1.1604 msecs"
;; 5
;; (time (count-overlap-points large-input))
;; "Elapsed time: 260.299 msecs"
;; 6005



;; Part 2
;; Include diagonal lines, which are always at 45 degrees.

(defn count-overlap-points-2 [f]
  (->> (parse-input f)
       (map (fn [[p1 p2]] (get-points [p1 p2])))
       (apply concat)
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (count)))

;; (time (count-overlap-points-2 small-input))
;; "Elapsed time: 1.1199 msecs"
;; 12
;; (time (count-overlap-points-2 large-input))
;; "Elapsed time: 383.1496 msecs"
;; 23864
