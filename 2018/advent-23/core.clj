(ns advent-23.core)

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

;; Day 23: Experimental Emergency Teleportation

;; Part 1
;; How many nanobots are in range of the strongest nanobot?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(let [[_ x y z r] (re-matches #"pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)" %)] {(mapv parse-long [x y z]) (parse-long r)}))
       (apply merge)))

(defn get-strongest-bot [bots]
  (apply max-key val bots))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2)) (abs (- z1 z2))))

(defn in-range [[pos range] bots]
  (filter (fn [[t-pos _]] (<= (distance pos t-pos) range)) bots))

(defn count-in-range-of-strongest [bots]
  (let [strongest (get-strongest-bot bots)]
    (count (in-range strongest bots))))

;; (time (count-in-range-of-strongest (parse-input small-input)))
;; "Elapsed time: 0.526101 msecs"
;; 7

;; (time (count-in-range-of-strongest (parse-input large-input)))
;; "Elapsed time: 5.3118 msecs"
;; 599



;; Part 2
;; What's the distance from the origin to the closest point in range of the most nanobots?
(defn get-bounds [bots]
  (reduce (fn [[[minx miny minz] [maxx maxy maxz]] [[x y z] _]]
            [[(min minx x) (min miny y) (min minz z)] [(max maxx x) (max maxy y) (max maxz z)]])
          [[Integer/MAX_VALUE Integer/MAX_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]]
          bots))

(defn volume-in-range [[x y z] r]
  r = 0 : 1
  r = 1 : 7 (6 + 1)
  r = 2 : 19 (12 + 7)
)
