(ns advent-11.core)

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

;; Day 11: Hex Ed

;; Part 1
;; How far away is the end of the given hex path?
(defn parse-input [f]
  (str/split (first (read-lines f)) #","))

(defn move [[x y :as position] move]
  (case move
    "n" [x (inc y)]
    "s" [x (dec y)]
    "ne" [(inc x) (if (even? x) y (inc y))]
    "nw" [(dec x) (if (even? x) y (inc y))]
    "se" [(inc x) (if (even? x) (dec y) y)]
    "sw" [(dec x) (if (even? x) (dec y) y)]))

(defn direction [[x1 y1] [x2 y2]]
  (let [gt-op (if (even? x1) >= >)]
   (cond (and (= x1 x2) (= y1 y2)) nil
         (= x2 x1) (if (> y2 y1) "n" "s")
         (> x2 x1) (if (gt-op y2 y1) "ne" "se")
         (< x2 x1) (if (gt-op y2 y1) "nw" "sw"))))

(defn hex-distance [[x1 y1 :as p1] [x2 y2 :as p2]]
  (if (and (= x1 x2) (= y1 y2))
    0
    (inc (hex-distance (move p1 (direction p1 p2)) p2))))

(defn path [start moves]
  (loop [path [start] moves moves]
    (if (empty? moves)
      path
      (recur (conj path (move (last path) (first moves))) (rest moves)))))

(deftest test-path-end-distance
  (is (= 3 (hex-distance [0 0] (last (path [0 0] '("ne" "ne" "ne"))))))
  (is (= 0 (hex-distance [0 0] (last (path [0 0] '("ne" "ne" "sw" "sw"))))))
  (is (= 2 (hex-distance [0 0] (last (path [0 0] '("ne" "ne" "s" "s"))))))
  (is (= 3 (hex-distance [0 0] (last (path [0 0] '("se" "sw" "se" "sw" "sw")))))))

;; (time (hex-distance [0 0] (last (path [0 0] (parse-input large-input)))))
;; "Elapsed time: 1616.3509 msecs"
;; 810



;; Part 2
;; How many steps away is the most distant point in the path?
(defn max-distance [start path]
  (apply max (map (fn [p] (hex-distance start p)) path)))

(deftest test-max-distance
  (is (= 3 (max-distance [0 0] (path [0 0] '("ne" "ne" "ne")))))
  (is (= 2 (max-distance [0 0] (path [0 0] '("ne" "ne" "sw" "sw")))))
  (is (= 2 (max-distance [0 0] (path [0 0] '("ne" "ne" "s" "s")))))
  (is (= 3 (max-distance [0 0] (path [0 0] '("se" "sw" "se" "sw" "sw"))))))

;; (time (max-distance [0 0] (path [0 0] (parse-input large-input))))
;; "Elapsed time: 6581.3496 msecs"
;; 1567
