(ns advent-24.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])
(require '[clojure.math.numeric-tower :as math :exclude [abs]])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 24: Planet of Discord

;; Part 1
;; What is the biodiversity rating for the first layout that appears twice?

(defn compute-biodiversity [grid]
  (reduce (fn [acc c]
            (+ (* 2 acc) (if (= c \#) 1 0)))
          0
          (reverse (flatten grid))))

(defn parse-input [f]
  (compute-biodiversity (mapv vec (read-lines f))))

(defn neighbors [[x y]]
  (filter (fn [[x y]] (and (<= 0 x 4) (<= 0 y 4)))
          (list [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y])))

(defn get-space [grid [x y]]
  (bit-test grid (+ (* 5 y) x)))

(def coordinates (reverse (for [j (range 5) i (range 5)] [i j])))

(defn process-grid [grid]
  (reduce (fn [acc [x y]]
            (let [current (get-space grid [x y])
                  neighbor-bugs (count (filter true? (map #(get-space grid %) (neighbors [x y]))))
                  new-state (cond (and current (not= 1 neighbor-bugs))
                                  0
                                  (and (not current) (<= 1 neighbor-bugs 2))
                                  1
                                  :else
                                  (if current 1 0))]
              (+ (* 2 acc) new-state)))
          0
          coordinates))

(defn draw-grid [grid]
  (str/join
   "\n"
   (for [j (range 5)]
     (str/join
      (for [i (range 5)]
        (if (get-space grid [i j]) \# \.))))))

(defn process-until-repeat [grid]
  (loop [grid grid
         previous #{}]
    (if (some #{grid} previous)
        (do
          (println (draw-grid grid))
          grid)
        (recur (process-grid grid) (conj previous grid)))))

;; (time (process-until-repeat (parse-input small-input)))
;; .....
;; .....
;; .....
;; #....
;; .#...
;; "Elapsed time: 21.8248 msecs"
;; 2129920
;; (time (process-until-repeat (parse-input large-input)))
;; #.###
;; #....
;; #.#..
;; #....
;; #...#
;; "Elapsed time: 10.1229 msecs"
;; 17863741



;; Part 2
;; We're in Plutonian Recursive Space. How many bugs are present after 200 minutes?

(defn neighbors-r [[x y z]]
  ;; (if (nil? z) (println "neighbors-r" x y z))
  (let [id (+ (* 5 y) x)
        basic-neighbors (list [x (dec y) z] [(inc x) y z] [x (inc y) z] [(dec x) y z])
        without-center (remove #{[2 2 z]} basic-neighbors)
        recursive-neighbors (concat without-center
                                    (cond (= 7 id) (list [0 0 (inc z)] [1 0 (inc z)] [2 0 (inc z)] [3 0 (inc z)] [4 0 (inc z)])
                                          (= 11 id) (list [0 0 (inc z)] [0 1 (inc z)] [0 2 (inc z)] [0 3 (inc z)] [0 4 (inc z)])
                                          (= 13 id) (list [4 0 (inc z)] [4 1 (inc z)] [4 2 (inc z)] [4 3 (inc z)] [4 4 (inc z)])
                                          (= 17 id) (list [0 4 (inc z)] [1 4 (inc z)] [2 4 (inc z)] [3 4 (inc z)] [4 4 (inc z)])
                                          (= 0 id) (list [2 1 (dec z)] [1 2 (dec z)])
                                          (= 4 id) (list [2 1 (dec z)] [3 2 (dec z)])
                                          (= 20 id) (list [1 2 (dec z)] [2 3 (dec z)])
                                          (= 24 id) (list [3 2 (dec z)] [2 3 (dec z)])
                                          (= 0 y) (list [2 1 (dec z)])
                                          (= 4 y) (list [2 3 (dec z)])
                                          (= 0 x) (list [1 2 (dec z)])
                                          (= 4 x) (list [3 2 (dec z)])))]
    (filter (fn [[x y z]] (and (<= 0 x 4) (<= 0 y 4)))
            recursive-neighbors)))

(deftest test-neighbors-r
  (is (= #{[3 2 0] [4 3 0] [3 4 0] [2 3 0]} (set (neighbors-r [3 3 0]))))
  (is (= #{[1 0 0] [2 1 0] [1 2 0] [0 1 0]} (set (neighbors-r [1 1 0]))))
  (is (= #{[2 0 0] [3 1 0] [4 0 0] [2 1 -1]} (set (neighbors-r [3 0 0]))))
  (is (= #{[3 0 0] [4 1 0] [2 1 -1] [3 2 -1]} (set (neighbors-r [4 0 0]))))
  (is (= #{[3 1 0] [4 2 0] [3 3 0] [4 0 1] [4 1 1] [4 2 1] [4 3 1] [4 4 1]} (set (neighbors-r [3 2 0])))))

(defn get-space-r [grids [x y l]]
  (bit-test (get grids l 0) (+ (* 5 y) x)))

(defn process-grid-r [grids level]
  (reduce (fn [acc [x y]]
            (let [current (get-space (get grids level 0) [x y])
                  neighbor-bugs (count (filter true? (map #(get-space-r grids %) (neighbors-r [x y level]))))
                  new-state (cond (= [x y] [2 2])
                                  0
                                  (and current (not= 1 neighbor-bugs))
                                  0
                                  (and (not current) (<= 1 neighbor-bugs 2))
                                  1
                                  :else
                                  (if current 1 0))]
              (+ (* 2 acc) new-state)))
          0
          coordinates))

(defn process-grids [grids]
  ;; (println "process-grids" (keys grids))
  (let [min-level (dec (apply min (keys grids)))
        max-level (inc (apply max (keys grids)))
        processed (reduce (fn [acc l] (assoc acc l (process-grid-r grids l)))
                          {}
                          (range min-level (inc max-level)))]
    (as-> processed gs
      (if (= 0 (get gs min-level)) (dissoc gs min-level) gs)
      (if (= 0 (get gs max-level)) (dissoc gs max-level) gs))))

(defn count-bugs [grids]
  (apply +
         (map (fn [grid]
                (count (filter true?
                               (for [i (range 25)]
                                 (bit-test grid i)))))
              (vals grids))))

(defn draw-grids [grids]
  (let [min-level (apply min (keys grids))
        max-level (apply max (keys grids))]
    (doseq [l (range min-level (inc max-level))]
      (printf "Depth %d:\n" l (get grids l))
      (println (draw-grid (get grids l 0)))
      (newline))
    (println "Bug count:" (count-bugs grids))))

(defn process-r [grid n]
  (loop [grids {0 grid}
         step 0]
    (if (= step n)
      ;; (draw-grids grids)
      (count-bugs grids)
      (recur (process-grids grids) (inc step)))))

;; (time (process-r (parse-input small-input) 10))
;; "Elapsed time: 17.3706 msecs"
;; 99
;; (time (process-r (parse-input large-input) 200))
;; "Elapsed time: 3462.9504 msecs"
;; 2029
