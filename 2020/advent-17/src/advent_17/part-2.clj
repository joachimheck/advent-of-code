(ns advent-17.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

;; Part 2
;; 4D - redo storing just the active cells in a vector.
(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (for [[y line] (map-indexed list (doall (line-seq rdr)))
          [x c] (map-indexed list line)
          :when (= \# c)]
      (vector x y 0 0))))

(defn get-neighbors [[x y z w]]
  (remove #(= [x y z w] %)
          (for [i (range (- x 1) (+ x 2))
                j (range (- y 1) (+ y 2))
                k (range (- z 1) (+ z 2))
                l (range (- w 1) (+ w 2))]
            (vector i j k l))))

(defn array-contains? [arr p]
  (boolean (some #{p} arr)))

(defn count-active-neighbors [arr p]
  (let [neighbors (get-neighbors p)]
    (count (filter (set neighbors) arr))))
;    (reduce #(if (some #{%2} arr) (inc %1) %1) 0 neighbors)))
;    (count (keep #(some #{%} arr) neighbors))))

(defn evolve-point [arr p]
  (let [active (array-contains? arr p)
        active-neighbors (count-active-neighbors arr p)]
    (if active
      (<= 2 active-neighbors 3)
      (= 3 active-neighbors))))

(defn get-bounds [arr]
  (reduce
   (fn [bounds [x y z w]]
     (assoc bounds
            :x-min (min x (get bounds :x-min))
            :x-max (max x (get bounds :x-max))
            :y-min (min y (get bounds :y-min))
            :y-max (max y (get bounds :y-max))
            :z-min (min z (get bounds :z-min))
            :z-max (max z (get bounds :z-max))
            :w-min (min w (get bounds :w-min))
            :w-max (max w (get bounds :w-max))))
   {:x-min 0 :x-max 0 :y-min 0 :y-max 0 :z-min 0 :z-max 0 :w-min 0 :w-max 0}
   arr))

(defn evolve-array [arr]
  (let [bounds (get-bounds arr)]
    (for [i (range (- (get bounds :x-min) 1) (+ 2 (get bounds :x-max)))
          j (range (- (get bounds :y-min) 1) (+ 2 (get bounds :y-max)))
          k (range (- (get bounds :z-min) 1) (+ 2 (get bounds :z-max)))
          l (range (- (get bounds :w-min) 1) (+ 2 (get bounds :w-max)))
          :when (evolve-point arr [i j k l])]
      [i j k l])))

;; (time (count (nth (iterate evolve-array (read-input large-input)) 6)))
;; "Elapsed time: 14626.4075 msecs"
;; 1180
