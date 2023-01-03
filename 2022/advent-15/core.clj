(ns advent-15.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; How many positions on line 2000000 of the map must not contain a beacon?
(defn parse-input [f]
  (map (fn [line]
         (let [[x1 y1 x2 y2] (map #(Long/parseLong %) (re-seq #"-?\d+" line))]
           (list [x1 y1] [x2 y2])))
       (read-lines f)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- y2 y1))
     (abs (- x2 x1))))

(defn remove-point [[x-min x-max y-row :as row] [x y]]
  (if (not= y-row y)
    (list row)
    (cond (= x-min x-max x) '()
          (= x x-min) (list [(+ x-min 1) x-max y-row])
          (= x x-max) (list [x-min (- x-max 1) y-row])
          :else (list [x-min (- x 1) y-row] [(+ x 1) x-max y-row]))))

(defn row-intersections [[sensor-x sensor-y :as sensor-pos] [beacon-x beacon-y :as beacon-pos] row]
  (let [distance (manhattan-distance sensor-pos beacon-pos)
        min-y (- sensor-y distance)
        max-y (+ sensor-y distance)
        beacon-y (second beacon-pos)]
    (if (<= min-y row max-y)
      (let [half-width (- distance (abs (- sensor-y row)))]
        (remove-point [(- sensor-x half-width) (+ sensor-x half-width) row] beacon-pos))
      '())))

(defn get-row-intersections [input row]
  (sort #(- (first %1) (first %2))
        (reduce #(apply conj %1 %2)
                (filter #(not (empty? %)) (mapv (fn [[sensor-pos beacon-pos]] (row-intersections sensor-pos beacon-pos row)) input)))))

(defn get-non-beacon-ranges [input row]
  (if (= 0 (mod row 100000)) (println "row" row))
  (let [intersections (get-row-intersections input row)]
    (reduce (fn [result [new-x-min new-x-max y :as new]]
              (let [[x-min x-max _ :as the-last] (last result)]
                (if (<= x-min new-x-min (inc x-max))
                  (conj (vec (remove #(= the-last %) result)) [x-min (max x-max new-x-max) y])
                  (conj result new))))
            (vector (first intersections))
            (rest intersections))))

(defn range-length [[x1 x2 y]]
  (abs (- (inc x2) x1)))

(defn sum-range-lengths [coll]
  (apply + (map range-length coll)))

;; (sum-range-lengths (get-non-beacon-ranges (parse-input small-input) 10))
;; 26
;; (sum-range-lengths (get-non-beacon-ranges (parse-input large-input) 2000000))
;; 4737567


;; Part 2
;; What is the only point that could contain a beacon?
(defn get-possible-beacon-locations [input max-val]
  (set (map (fn [[i [[_ x1 _] [x2 _ _]]]] (vector (inc x1) i))
        (filter #(> (count (second %)) 1)
                (map-indexed #(list %1 (get-non-beacon-ranges input %2)) (range 0 (inc max-val)))))))

(defn find-hidden-beacon [f max-val]
  (let [input (parse-input f)
        beacon-locations (set (distinct (map second input)))
        possible-locations (get-possible-beacon-locations input max-val)]
    (set/difference possible-locations beacon-locations)))


;; (find-hidden-beacon small-input 20)
;; #{[14 11]}
;; (find-hidden-beacon large-input 4000000)
;; #{[3316868 2686239]}
;; (+ (* 3316868 4000000) 2686239)
;; 13267474686239
