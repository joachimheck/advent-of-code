(ns advent-06.core)

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

;; Day 6: Chronal Coordinates

;; Part 1
;; What is the size of the largest finite area around one of the points?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/join (concat "[" % "]")))
       (map load-string)))

(defn get-bounds [coordinates]
  (let [[minx miny maxx maxy] (reduce (fn [[minx miny maxx maxy] [x y]]
                                        [(min minx x) (min miny y) (max maxx x) (max maxy y)])
                                      [Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]
                                      coordinates)]
    (list [(dec minx) (dec miny)] [(inc maxx) (inc maxy)])))

(defn manhattan-distance [[x y] [a b]]
  (+ (abs (- x a)) (abs (- y b))))

(defn closest-point [p coordinates]
  (let [distances (map #(list % (manhattan-distance % p)) coordinates)
        min (apply min (map second distances))
        closest (filter #(= min (second %)) distances)]
    (if (= 1 (count closest))
      (first (first closest)))))

(defn map-spaces [coordinates]
  (let [[[minx miny] [maxx maxy]] (get-bounds coordinates)
        self-map (reduce (fn [acc p] (assoc acc p '())) {} coordinates)]
    (apply merge-with conj
           (concat (list self-map)
                   (for [x (range minx (inc maxx))
                         y (range miny (inc maxy))
                         :let [closest (closest-point [x y] coordinates)]
                         :when closest]
                     {closest [x y]})))))

(defn infinite? [area coordinates]
  (let [[[minx miny] [maxx maxy]] (get-bounds coordinates)]
    (first (filter (fn [[x y]] (or (= x minx) (= x maxx) (= y miny) (= y maxy))) area))))

(defn find-largest-finite-area [coordinates]
  (let [finite (remove (fn [[k v]] (infinite? v coordinates)) (map-spaces coordinates))
        counts (map (fn [[k v]] (list k (count v))) finite)]
    (apply max (map second counts))))


;; (time (find-largest-finite-area (parse-input small-input)))
;; "Elapsed time: 5.2907 msecs"
;; 17

;; (time (find-largest-finite-area (parse-input large-input)))
;; "Elapsed time: 5251.4416 msecs"
;; 3251



;; Part 2
;; What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
(defn distance-to-coordinates [p coordinates]
  (apply + (map #(manhattan-distance p %) coordinates)))

(defn map-spaces-2 [coordinates max-distance]
  (let [[[minx miny] [maxx maxy]] (get-bounds coordinates)]
    (for [x (range minx (inc maxx))
          y (range miny (inc maxy))
          :when (< (distance-to-coordinates [x y] coordinates) max-distance)]
      [x y])))

(defn find-area-within-distance [coordinates max-distance]
  (count (map-spaces-2 coordinates max-distance)))

;; (time (find-area-within-distance (parse-input small-input) 32))
;; "Elapsed time: 3.3836 msecs"
;; 16

;; (time (find-area-within-distance (parse-input large-input) 10000))
;; "Elapsed time: 2094.1143 msecs"
;; 47841
