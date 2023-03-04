(ns advent-17.core)

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

;; Day 17: Reservoir Research

;; Part 1
;; How many tiles can the water reach?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/split % #", "))
       (map sort)
       (map #(map (fn [s] (rest (re-find #"(\d+).?.?(\d+)?" s))) %))
       (map #(map (fn [[a b]] (if (nil? b) [(parse-long a) (parse-long a)] [(parse-long a) (parse-long b)])) %))
       (map (fn [[[x1 x2] [y1 y2]]] (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [x y])))
       (apply concat)
       (distinct)
       (map (fn [x] [x \#]))
       (into {})))

(defn get-bounds [grid]
  (reduce (fn [[minx maxx miny maxy :as acc] [x y]]
            [(min minx x) (max maxx x) (min miny y) (max maxy y)])
          [Integer/MAX_VALUE Integer/MIN_VALUE Integer/MAX_VALUE Integer/MIN_VALUE]
          (keys grid)))

(def spring-pos [500 0])

(defn draw-grid [grid]
  (let [[minx maxx miny maxy :as bounds] (get-bounds grid)]
    (str/join "\n"
              (for [j (range miny (inc maxy))]
                (str/join (for [i (range minx (inc maxx))]
                            (let [v (get grid [i j])]
                              (if v v " "))))))))

(defn find-open-tiles [grid [minx maxx miny maxy :as bounds]]
  (let [falling-water (filter (fn [[[x y] c]]
                                (and (= \| c)
                                     (< y maxy)
                                     (let [char-below (get grid [x (inc y)])]
                                       (or (nil? char-below)
                                           (and (= \# char-below)
                                                (or (nil? (get grid [(dec x) y]))
                                                    (nil? (get grid [(inc x) y]))))
                                           (and (= \~ char-below)
                                                (or (nil? (get grid [(dec x) y]))
                                                    (nil? (get grid [(inc x) y]))))))))
                              grid)]
    falling-water))

(defn process-open-tile [[x y] grid]
  (let [below [x (inc y)]]
    (if (nil? (get grid below))
      (assoc grid below \|)
      (loop [i 1 left-val nil right-val nil]
        (if (and left-val right-val)
            (let [new-char (if (and (= (second left-val) :closed) (= (second right-val) :closed)) \~ \|)]
              (reduce (fn [grid [x y]] (assoc grid [x y] new-char))
                      grid
                      (for [i (range (first left-val) (inc (first right-val)))] [i y])))
            (let [left-top (get grid [(- x i) y])
                  left-bottom (get grid [(- x i) (inc y)])
                  new-left (if left-val
                             left-val
                             (cond (= \# left-top)
                                   (list (- x (dec i)) :closed)
                                   (nil? left-bottom)
                                   (list (- x i) :open)))
                  right-top (get grid [(+ x i) y])
                  right-bottom (get grid [(+ x i) (inc y)])
                  new-right (if right-val
                              right-val
                              (cond (= \# right-top)
                                    (list (+ x (dec i)) :closed)
                                    (nil? right-bottom)
                                    (list (+ x i) :open)))]
              (recur (inc i) new-left new-right)))))))

(defn place-water [initial-grid]
  (let [bounds (get-bounds initial-grid)]
    (loop [grid (assoc initial-grid spring-pos \|)
           open-tiles (find-open-tiles grid bounds)]
      (if (empty? open-tiles)
          grid
          (let [[[x y] c :as current] (first open-tiles)]
            (let [new-grid (process-open-tile [x y] grid)
                  new-open-tiles (find-open-tiles new-grid bounds)]
              (recur new-grid new-open-tiles)))))))

(defn count-reachable-tiles [initial-grid]
  (let [[_ _ miny maxy :as bounds] (get-bounds initial-grid)
        grid (place-water initial-grid)]
    (count (remove (fn [[[x y] c]]
                     (or (= \# c)
                         (< y miny)
                         (> y maxy)))
                   grid))))

;; (count-reachable-tiles (parse-input large-input))
;; 
;; 4640
;; ---> answer <---

;; The water should flow up and over the sides of this container:
;;
;;                 |                     |           
;;                 |                     |           
;;   #             |                     |         # 
;;   #             |                     |         # 
;;   #             |                     |         # 
;;   #|||||||||||||||||||||||||||||||||||||||      # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#|#     # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#|#     # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#|#     # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#|#     # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###     # 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
;;   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
;;   ############################################### 
;;
