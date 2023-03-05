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
(defn parse-lines [lines]
  (->> lines
       (map #(str/split % #", "))
       (map sort)
       (map #(map (fn [s] (rest (re-find #"(\d+).?.?(\d+)?" s))) %))
       (map #(map (fn [[a b]] (if (nil? b) [(parse-long a) (parse-long a)] [(parse-long a) (parse-long b)])) %))
       (map (fn [[[x1 x2] [y1 y2]]] (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [x y])))
       (apply concat)
       (distinct)
       (map (fn [x] [x \#]))
       (into {})))

(defn parse-input [f]
  (->> f
       (read-lines)
       (parse-lines)))

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
                                (and (< y maxy)
                                     (let [char-below (get grid [x (inc y)])
                                           char-left (get grid [(dec x) y])
                                           char-right (get grid [(inc x) y])]
                                       (or (and (= \- c)
                                                (nil? char-below))
                                           (and (= \| c)
                                                (or (nil? char-below)
                                                    (and (= \# char-below)
                                                         (or (or (nil? char-left) (nil? char-right))
                                                             (= \# char-left char-right))
                                                         ;; (not (and (or (= \| char-left) (= \# char-left))
                                                         ;;           (or (= \| char-right) (= \# char-right))))
                                                         ;; (or (nil? (get grid [(dec x) y]))
                                                         ;;     (nil? (get grid [(inc x) y])))
                                                         )
                                                    (and (= \~ char-below)
                                                         (or (or (nil? char-left) (nil? char-right))
                                                             (= \# char-left char-right))
                                                         ;; (not (and (= \| (get grid [(dec x) y]))
                                                         ;;           (= \| (get grid [(inc x) y]))))
                                                         ;; (or (nil? (get grid [(dec x) y]))
                                                         ;;     (nil? (get grid [(inc x) y])))
                                                         )))))))
                              grid)]
    falling-water))

(defn process-open-tile [[x y] grid]
  ;; (println "processing" [x y])
  (let [below [x (inc y)]]
    (if (nil? (get grid below))
      (assoc grid below \|)
      (loop [i 1 left-val nil right-val nil]
        (if (and left-val right-val)
            (let [new-char (if (and (= (second left-val) :closed) (= (second right-val) :closed)) \~ \|)]
              ;; (println "left-val" left-val "right-val" right-val)
              (reduce (fn [grid [x y]] (assoc grid [x y] new-char))
                      grid
                      (for [i (range (first left-val) (inc (first right-val)))] [i y])))
            (let [left-top (get grid [(- x i) y])
                  left-bottom (get grid [(- x i) (inc y)])
                  new-left (if left-val
                             left-val
                             (cond (= \# left-top)
                                   (list (- x (dec i)) :closed)
                                   (or (nil? left-bottom) (= \| left-bottom))
                                   (list (- x i) :open)))
                  right-top (get grid [(+ x i) y])
                  right-bottom (get grid [(+ x i) (inc y)])
                  new-right (if right-val
                              right-val
                              (cond (= \# right-top)
                                    (list (+ x (dec i)) :closed)
                                    (or (nil? right-bottom) (= \| right-bottom))
                                    (list (+ x i) :open)))]
              (recur (inc i) new-left new-right)))))))

(defn place-water [initial-grid]
  (let [bounds (get-bounds initial-grid)]
    (loop [i 0
           grid (assoc initial-grid spring-pos \|)
           open-tiles (find-open-tiles grid bounds)]
      (if (or ;; (= 10000 i)
              (empty? open-tiles))
          grid
          (let [[[x y] c :as current] (first open-tiles)]
            (let [new-grid (process-open-tile [x y] grid)
                  new-open-tiles (find-open-tiles new-grid bounds)]
              (recur (inc i) new-grid new-open-tiles)))))))

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
;; 31790

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
(def test-input '("x=499, y=2..4" "x=501, y=2..4" "x=500, y=4"))
(def test-input-2 '("x=496, y=2..6" "x=506, y=2..6" "x=496..506, y=7" "x=501..502, y=3" "x= 501..502, y=4"))
(def test-input-3 '("x=500, y=2..4" "x=503, y=2..4" "x=500..503, y=5"))

(def large-input-2 "large-input-2.txt") ;; Answer is apparently NOT 31677? I got 31689, which I think is also wrong.
;; (time (count-reachable-tiles (parse-input "large-input-2.txt")))
;; "Elapsed time: 221115.3491 msecs"
;; 31689

(def large-input-3 "large-input-3.txt") ;; Answer is 37649.
;; (time (count-reachable-tiles (parse-input large-input-3)))
;; "Elapsed time: 300069.3497 msecs"
;; 38404

;; I sometimes had double waterfalls coming off one edge.

;; (time (count-reachable-tiles (parse-input small-input)))
;; "Elapsed time: 2.8802 msecs"
;; 57

;; (time (count-reachable-tiles (parse-input large-input)))
;; "Elapsed time: 193705.3781 msecs"
;; 31788



;; Part 2
;; How much standing water is there?
(defn count-standing-water [initial-grid]
  (let [[_ _ miny maxy :as bounds] (get-bounds initial-grid)
        grid (place-water initial-grid)]
    (count (filter (fn [[[x y] c]] (= \~ c)) grid))))

;; (time (count-standing-water (parse-input small-input)))
;; "Elapsed time: 2.5513 msecs"
;; 29

;; (time (count-standing-water (parse-input large-input)))
;; "Elapsed time: 199072.146 msecs"
;; 25800
