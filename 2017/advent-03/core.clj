(ns advent-03.core)

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

;; Day 3: Spiral Memory

;; Part 1
;; How many steps are required to carry data to the access port?
(defn square-size [radius]
  (* 4 (* 2 radius)))

(defn max-square-number [radius]
  (if (= radius 0)
    1
    (+ (square-size radius) (max-square-number (dec radius)))))

(defn get-square [n]
  (first (filter #(>= (:max-number %) n) (map (fn [n] {:radius n :max-number (max-square-number n) :side-size (* 2 n)}) (iterate inc 0)))))

(defn get-corners [max-number side-size]
  (for [side [0 1 2 3]]
    (- max-number (* side-size side))))

(defn get-centers [max-number side-size]
  (map #(- % (/ side-size 2)) (get-corners max-number side-size)))

(defn closest [n coll]
  (reduce (fn [acc x] (if (< (abs (- x n)) (abs (- x acc))) x acc))
          Integer/MAX_VALUE
          coll))

(defn steps-to-center [n]
  (let [{:keys [max-number radius side-size]} (get-square n)
        closest-center (closest n (get-centers max-number side-size))]
   (+ radius (abs (- n closest-center)))))

;; (time (steps-to-center 361527))
;; "Elapsed time: 11.352199 msecs"
;; 326



;; Part 2
;; Starting with 1 at the center, sum adjacent values in a spiral.
(defn *vec [v x]
  (mapv #(* x %) v))

(defn +vec [& vs]
  (vec (apply map + vs)))

(def deltas {0 [0 1] 1 [-1 0] 2 [0 -1] 3 [1 0]})

(defn side-start [square-start side-size target-side]
  (apply +vec (conj (for [side [0 1 2]
                          :when (< side target-side)]
                      (+vec (*vec (get deltas side) (dec side-size))
                            (get deltas (inc side))))
                    square-start)))

(defn square-positions [radius]
  (if (= 0 radius)
    (list [0 0])
    (let [side-size (* 2 radius)
          square-start [radius (- (dec radius))]]
      (for [side (range 4)
            :let [delta (get deltas side)
                  side-start (side-start square-start side-size side)]
            p (range side-size)
            :let [pos (+vec side-start (*vec delta p))]]
        pos))))

(defn spiral-position [n]
  (if (= n 1)
    [0 0]
    (let [{:keys [max-number radius side-size]} (get-square n)
          start-number (- max-number (* 4 side-size))]
      (nth (square-positions radius) (- n 1 start-number)))))

(defn neighbors [[x y]]
  (list [(dec x) (dec y)]
        [x (dec y)]
        [(inc x) (dec y)]
        [(dec x) y]
        [(inc x) y]
        [(dec x) (inc y)]
        [x (inc y)]
        [(inc x) (inc y)]))

(defn sum-neighbors [values pos]
  (let [neighbors (neighbors pos)]
    (apply + (map #(get values % 0) neighbors))))

(defn find-first-value-larger-than [n]
  (loop [values {[0 0] 1} spiral-index 2]
    (let [pos (spiral-position spiral-index)
          new-values (assoc values pos (sum-neighbors values pos))]
      (if (> (get new-values pos 0) n)
        (list (get new-values pos) :at pos)
        (recur new-values (inc spiral-index))))))

;; (time (find-first-value-larger-than 361527))
;; "Elapsed time: 3.398799 msecs"
;; (363010 :at [-3 4])
