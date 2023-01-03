(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; How much sand comes to rest?
(defn parse-input [f]
  (let [input (read-lines f)]
   (mapcat
    (fn [positions] (partition 2 1 positions))
    (map
     (fn [s] (map
              (fn [s2] (read-string (str/join ["[" s2 "]"])))
              (str/split s #" -> ")))
     input))))

(defn +vec [v1 v2]
  (mapv #(apply + %) (map vector v1 v2)))

(defn min-vec [v1 v2]
  (let [diffs (drop-while #(= 0 %) (map #(apply - %) (map vector v1 v2)))]
    (if (empty? diffs) v1
        (if (> (first diffs) 0) v2 v1))))

(defn max-vec [v1 v2]
  (let [diffs (drop-while #(= 0 %) (map #(apply - %) (map vector v1 v2)))]
    (if (empty? diffs) v1
        (if (< (first diffs) 0) v2 v1))))

(defn build-line [p1 p2]
  (let [start (min-vec p1 p2)
        end (max-vec p1 p2)
        step-vector (if (= (start 0) (end 0)) [0 1] [1 0])]
    (loop [points [start]]
      (if (= (last points) end)
        points
        (recur (conj points (+vec (last points) step-vector)))))))

(defn build-diagram [endpoints]
  (distinct (mapcat #(apply build-line %) endpoints)))

(defn process-sand-unit [rocks sand]
  (let [obstructions (concat rocks sand)
        bottom-y (apply max (map second rocks))]
   (loop [pos [500 0]]
     (let [below (+vec pos [0 1])
           below-left (+vec pos [-1 1])
           below-right (+vec pos [1 1])]
       (cond (= (second pos) bottom-y) sand
             (not (some #{below} obstructions)) (recur below)
             (not (some #{below-left} obstructions)) (recur below-left)
             (not (some #{below-right} obstructions)) (recur below-right)
             :else (conj sand pos))))))

(defn process-sand [f]
  (let [rocks (build-diagram (parse-input f))]
    (loop [sand []
           i 0]
      (let [new-sand (process-sand-unit rocks sand)]
        (if (= sand new-sand)
          i
          (recur new-sand (inc i)))))))

;; (process-sand small-input)
;; 24
;; (process-sand large-input)
;; 1016


;; Part 2
;; Consider the floor and compute until the entry space is blocked.
(defn obstructed? [pos obstructions floor-y]
  (or (some #{pos} obstructions)
      (= (second pos) floor-y)))

(defn process-sand-unit-with-floor [rocks sand]
  (let [obstructions (concat rocks sand)
        floor-y (+ 2 (apply max (map second rocks)))]
    (loop [pos [500 0]]
      (let [below (+vec pos [0 1])
            below-left (+vec pos [-1 1])
            below-right (+vec pos [1 1])]
        (cond (not (obstructed? below obstructions floor-y)) (recur below)
              (not (obstructed? below-left obstructions floor-y)) (recur below-left)
              (not (obstructed? below-right obstructions floor-y)) (recur below-right)
              (= pos [500 0]) sand
              :else (conj sand pos))))))

(defn process-sand-with-floor [f]
  (let [rocks (build-diagram (parse-input f))]
    (loop [sand []
           i 1]
      (let [new-sand (process-sand-unit-with-floor rocks sand)]
        (if (= (count sand) (count new-sand))
          i
          (recur new-sand (inc i)))))))

;; (process-sand-with-floor small-input)
;; 93
;; (process-sand-with-floor large-input)
;; 25402
