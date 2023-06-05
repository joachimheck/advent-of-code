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
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 24: Air Duct Spelunking

;; Part 1
;; Visit the numbered points in the fewest steps.

(defn parse-input [f]
  (let [lines (read-lines f)
        width (count (first lines))
        height (count lines)]
    (reduce (fn [{:keys [grid locations]} [pos c]]
              (case c
                (\# \.) {:grid (assoc grid pos c) :locations locations}
                {:grid (assoc grid pos \.) :locations (assoc locations (parse-long (str c)) pos)}))
            {:grid {} :locations {}}
            (for [j (range height)
                  i (range width)]
              [[i j] (get (nth lines j) i)]))))

(defn neighbors [[x y] maze]
  (remove (fn [pos] (= \# (get (:grid maze) pos)))
          (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)])))

(defn shortest-path [maze]
  (let [start-pos (get (:locations maze) 0)
        target-points (set (vals (dissoc (:locations maze) 0)))]
    (loop [paths (list [start-pos])
           shortest nil]
      (if (empty? paths)
        {:shortest shortest
         :length (count shortest)}
        (let [current (first paths)
              next-points (remove (set current) (neighbors (last current) maze))]
          (if (set/subset? target-points (set current))
            (recur (rest paths) (if (or (nil? shortest) (< (count current) (count shortest))) current shortest))
            (let [new-paths (doall (filter #(or (nil? shortest) (< (count %) (count shortest))) (doall (map #(conj current %) next-points))))]
              (recur (doall (concat (rest paths) new-paths)) shortest))))))))
