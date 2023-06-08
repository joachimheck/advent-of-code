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
  (filter (fn [pos] (= \. (get (:grid maze) pos)))
          (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)])))

(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
          (pairs (rest coll)))))

(defn shortest-path [a b maze]
  (let [start-pos (get (:locations maze) a)
        goal #{(get (:locations maze) b)}]
    ;; (println "Finding path from" start-pos "to" goal)
    (loop [open-set #{start-pos}
           visited #{}
           distance 0]
      ;; (println "open-set size" (count open-set) "visited size" (count visited) "distance" distance)
      (if (empty? open-set)
        :no-path
        (if (some goal open-set)
          distance
          (recur (set (remove visited (mapcat #(neighbors % maze) open-set)))
                 (apply conj visited open-set)
                 (inc distance)))))))

(defn pair-paths [maze]
  (into {}
        (for [[a b] (pairs (keys (:locations maze)))]
          [#{a b} (shortest-path a b maze)])))

(defn permutations [coll]
  (if (= (count coll) 1)
    coll
    (mapcat (fn [f]
              (map #(flatten (list f %)) (permutations (remove #{f} coll))))
            coll)))

(defn route-distance [route paths]
  (apply +
         (for [[a b] (partition 2 1 route)]
           (get paths #{a b}))))

(defn shortest-route [maze]
  (let [paths (pair-paths maze)
        possible-routes (filter #(= 0 (first %)) (permutations (keys (:locations maze))))]
    (first
     (sort-by second
              (for [route possible-routes]
                (list route (route-distance route paths)))))))

;; (time (shortest-route (parse-input small-input)))
;; "Elapsed time: 3.1842 msecs"
;; ((0 4 1 2 3) 14)

;; (time (shortest-route (parse-input large-input)))
;; "Elapsed time: 914.0636 msecs"
;; ((0 4 5 6 2 1 3 7) 442)



;; Part 2
;; How many steps to also return to zero?

(defn shortest-return-route [maze]
  (let [paths (pair-paths maze)
        possible-routes (map #(conj (vec %) 0) (filter #(= 0 (first %)) (permutations (keys (:locations maze)))))]
    (first
     (sort-by second
              (for [route possible-routes]
                (list route (route-distance route paths)))))))

;; (time (shortest-return-route (parse-input small-input)))
;; "Elapsed time: 13.7404 msecs"
;; ([0 1 2 3 4 0] 20)

;; (time (shortest-return-route (parse-input large-input)))
;; "Elapsed time: 1021.532 msecs"
;; ([0 2 1 3 7 6 5 4 0] 660)
