(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of the distances between the galaxies considering universal expansion?
(defn parse-input [input]
  (let [vectorized (mapv vec (read-lines input))
        height (count vectorized)
        width (count (first vectorized))]
    {:width width
     :height height
     :grid (into {}
                 (for [j (range height)
                       i (range width)
                       :let [c (get-in vectorized [j i])]
                       :when (= c \#)]
                   [[i j] c]))}))

(defn find-empty [{:keys [width height grid]}]
  (let [galaxy-positions (map first grid)
        empty-rows (remove (set (map second galaxy-positions)) (range height))
        empty-columns (remove (set (map first galaxy-positions)) (range width))]
    {:empty-rows empty-rows :empty-columns empty-columns}))

(defn expand-grid [{:keys [width height grid] :as state} empty-rows empty-columns expansion]
  {:width (+ width (* (dec expansion) (count empty-columns)))
   :height (+ height (* (dec expansion) (count empty-rows)))
   :grid (into {} (for [[[x y] c] grid]
                    [[(+ x (* (dec expansion) (count (filter #(> x %) empty-columns))))
                      (+ y (* (dec expansion) (count (filter #(> y %) empty-rows))))] c]))})

(defn print-grid [{:keys [width height grid]}]
  (println "width" width "height" height)
  (let [output (str/join "\n"
                         (for [j (range height)]
                           (str/join
                            (for [i (range width)]
                              (or (get grid [i j]) \.)))))]
    (println output)))

(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
          (pairs (rest coll)))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn sum-shortest-path-lengths [input expansion]
  (let [{:keys [width height grid] :as state} (parse-input input)
        {:keys [empty-rows empty-columns]} (find-empty state)
        expanded (expand-grid state empty-rows empty-columns expansion)
        galaxy-pairs (pairs (map first (:grid expanded)))
        distances (map #(apply manhattan-distance %) galaxy-pairs)]
    (apply + distances)))

;; (sum-shortest-path-lengths small-input)
;; 374

;; (sum-shortest-path-lengths large-input)
;; 9545480



;; Part 2
;; Expansion is actually 1,000,000 times greater than previously supposed.

;; (sum-shortest-path-lengths large-input 1000000)
;; 406725732046
