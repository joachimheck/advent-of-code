(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn parse-input [input]
  (let [grid (mapv vec (read-lines input))
        width (count (first grid))
        height (count grid)]
    (into {:width width :height height}
          (for [i (range width)
                j (range height)
                :when (not= \. (get-in grid [j i]))]
            [[i j] (get-in grid [j i])]))))

;; Part 1
;; How many unique locations within the map bounds contain an antinode?
(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
          (pairs (rest coll)))))

(defn in-bounds [[x y] width height]
  (and (< -1 x width)
       (< -1 y height)))

(defn count-antinode-locations [input]
  (let [the-map (parse-input input)
        by-id (reduce (fn [acc [k v]]
                        (assoc acc v (conj (get acc v '()) k)))
                      {}
                      (dissoc the-map :width :height))
        antenna-pairs (apply concat
                             (for [id (keys by-id)
                                   ]
                               (pairs (get by-id id))
                               ))
        all-antinodes (apply concat
                             (for [[[x1 y1] [x2 y2]] antenna-pairs
                                   :let [xdiff (- x2 x1)
                                         ydiff (- y2 y1)]]
                               [[(+ x2 xdiff) (+ y2 ydiff)] [(- x1 xdiff) (- y1 ydiff)]]))]
    (count (distinct (filter #(in-bounds % (:width the-map) (:height the-map)) all-antinodes)))))


;; (time (count-antinode-locations small-input))
;; "Elapsed time: 0.8648 msecs"
;; 14
;; (time (count-antinode-locations large-input))
;; "Elapsed time: 7.7905 msecs"
;; 359
