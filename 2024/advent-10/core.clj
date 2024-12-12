(ns advent-10.core)

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

(defn print-disk [disk]
  (str/join
   (map #(if (nil? %) "." (format "%d" %)) disk)))

(defn parse-input [input]
  (let [array (->> input
                   (read-lines)
                   (map vec)
                   (map #(map str %))
                   (mapv #(mapv parse-long %)))
        height (count array)
        width (count (first array))]
    (into {:width width :height height}
          (for [i (range width)
                j (range height)]
            [[i j] (get-in array [j i])]))))

;; Part 1
;; Sum the scores of the trailheads on the map.
(defn find-trailheads [grid]
  (map first (filter #(= 0 (second %)) (vec grid))))

(defn surrounding [grid [x y]]
  [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn available-tiles [grid pos]
  (let [altitude (get grid pos)]
    (filter #(= (inc altitude) (get grid %)) (surrounding grid pos))))

(defn find-paths [grid trailhead]
  (loop [paths [[trailhead]] 
         completed []]
    (let [new-paths (for [path paths
                          :let [end (last path)
                                {finished true extended false} (group-by #(= 9 (get grid (last %))) (map #(conj path %) (available-tiles grid end)))]]
                      {:completed finished :extended extended})
          merged (apply merge-with concat new-paths)]
      (if (empty? (:extended merged))
        (:completed merged)
        (recur (:extended merged) (:completed merged))))))

(defn score-trailheads [input]
  (let [grid (parse-input input)
        trailheads (find-trailheads grid)
        paths (map #(vector % (find-paths grid %)) trailheads)
        ends (map #(vector (first %) (distinct (map last (second %)))) paths )]
    (apply + (map #(count (second %)) ends))))


;; (time (score-trailheads small-input))
;; "Elapsed time: 5.4584 msecs"
;; 36
;; (time (score-trailheads large-input))
;; "Elapsed time: 45.1954 msecs"
;; 501


;; Part 2
;; Sum the trailhead ratings - the number of paths from each trailhead.
(defn rate-trailheads [input]
  (let [grid (parse-input input)
        trailheads (find-trailheads grid)
        paths (map #(vector % (find-paths grid %)) trailheads)]
    (count (apply concat (map second paths)))))

;; (time (rate-trailheads small-input))
;; "Elapsed time: 2.7238 msecs"
;; 81
;; (time (rate-trailheads large-input))
;; "Elapsed time: 37.0556 msecs"
;; 1017
