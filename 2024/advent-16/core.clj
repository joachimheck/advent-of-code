(ns advent-16.core)

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
  (let [grid-vector (mapv vec (read-lines input))
        width (count (first grid-vector))
        height (count grid-vector)
        grid (-> (into {}
                       (for [i (range width)
                             j (range height)]
                         [[i j] (get-in grid-vector [j i])]))
                 (assoc :width width)
                 (assoc :height height))
        start-pos (first (first (filter #(= (second %) \S) grid)))
        end-pos (first (first (filter #(= (second %) \E) grid)))
        grid (-> grid
                 (assoc :start start-pos)
                 (assoc :end end-pos)
                 (assoc start-pos \.)
                 (assoc end-pos \.))]
    grid))

(defn print-grid [grid]
  (let [start (:start grid)
        end (:end grid)]
   (println
    (str/join "\n"
              (for [j (range (:height grid))]
                (str/join (for [i (range (:width grid))]
                            (cond 
                              (= [i j] start) \S
                              (= [i j] end) \E
                              :else (get grid [i j])))))))))

;; Part 1
;; The maze is scored at one point per move and 1000 points per
;; 90 degree turn. What is the lowest score a Reindeer can get?

