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
                 (assoc :start (conj start-pos :east))
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

(defn reconstruct-path [came-from current]
  (reverse
   (loop [current current
          total-path [current]]
     (let [[next-pos weight :as next] (get came-from current)]
       (if next
         (recur next-pos (conj total-path next))
         total-path)))))

(defn edge-weight [[x y d :as current] [n-x n-y n-d :as next]]
  (let [d-set #{d n-d}]
    ;; (println "edge-weight d-set" d-set "superset" #{#{:north :east} {:east :south} {:south :west} {:west :north}})
    (cond (= (count d-set) 1) 1
          (#{#{:north :east} {:east :south} {:south :west} {:west :north}} [d-set]) 1000
          :else 2000)))

(def directions [:east :south :west :north])

(defn move [[x y d]]
  (case d
    :east [(inc x) y d]
    :south [x (inc y) d]
    :west [(dec x) y d]
    :north [x (dec y) d]))

(defn neighbors [grid [x y d :as current]]
  (let [raw-neighbors [[(move current) 1]
                       [[x y (get directions (mod (inc (count (take-while #(not= % d) directions))) 4))] 1000]
                       [[x y (get directions (mod (dec (count (take-while #(not= % d) directions))) 4))] 1000]]]
    (remove (fn [[pos edge-weight]]
              (let [at-grid (get grid (take 2 pos))]
                (or (nil? at-grid) (= \# at-grid))))
            raw-neighbors)))

(defn manhattan-distance [[x y _ :as start] [x' y' _ :as end]]
  (+ (Math/abs (- x' x)) (Math/abs (- y' y))))

(defn cheapest-path-a* [{start :start end :end width :width height :height :as grid} h-fn limit]
  (loop [open-set #{start}
         came-from {}
         g-score {start 0}
         f-score {start (h-fn start end)}
         n 0]
    ;; (println "n" n "open-set" open-set)
    (if (>= n limit)
      :limit-reached
      (if (not (empty? open-set))
        (let [current (first (sort-by #(get f-score %) open-set))]
          (if (= (take 2 current) end)
            (let [path (reconstruct-path came-from current)
                  cost (reduce (fn [acc [pos weight]]
                                 (+ acc weight))
                               0
                               path)]
              {:path path :cost cost})
            (let [open-set (disj open-set current)
                  {open-set :open-set came-from :came-from g-score :g-score f-score :f-score}
                  (reduce (fn [{open-set :open-set came-from :came-from g-score :g-score f-score :f-score :as acc}
                               [neighbor edge-weight]]
                            (let [tentative-g-score (+ (get g-score current Integer/MAX_VALUE) edge-weight)]
                              (if (< tentative-g-score (get g-score neighbor Integer/MAX_VALUE))
                                {:open-set (conj open-set neighbor)
                                 :came-from (assoc came-from neighbor [current edge-weight])
                                 :g-score (assoc g-score neighbor tentative-g-score)
                                 :f-score (assoc f-score neighbor (+ tentative-g-score (h-fn neighbor end)))}
                                acc)))
                          {:open-set open-set :came-from came-from :g-score g-score :f-score f-score}
                          (neighbors grid current))]
              (recur open-set came-from g-score f-score (inc n)))))
        :empty-open-set
        ))))
