(ns advent-06.core)

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
  (let [grid (->> input
                  (read-lines)
                  (map vec)
                  vec)
        height (count grid)
        width (count (first grid))
        start (first (for [j (range height) i (range width)
                           :when (= \^ (get-in grid [j i]))]
                       [i j :north]
                       ))]
    {:grid (assoc-in grid [(second start) (first start)] \.)
     :height height
     :width width
     :start start}
    ))


;; Part 1
;; Predict the path of the guard and count the positions she reaches.
(def dirs [:east :south :west :north])

(defn move-dir [grid [i j dir]]
  (case dir
    :east [(inc i) j dir]
    :south [i (inc j) dir]
    :west [(dec i) j dir]
    :north [i (dec j) dir]))

(defn move [grid [i j dir]]
  (let [dirs (drop-while #(not= % dir) (take 7 (cycle dirs)))
        nexts (->> dirs
                   (map #(move-dir grid [i j %]))
                   (map #(get-in grid (reverse (take 2 %)))))
        ats (map list dirs nexts)
        next-move (first (filter #(not= \# (second %)) ats))]
    ;; (println "move" [i j dir] "->" (move-dir grid [i j (first next-move)]) "ats" ats)
    (move-dir grid [i j (first next-move)])))

(defn count-positions-reached [input]
  (let [{:keys [grid height width start]} (parse-input input)]
    (count
     (distinct
      (loop [pos start
             positions [(take 2 start)]]
        (let [[ni nj nd :as next] (move grid pos)]
          (if (and (< -1 ni width) (< -1 nj height))
            (recur next (conj positions (take 2 next)))
            positions)))))))

;; (time (count-positions-reached small-input))
;; "Elapsed time: 1.843201 msecs"
;; 41
;; (time (count-positions-reached large-input))
;; "Elapsed time: 54.613499 msecs"
;; 4374


;; Part 2
;; Where can an object be placed to make the guard's path a loop?
(def move-fns {:east (fn [[i j d]] [(inc i) j d])
               :south (fn [[i j d]] [i (inc j) d])
               :west (fn [[i j d]] [(dec i) j d])
               :north (fn [[i j d]] [i (dec j) d])
               })

(defn move-forward
  "Returns the positions the guard will move through before encountering an obstacle or leaving the room."
  [{:keys [grid width height]} start]
  (let [[x y dir] start
        move-fn (dir move-fns)]
    (loop [pos [x y dir] posns []]
      (let [[i j d :as newpos] (move-fn pos)]
        (cond (= \# (get-in grid [j i]))
              posns
              (or (not (< -1 i width))
                  (not (< -1 j height)))
              (conj posns newpos)
              :else
              (recur newpos (conj posns newpos)))))))

(defn turn-right [dir]
  ;; (first (drop 1 (drop-while #(not= % dir) (take 5 (cycle dirs)))))
  (get dirs (mod (inc (.indexOf dirs dir)) 4)))

(defn loop? [{:keys [grid width height] :as state} start]
  (loop [[i j d :as pos] start
         positions [start]]
    (let [newposns (move-forward state pos)
          [ni nj nd :as next] (last newposns)]
      (cond
        (empty? newposns)
        (recur [i j (turn-right d)] positions)
        (some (set newposns) positions)
        [true (concat positions newposns)]
        (and (< -1 ni width) (< -1 nj height))
        (recur next (concat positions newposns))
        :else
        [false (concat positions newposns)]))))

(defn count-loop-positions [input]
  (let [{:keys [grid height width start] :as state} (parse-input input)]
    (count
     (filter #(true? (first %))
             (for [j (range height)
                   ;; :let [_ (println "Processing row" j)]
                   i (range width)
                   :when (not= [i j] (take 2 start))
                   :let [new-state (assoc state :grid (assoc-in grid (reverse [i j]) \#))
                         ;; _ (Thread/sleep 10)
                         ]]
               (loop? new-state start))))))

;; (time (count-loop-positions small-input))
;; "Elapsed time: 18.9686 msecs"
;; 6
;; (time (count-loop-positions large-input))
;; "Elapsed time: 635175.0989 msecs"
;; 1705
