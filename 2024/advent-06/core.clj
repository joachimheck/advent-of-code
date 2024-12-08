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

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

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

(defn turn-right [dir]
  (first (drop 1 (drop-while #(not= % dir) (take 5 (cycle dirs))))))

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


