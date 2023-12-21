(ns advent-21.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many garden plots can be reached in the given number of steps?
(defn parse-pattern [input]
  (let [vectorized (mapv vec input)
        height (count vectorized)
        width (count (first vectorized))
        grid (into {}
                   (for [j (range height)
                         i (range width)
                         :let [c (get-in vectorized [j i])]]
                     [[i j] c]))
        start (get (set/map-invert grid) \S)]
    {:width width
     :height height
     :grid (assoc grid start \.)
     :start start}))

(defn parse-input [input]
  (->> (read-lines input)
       (parse-pattern)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (get grid [i j]))))))

(defn move [x y dir amount]
  (case dir
    :right [(+ x amount) y]
    :down [x (+ y amount)]
    :left [(- x amount) y]
    :up [x (- y amount)]))

(defn adjacent [[x y :as p]]
  #{[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]})

(defn in-bounds [width height points]
  (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))) points))

(defn adjacent-open [{:keys [width height grid]} p]
  (->> p
       (adjacent)
       (map #(list % (get grid %)))
       (filter #(= \. (second %)))
       (map first)
       (in-bounds width height)))

(defn find-tiles-by-steps [{:keys [width height grid start] :as pattern} steps]
  (loop [open-set #{start}
         i steps]
    (if (<= i 0)
      open-set
      (recur (set (mapcat #(adjacent-open pattern %) open-set)) (dec i)))))

(defn how-many-plots [input steps]
  (count (find-tiles-by-steps (parse-input input) steps)))

;; (time (how-many-plots small-input 6))
;; "Elapsed time: 1.0878 msecs"
;; 16

;; (time (how-many-plots large-input 64))
;; "Elapsed time: 607.8964 msecs"
;; 3751



;; Part 2
;; With infinitely repeating gardens, how many plots can be reached in a large number of steps?
(defn mark-tiles [{:keys [width height grid start] :as pattern} points]
  (assoc pattern :grid
         (reduce (fn [acc p] (assoc acc p \O))
                 grid
                 points)))

;; TODO: maybe look for repetition in the increase from one step count to the next?

