(ns advent-17.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Trick Shot

;; Part 1
;; What's the maximum y position reachable by a probe that enters the target area?
(defn parse-input [f]
  (let [[x1 x2 y1 y2] (->> f
                           read-lines
                           first
                           (re-find #"x=([-\d]+)\.\.([-\d]+), y=([-\d]+)\.\.([-\d]+)")
                           rest
                           (map parse-long))]
    (list [(min x1 x2) (max y1 y2)] [(max x1 x2) (min y1 y2)])))

(defn within? [[px py] [[x1 y1] [x2 y2]]]
  (and (<= x1 px x2) (>= y1 py y2)))

(defn missed? [[px py] [[x1 y1] [x2 y2]]]
  (< py y2))

(defn next-pv [[x y] [dx dy]]
  (let [new-p [(+ x dx) (+ y dy)]
        ddx (cond (> dx 0) -1 (< dx 0) 1 :else 0)
        new-v [(+ dx ddx) (- dy 1)]]
    (list new-p new-v)))

(defn compute-trajectory [target dx dy]
  (loop [x 0 y 0 dx dx dy dy ps []]
    (let [[[new-x new-y] [new-dx new-dy]] (next-pv [x y] [dx dy])
            new-ps (conj ps [new-x new-y])]
        (cond (within? [new-x new-y] target)
              {:hit true :trajectory new-ps}
              (missed? [new-x new-y] target)
              {:hit false :trajectory new-ps}
              :else
              (recur new-x new-y new-dx new-dy new-ps)))))

(defn draw-trajectory [[[t-min-x t-max-y] [t-max-x t-min-y] :as target] ps]
  (let [min-x 0
        min-y (apply min (conj (map second ps) t-min-y))
        max-x (apply max (conj (map first ps) t-max-x))
        max-y (apply max (conj (map second ps) t-max-y 0))]
    (str/join
     "\n"
     (for [j (range max-y (dec min-y) -1)]
       (str/join
        (for [i (range min-x (inc max-x))]
          (let [pos [i j]]
            (cond (= [0 0] pos)
                  "S"
                  (some #{pos} ps)
                  "#"
                  (within? pos target)
                  "T"
                  :else
                  "."))))))))

(defn find-good-trajectories [target maxdx maxdy]
  (->> (for [di (range (inc maxdx))
             dj (range (inc maxdy))
             :let [{hit :hit trajectory :trajectory} (compute-trajectory target di dj)]
             :when hit
             :let [maxy (apply max (map second trajectory))]]
         [di dj maxy])
       (sort-by last)
       last))

;; (let [target (parse-input small-input)]
;;                   (time (find-good-trajectories target 100 100)))
;; "Elapsed time: 2408.8886 msecs"
;; [7 9 45]

;; (let [target (parse-input large-input)]
;;                   (time (find-good-trajectories target 200 200)))
;; "Elapsed time: 18971.3222 msecs"
;; [17 145 10585]



;; Part 2
;; Find all successful trajectories.
(defn find-good-trajectories [target maxdx mindy maxdy]
  (->> (for [di (range (inc maxdx))
             dj (range mindy (inc maxdy))
             :let [{hit :hit trajectory :trajectory} (compute-trajectory target di dj)]
             :when hit
             :let [maxy (apply max (map second trajectory))]]
         [di dj maxy])
       count))

;; (let [target (parse-input small-input)]
;;                   (time (find-good-trajectories target 100 -50 50)))
;; "Elapsed time: 637.8575 msecs"
;; 112

;; (let [target (parse-input large-input)]
;;                   (time (find-good-trajectories target 157 -146 250)))
;; "Elapsed time: 23225.4523 msecs"
;; 5247
