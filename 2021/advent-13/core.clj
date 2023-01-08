(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 13: Transparent Origami

;; Part 1
;; Count the points after folding the paper once.
(defn parse-input [f]
  (let [partitioned-lines (partition-by #{""} (read-lines f))
        [point-lines fold-lines] [(first partitioned-lines) (last partitioned-lines)]
        points (->> point-lines
                    (map #(str/join (list "[" % "]")))
                    (map read-string))
        folds (->> fold-lines
                   (map #(re-find #"([x|y])=(\d+)" %))
                   (map rest)
                   (map #(list (first %) (parse-long (second %)))))]
    {:points points :folds folds}))

(defn draw-grid [grid]
  (let [{minx :minx miny :miny maxx :maxx maxy :maxy}
        (reduce (fn [result [x y]]
                  {:minx (min (result :minx) x) :miny (min (result :miny) y) :maxx (max (result :maxx) x) :maxy (max (result :maxy) y)})
                {:minx Long/MAX_VALUE :miny Long/MAX_VALUE :maxx Long/MIN_VALUE :maxy Long/MIN_VALUE}
                grid)]
    (str/join
     "\n"
     (for [j (range miny (inc maxy))]
       (str/join
        (for [i (range minx (inc maxx))]
          (if (some #{[i j]} grid) "#" ".")))))))

(def X 0)

(def Y 1)

(defn fold-at [[axis v] grid]
  (if (= axis "y")
    (let [{lower-points true upper-points false} (group-by #(> (% Y) v) grid)
          folded-lowers (map (fn [[x y]] [x (- v (- y v))]) lower-points)]
      (distinct (concat upper-points folded-lowers)))
    (let [{right-points true left-points false} (group-by #(> (% X) v) grid)
          folded-rights (map (fn [[x y]] [(- v (- x v)) y]) right-points)]
      (distinct (concat left-points folded-rights)))))


;; (let [{points :points folds :folds} (parse-input small-input)]
;;                   (time (count (fold-at (first folds) points))))
;; "Elapsed time: 0.7658 msecs"
;; 17

;; (let [{points :points folds :folds} (parse-input large-input)]
;;                   (time (count (fold-at (first folds) points))))
;; "Elapsed time: 23.2441 msecs"
;; 729



;; Part 2
;; What letters are shown on the paper after folding?
(defn fold [f]
  (let [{points :points folds :folds} (parse-input f)]
    (reduce (fn [result [axis v]]
              (fold-at [axis v] result))
            points
            folds)))

;; (time (println (draw-grid (fold small-input))))
;; #####
;; #...#
;; #...#
;; #...#
;; #####
;; "Elapsed time: 1.6118 msecs"
;; nil

;; (time (println (draw-grid (fold large-input))))
;; ###...##..####.#....###..#..#.####.###.
;; #..#.#..#....#.#....#..#.#..#.#....#..#
;; #..#.#......#..#....###..####.###..#..#
;; ###..#.##..#...#....#..#.#..#.#....###.
;; #.#..#..#.#....#....#..#.#..#.#....#...
;; #..#..###.####.####.###..#..#.#....#...
;; "Elapsed time: 24.2453 msecs"
;; nil
