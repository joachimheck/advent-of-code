(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 10: The Stars Align

;; Part 1
;; What message will the points of light in the sky eventually create?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"position=<([ \d-]+), ([ \d-]+)> velocity=<([ \d-]+), ([ \d-]+)>" %))
       (map rest)
       (map #(map str/trim %))
       (map #(map parse-long %))
       (map (fn [[a b c d]] {:p [a b] :v [c d]}))))

(defn bounds-base [points type]
  (reduce (fn [[minx miny maxx maxy] [x y]]
            [(min minx x)
             (min miny y)
             (max maxx x)
             (max maxy y)])
          [Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]
          (map type points)))

(defn bounds [points]
  (bounds-base points :p))

(defn v-bounds [points]
  (bounds-base points :v))

(defn draw-points [pvs]
  (let [points (map :p pvs)
        [minx miny maxx maxy :as the-bounds] (bounds pvs)]
    (str/join "\n"
              (for [j (range (dec miny) (+ 2 maxy))]
                (str/join
                 (for [i (range (dec minx) (+ 2 maxx))]
                   (if (some #(= [i j] %) points) "#" ".")))))))

(defn vec+ [v1 v2]
  (mapv #(apply + %) (map list v1 v2)))

(defn vec* [v x]
  (mapv #(* % x) v))

(defn move-points [pvs seconds]
  (map (fn [{p :p v :v}] {:p (vec+ p (vec* v seconds)) :v v}) pvs))

(defn outside [[minx1 miny1 maxx1 maxy1] [minx2 miny2 maxx2 maxy2]]
  (and (< minx1 minx2)
       (< miny1 miny2)
       (> maxx1 maxx2)
       (> maxy1 maxy2)))

(defn find-message [initial-pvs]
  (let [initial-bounds (bounds initial-pvs)]
    (loop [pvs initial-pvs time 0 prev-ydiff Integer/MAX_VALUE]
      (let [[_ miny _ maxy :as current-bounds] (bounds pvs)
            ydiff (- maxy miny)]
        (if (< ydiff prev-ydiff)
          (recur (move-points pvs 1) (inc time) ydiff)
          (let [end-points (move-points initial-pvs (dec time))]
            (println "After" (dec time) "seconds:")
            (println (draw-points end-points))))))))

;; (time (find-message (parse-input small-input)))
;; After 3 seconds:
;; ............
;; .#...#..###.
;; .#...#...#..
;; .#...#...#..
;; .#####...#..
;; .#...#...#..
;; .#...#...#..
;; .#...#...#..
;; .#...#..###.
;; ............
;; "Elapsed time: 3.902 msecs"
;; nil

;; (time (find-message (parse-input large-input)))
;; After 10521 seconds:
;; ................................................................
;; .#....#..#####...######..#....#..#....#..#....#..#....#..#......
;; .#....#..#....#..#.......#....#..#....#..#....#..#...#...#......
;; ..#..#...#....#..#........#..#....#..#....#..#...#..#....#......
;; ..#..#...#....#..#........#..#....#..#....#..#...#.#.....#......
;; ...##....#####...#####.....##......##......##....##......#......
;; ...##....#.......#.........##......##......##....##......#......
;; ..#..#...#.......#........#..#....#..#....#..#...#.#.....#......
;; ..#..#...#.......#........#..#....#..#....#..#...#..#....#......
;; .#....#..#.......#.......#....#..#....#..#....#..#...#...#......
;; .#....#..#.......#.......#....#..#....#..#....#..#....#..######.
;; ................................................................
;; "Elapsed time: 11877.3259 msecs"
;; nil



;; Part 2
;; How many seconds would it have taken for that message to appear?

;; 10521 ; from above.
