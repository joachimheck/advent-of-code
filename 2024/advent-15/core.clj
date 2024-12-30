(ns advent-15.core)

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
  (let [[grid-input _ moves-input] (->> input
                                        (read-lines)
                                        (partition-by #(zero? (count %))))
        width (count (first grid-input))
        height (count grid-input)
        grid (into {}
                   (let [grid-vector (mapv vec grid-input)]
                     (for [i (range width)
                           j (range height)]
                       [[i j] (get-in grid-vector [j i])])))
        robot-pos (first (first (filter #(= \@ (second %)) grid)))
        grid (assoc grid robot-pos \.)
        moves (vec (str/join moves-input))
        state {:grid grid
               :width width
               :height height
               :robot-pos robot-pos
               :moves moves}]
    state))

;; Part 1
(defn print-grid [{grid :grid width :width height :height robot-pos :robot-pos :as state}]
  (println
   (str/join "\n"
             (for [j (range height)]
               (str/join
                (for [i (range width)]
                  (if (= [i j] robot-pos)
                    \@
                    (get grid [i j]))))))))

(defn move-next [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state}]
  (let [move (first moves)
        move-fn (case move
                  \> #(vector (inc (first %)) (second %))
                  \v #(vector (first %) (inc (second %)))
                  \< #(vector (dec (first %)) (second %))
                  \^ #(vector (first %) (dec (second %))))
        stuff-ahead (take-while #(not= (second %) \#) (map #(vector % (get grid %)) (rest (iterate move-fn robot-pos))))
        first-space (first (filter #(= \. (second %)) stuff-ahead))
        [new-grid new-robot-pos] (cond
                                   ;; No space - robot can't move.
                                   (nil? first-space)
                                   [grid robot-pos]
                                   ;; Robot moves into a space - no boxes move.
                                   (= \. (second (first stuff-ahead)))
                                   [grid (move-fn robot-pos)]
                                   ;; Robot pushes boxes.
                                   :else
                                   [(-> grid
                                        (assoc (move-fn robot-pos) \.)
                                        (assoc (first first-space) \O))
                                    (move-fn robot-pos)])]
    (-> state
        (assoc :moves (rest moves))
        (assoc :grid new-grid)
        (assoc :robot-pos new-robot-pos))))

(defn predict-robot [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state}]
  (loop [state state]
    (if (empty? (:moves state))
      state
      (recur (move-next state)))))

(defn sum-gps-coordinates [input]
  (let [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as end-state} (predict-robot (parse-input input))]
    (print-grid end-state)
    (apply +
           (for [i (range width)
                 j (range height)
                 :when (= \O (get grid [i j]))]
             (+ (* 100 j) i)))))

;; (time (sum-gps-coordinates small-input))
;; ##########
;; #.O.O.OOO#
;; #........#
;; #OO......#
;; #OO@.....#
;; #O#.....O#
;; #O.....OO#
;; #O.....OO#
;; #OO....OO#
;; ##########
;; "Elapsed time: 5.8483 msecs"
;; 10092
;; 
;; (time (sum-gps-coordinates large-input))
;; ##################################################
;; #OOO..O.....OOO#..OO...O#.OOO..O#.....O..O#..O#..#
;; #.OO#..#......OO.#OO.O.O#O...O..O#......O..O.....#
;; #....O.O......OO#OO.O#.O..........O............O.#
;; ##....#O.O.O.OOO#O..#OOOOOO...#OO.OO.O..#O....#..#
;; #O...#O..O.###OOOO...OOOOO.#....OOOO#O.O.......O.#
;; #OOOO...#O.O...OO....O....O...OO.#OO..O.O..OOO...#
;; #.O..#..OO...........OO.....OO........O.O...OO#O##
;; #OO.........O..#....#OO.O............OO.........##
;; #.#.OO............O......OOO.......#..O#.........#
;; ##O..OOO........#OO.....##.O..O.....OOO...#.....O#
;; #O....##O........##O.......O#..OOOOOO............#
;; #O....OO#........O......O...O.O#OO...........OO..#
;; #OO....O.........O#...O...O..OO.#O...........##..#
;; #O#..#.......................OOOO..O#....#.......#
;; #................OOO.....#..##O#...O.O.......##..#
;; #OO....O......##....O.O..O.OO..OO................#
;; #..O##O.......OO...OOO.....O#O.OO........O.......#
;; ##.OOO#........O......##....##..O..OOO...OO#...OO#
;; #O.OOO#...O...#OOO.....#O.O.......O..OOOO...O#OOO#
;; #OO.OO....OOO...#..O..O.OO.OOOOOO.O.........OO..O#
;; #OO....O#......OO....OOO#O.......#O.#.....O.OO...#
;; #O......OOOO.......#OO...#.#......O....OOO#.O...O#
;; #O..........OO...O.....O...#......#O...#.OOO..O.O#
;; #O.........OO.OOO#O..............O.O#O..OO.O.#..O#
;; #O.....#...OO.OOOOO.............O..O#...OOOO.....#
;; #O...OO....O..OO.OO.OO#..#.......#.O#OO....OOO.#.#
;; #O..O#OO...#.#OO..O..OOOOO..O......OOOO..O.OO.O.O#
;; #OO#O......#..........OO....#.......OOOO..#O....##
;; #OOO......O#....O.....#OO.O....O..#..O......OOO#.#
;; #OOO...OOO.....O##..O...O#.#..O........O.OOO#.O#.#
;; #O#OO.#.#...#..OO.OOOO...#...O.......#....O##O...#
;; #.O#O....OO.......OO#.OOOOOOOO..........OOOO..O.O#
;; #..O..OO..O.......OO....#....O##.O......O#O..OO.O#
;; #.....O.....OO....O......O#.#..#......#O...O#....#
;; #....#O#OO..#.........#..#O.....OO..........O..O.#
;; #....OOO#O...O...........OO#..O...#O....#...O#O.##
;; ##...OOOO#O..O......OO..#OO....O...OO..#....O...O#
;; ##.......O...O.................OO#.O#.....O.O..OO#
;; #O.......#...O....OO...OOO........#...OO.O..O..O.#
;; #............O..O#OO..OO#.#O....OO.#...O...#...#.#
;; #....O....O....OOO#O..#OO.....OOO.O..OOO...O...O.#
;; #O#.O....#....OO#O....OO....O.O#...#......#..O.O.#
;; #OO..#.....#...O.O..O..#.O..O..O#....#O.O.....#..#
;; #O.......#.......#.......#OOO....#..#.O......O...#
;; #O........O.....@........OOO........#..OO.#.#....#
;; #OO..................O...O......O#..O#..OO..O...O#
;; #OOOOOOO.....O...O#.#O......#...O###...#O....O..O#
;; #O#OOOOO..OOOOOO#O.O.O.#O.....OOOO.OO....O.#....##
;; ##################################################
;; "Elapsed time: 235.099 msecs"
;; 1485257
