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
;; Sum the GPS coordinates of the boxes in the warehouse after the robot has moved them.
(defn print-grid [{grid :grid width :width height :height robot-pos :robot-pos :as state}]
  (println
   (str/join "\n"
             (for [j (range height)]
               (str/join
                (for [i (range width)]
                  (if (= [i j] robot-pos)
                    \@
                    (get grid [i j]))))))))

(defn make-move-fn [move]
  (case move
    \> #(vector (inc (first %)) (second %))
    \v #(vector (first %) (inc (second %)))
    \< #(vector (dec (first %)) (second %))
    \^ #(vector (first %) (dec (second %)))))

(defn move-next [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state}]
  (let [move (first moves)
        move-fn (make-move-fn move)
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


;; Part 1
;; Everything in the second warehouse is twice as wide.
(defn parse-input-wide [input]
  (let [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state} (parse-input input)]
    {:grid (into {}
                 (apply concat
                        (for [i (range width)
                              j (range height)]
                          (case (get grid [i j])
                            \# [[[(* 2 i) j] \#] [[(inc (* 2 i)) j] \#]]
                            \O [[[(* 2 i) j] \[] [[(inc (* 2 i)) j] \]]]
                            \. [[[(* 2 i) j] \.] [[(inc (* 2 i)) j] \.]]
                            \@ [[[(* 2 i) j] \@] [[(inc (* 2 i)) j] \.]])
                          )))
     :width (* 2 width)
     :height height
     :robot-pos [(* 2 (first robot-pos)) (second robot-pos)]
     :moves moves}))

(defn moveable-boxes-ahead-wide [grid robot-pos move]
  (let [move-fn (make-move-fn move)]
    (if (some #{\<\>} [move])
      (let [stuff-ahead (take-while #(not (some #{(second %)} [\#\.])) (map #(vector % (get grid %)) (rest (iterate move-fn robot-pos))))
            next (get grid (move-fn (first (last stuff-ahead))))]
        (if (= \. next)
          (map first (filter #(= \[ (second %)) stuff-ahead))))
      (loop [positions [robot-pos]
             moving-boxes []]
        ;; (println "loop" "positions" positions "moving-boxes" moving-boxes)
        (let [next-positions (map move-fn positions)
              vals-at-next (map #(get grid %) next-positions)]
          (cond (every? #(= % \.) vals-at-next)
                moving-boxes
                (some #(= % \#) vals-at-next)
                nil
                :else
                ;; add boxes to moving-boxes, spaces above boxes to positions; loop.
                (let [new-boxes (distinct
                                 (for [[x y :as p] next-positions
                                       :let [at-p (get grid p)
                                             ;; _ (println "for, at-p" p at-p)
                                             ]
                                       :when (some #{\[\]} [at-p])]
                                   (if (= \[ at-p)
                                     p
                                     [(dec x) y])))
                      new-positions (apply concat (map (fn [[x y]] (list [x y] [(inc x) y])) new-boxes))]
                  ;; (println "new-boxes" new-boxes)
                  (recur new-positions (concat moving-boxes new-boxes)))))))))

(defn move-next-wide [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state}]
  (let [move (first moves)
        move-fn (make-move-fn move)
        boxes-ahead (moveable-boxes-ahead-wide grid robot-pos move)
        ;; _ (println "boxes-ahead" boxes-ahead)
        [new-grid new-robot-pos] (cond
                                   ;; No space - robot can't move.
                                   (nil? boxes-ahead)
                                   [grid robot-pos]
                                   ;; Robot moves into an empty space or pushes boxes.
                                   :else
                                   (let [new-positions (map move-fn boxes-ahead)
                                         empty-spaces (remove (set new-positions) boxes-ahead)
                                         grid-with-moved-boxes (reduce (fn [grid [x y :as box]]
                                                                         (-> grid
                                                                             (assoc box \[)
                                                                             (assoc [(inc x) y] \])))
                                                                       grid
                                                                       new-positions)
                                         grid-with-empty-spaces (reduce (fn [grid [x y :as space]]
                                                                          (-> grid
                                                                              (assoc space \.)
                                                                              (assoc [(inc x) y] \.)))
                                                                        grid-with-moved-boxes
                                                                        empty-spaces)]
                                     [grid-with-empty-spaces (move-fn robot-pos)]))]
    (-> state
        (assoc :moves (rest moves))
        (assoc :grid new-grid)
        (assoc :robot-pos new-robot-pos))))

(defn predict-robot-wide [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as state}]
  (loop [state state]
    (if (empty? (:moves state))
      state
      (recur (move-next-wide state)))))

(defn sum-gps-coordinates-wide [input]
  (let [{grid :grid width :width height :height robot-pos :robot-pos moves :moves :as end-state} (predict-robot-wide (parse-input-wide input))]
    (print-grid end-state)
    (apply +
           (for [i (range width)
                 j (range height)
                 :when (= \O (get grid [i j]))]
             (+ (* 100 j) i)))))
