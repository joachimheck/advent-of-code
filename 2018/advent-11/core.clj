(ns advent-11.core)

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

;; Day 11: Chronal Change

;; Part 1
;; Which square of fuel cells has the largest total power?
(defn hundreds-digit [n]
  (rem (quot n 100) 10))

(defn power-level [[x y] serial-number]
  (let [rack-id (+ x 10)]
    (- (hundreds-digit (* rack-id (+ (* rack-id y) serial-number))) 5)))

(deftest test-power-level
  (is (= 4 (power-level [3 5] 8)))
  (is (= -5 (power-level [122 79] 57)))
  (is (= 0 (power-level [217 196] 39)))
  (is (= 4 (power-level [101 153] 71))))

(defn generate-grid [serial-number]
  (into {} (for [i (range 1 301)
                 j (range 1 301)]
             {[i j] (power-level [i j] serial-number)})))

(defn square-coordinates [[x y]]
  (for [i (range x (+ x 3))
        j (range y (+ y 3))]
    [i j]))

(defn square-power [[x y :as p] grid]
  (let [coordinates (square-coordinates p)]
    (reduce + (map #(get grid %) coordinates))))

(deftest test-square-power
  (is (= 29 (square-power [33 45] (generate-grid 18))))
  (is (= 30 (square-power [21 61] (generate-grid 42)))))

(defn find-largest-square [serial-number]
  (let [grid (generate-grid serial-number)
        square-powers (for [i (range 1 299)
                            j (range 1 299)]
                        (list [i j] (square-power [i j] grid)))]
    (last (sort-by second square-powers))))

;; (time (find-largest-square 18))
;; "Elapsed time: 1321.6556 msecs"
;; ([33 45] 29)

;; (time (find-largest-square 42))
;; "Elapsed time: 1324.0126 msecs"
;; ([21 61] 30)

;; (time (find-largest-square 1723))
;; "Elapsed time: 1219.3957 msecs"
;; ([34 13] 30)



;; Part 2
;; Squares of any size are allowed. What's the square with the largest total power?

(defn generate-grid-2 [serial-number size]
  (into {} (for [i (range 1 (inc size))
                 j (range 1 (inc size))]
             {[i j 1] (power-level [i j] serial-number)})))

(defn square-coordinates-2 [[x y s]]
  (for [i (range x (+ x s))
        j (range y (+ y s))]
    [i j s]))

(defn square-power-2 [[x y s :as p] grid]
  (let [coordinates (square-coordinates-2 p)]
    (reduce + (map #(let [v (get grid %)]
                      (if (nil? v)
                        (println "exception on coordinates" % coordinates)
                        v))
                   coordinates))))

(defn generate-larger-grid [square-size grids max-size]
  (let [grid-sizes (reverse (sort (keys grids)))
        factor (first (filter #(= 0 (rem square-size %)) grid-sizes))
        step (quot square-size factor)
        smaller-grid (get grids factor)]
    (into {}
          (for [i (range 1 (- (+ 2 max-size) square-size))
                j (range 1 (- (+ 2 max-size) square-size))]
            {[i j square-size]
             (apply +
                    (for [sub-i (range step)
                          sub-j (range step)]
                      (get smaller-grid [(+ i (* sub-i factor))
                                         (+ j (* sub-j factor))
                                         factor])))}))))

(defn generate-square-power-grids [serial-number grid-size]
  (loop [size 2 grids {1 (generate-grid-2 serial-number grid-size)}]
    (if (> size grid-size)
      grids
      (recur (inc size) (assoc grids size (generate-larger-grid size grids grid-size))))))

(deftest test-square-power-2
  (is (= 113 (square-power-2 [90 269 16] (generate-grid-2 18))))
  (is (= 119 (square-power-2 [232 251 12] (generate-grid-2 42)))))

(defn generate-square-grids-simply [serial-number max-square-size full-grid-size]
  (loop [grids (generate-grid-2 serial-number full-grid-size)
         square-size 2]
    (if (> square-size max-square-size)
      grids
      (recur (into grids
                   (for [i (range 1 (- (+ 2 full-grid-size) square-size))
                         j (range 1 (- (+ 2 full-grid-size) square-size))]
                     {[i j square-size]
                      (apply +
                             (for [sub-i (range square-size)
                                   sub-j (range square-size)]
                               (get grids [(+ i sub-i) (+ j sub-j) 1])))}))
             (inc square-size)))))

(defn generate-square-grids-extending [serial-number max-square-size full-grid-size]
  (let [grid (generate-grid-2 serial-number full-grid-size)]
    (loop [square-size 2
           previous-grid grid
           max-values (into {} (vector (apply max-key val grid)))]
      (if (> square-size max-square-size)
        max-values
        (let [new-grid (into {}
                             (for [i (range 1 (- (+ 2 full-grid-size) square-size))
                                   j (range 1 (- (+ 2 full-grid-size) square-size))]
                               {[i j square-size]
                                (apply + (concat (list (get previous-grid [i j (dec square-size)]))
                                                 (for [sub-i (range (dec square-size))]
                                                   (get grid [(+ i sub-i) (+ j (dec square-size)) 1]))
                                                 (for [sub-j (range (dec square-size))]
                                                   (get grid [(+ i (dec square-size)) (+ j sub-j) 1]))
                                                 (list (get grid [(+ i (dec square-size)) (+ j (dec square-size)) 1]))))}))]
          (if (= 0 (mod square-size 10))
            (println "Square size" square-size "at" (.toString (new java.util.Date))))
          (recur (inc square-size)
                 new-grid
                 (into max-values (vector (apply max-key val new-grid)))))))))

(defn find-best-square [serial-number max-square-size full-grid-size]
  (let [[[x y size] power] (apply max-key val (generate-square-grids-extending serial-number max-square-size full-grid-size))]
    (format "Square %d,%d,%d has total power %d" x y size power)))

;; (time (find-best-square 1723 300 300))
;; Square size 10 at Sat Feb 25 10:53:34 MST 2023
;; Square size 20 at Sat Feb 25 10:53:58 MST 2023
;; Square size 30 at Sat Feb 25 10:54:32 MST 2023
;; Square size 40 at Sat Feb 25 10:55:18 MST 2023
;; Square size 50 at Sat Feb 25 10:56:08 MST 2023
;; Square size 60 at Sat Feb 25 10:57:05 MST 2023
;; Square size 70 at Sat Feb 25 10:58:08 MST 2023
;; Square size 80 at Sat Feb 25 10:59:13 MST 2023
;; Square size 90 at Sat Feb 25 11:00:20 MST 2023
;; Square size 100 at Sat Feb 25 11:01:34 MST 2023
;; Square size 110 at Sat Feb 25 11:02:51 MST 2023
;; Square size 120 at Sat Feb 25 11:04:01 MST 2023
;; Square size 130 at Sat Feb 25 11:05:13 MST 2023
;; Square size 140 at Sat Feb 25 11:06:22 MST 2023
;; Square size 150 at Sat Feb 25 11:07:30 MST 2023
;; Square size 160 at Sat Feb 25 11:08:32 MST 2023
;; Square size 170 at Sat Feb 25 11:09:30 MST 2023
;; Square size 180 at Sat Feb 25 11:10:23 MST 2023
;; Square size 190 at Sat Feb 25 11:11:10 MST 2023
;; Square size 200 at Sat Feb 25 11:11:52 MST 2023
;; Square size 210 at Sat Feb 25 11:12:28 MST 2023
;; Square size 220 at Sat Feb 25 11:12:58 MST 2023
;; Square size 230 at Sat Feb 25 11:13:23 MST 2023
;; Square size 240 at Sat Feb 25 11:13:42 MST 2023
;; Square size 250 at Sat Feb 25 11:13:56 MST 2023
;; Square size 260 at Sat Feb 25 11:14:06 MST 2023
;; Square size 270 at Sat Feb 25 11:14:13 MST 2023
;; Square size 280 at Sat Feb 25 11:14:16 MST 2023
;; Square size 290 at Sat Feb 25 11:14:18 MST 2023
;; Square size 300 at Sat Feb 25 11:14:18 MST 2023
;; "Elapsed time: 1254279.9752 msecs"
;; "Square 280,218,11 has total power 68"
