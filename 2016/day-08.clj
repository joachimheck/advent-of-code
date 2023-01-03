(ns day-08.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(defn make-screen [width height]
  {:width width :height height :pixels (vec (for [i (range height) j (range width)] \.))})

(def test-screen (make-screen 7 3))

(defn set-pixel [screen x y p]
  (update screen :pixels (fn [pixels]
                           (assoc pixels (+ x (* y (get screen :width))) p))))
(defn draw-screen [screen]
  (let [width (get screen :width)
        height (get screen :height)
        pixels (get screen :pixels)]
    (println "screen" width "x" height)
    (for [i (range height)]
      (let [row (str/join (subvec pixels (* i width) (+ width (* i width))))]
        (println row)
        row))))

(defn rect [screen width height]
  (reduce
   (fn [screen [x y]] (set-pixel screen x y \#))
   screen
   (for [j (range height) i (range width)] [i j])))

(defn get-pixel [screen x y]
  (let [width (get screen :width)
        pixels (get screen :pixels)]
    (get pixels (+ x (* width y)))))

(defn get-row [screen rownum]
  (vec
   (for [i (range (get screen :width))]
     (get-pixel screen i rownum))))

(defn get-column [screen colnum]
  (vec
   (for [j (range (get screen :height))]
     (get-pixel screen colnum j))))

(defn rotate-row [screen rownum amt]
  (let [row (get-row screen rownum)
        width (get screen :width)
        shifted (vec (take width (drop (- width amt) (cycle row))))]
    (reduce
     (fn [screen x] (set-pixel screen x rownum (get shifted x)))
     screen
     (range width))))

(defn rotate-column [screen colnum amt]
  (let [col (get-column screen colnum)
        height (get screen :height)
        shifted (vec (take height (drop (- height amt) (cycle col))))]
    (reduce
     (fn [screen y] (set-pixel screen colnum y (get shifted y)))
     screen
     (range height))))


(def test-input '("rect 3x2"
                  "rotate column x=1 by 1"
                  "rotate row y=0 by 4"
                  "rotate column x=1 by 1"))

(defn parse-instruction [instruction]
  (cond
    (str/starts-with? instruction "rect")
    (let [[_ width height] (re-matches #"rect (\d+)x(\d+)" instruction)]
      (format "(rect %s %s)" width height))

    (str/starts-with? instruction "rotate column")
    (let [[_ col amt] (re-matches #"rotate column x=(\d+) by (\d+)" instruction)]
      (format "(rotate-column %s %s)" col amt))

    (str/starts-with? instruction "rotate row")
    (let [[_ row amt] (re-matches #"rotate row y=(\d+) by (\d+)" instruction)]
      (format "(rotate-row %s %s)" row amt))))

(defn evaluate-instructions [width height instructions]
  (eval
   (read-string
    (str/join
     (concat (list "(->" (format "(make-screen %d %d)" width height))
             (map parse-instruction instructions)
             '("draw-screen)"))))))

;; (evaluate-instructions 7 3 test-input)
;; => .#..#.#
;;    #.#....
;;    .#.....
;;    (".#..#.#" "#.#...." ".#.....")

;; (count
;;  (filter #(= % \#)
;;          (apply concat (evaluate-instructions 50 6 (str/split-lines (slurp "input-08.txt"))))))
;; => 119



;; Part 2
;; What is the code?
;;; (evaluate-instructions 50 6 (str/split-lines (slurp "input-08.txt")))
;; => ####.####.#..#.####..###.####..##...##..###...##..
;;    ...#.#....#..#.#....#....#....#..#.#..#.#..#.#..#.
;;    ..#..###..####.###..#....###..#..#.#....#..#.#..#.
;;    .#...#....#..#.#.....##..#....#..#.#.##.###..#..#.
;;    #....#....#..#.#.......#.#....#..#.#..#.#....#..#.
;;    ####.#....#..#.#....###..#.....##...###.#.....##..
;;    ("####.####.#..#.####..###.####..##...##..###...##.."
;;     "...#.#....#..#.#....#....#....#..#.#..#.#..#.#..#."
;;     "..#..###..####.###..#....###..#..#.#....#..#.#..#."
;;     ".#...#....#..#.#.....##..#....#..#.#.##.###..#..#."
;;     "#....#....#..#.#.......#.#....#..#.#..#.#....#..#."
;;     "####.#....#..#.#....###..#.....##...###.#.....##..")
;; ZFHFSFOGPO
