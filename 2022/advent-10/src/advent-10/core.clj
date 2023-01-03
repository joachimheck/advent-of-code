(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; What is the sum of six signal strengths in my communications device?
(def mini-input '("noop" "addx 3" "addx -5"))

(defn process-instruction [x inst]
  (if (= inst "noop")
    [x]
    (let [[op n] (str/split inst #" ")] ; op = "addx"
      [x (+ x (Long/parseLong n))])))

(defn run-program [x program]
  (reduce (fn [x-history inst]
            (apply conj x-history (process-instruction (last x-history) inst)))
          [1]
          program))

(defn compute-signal-strengths [f]
  (let [x-vals (run-program 1 (read-lines f))
        signal-strengths (for [i [20 60 100 140 180 220]]
                           (let [x-val (get x-vals (- i 1))]
                             (list i x-val (* i x-val))))
        sum (apply + (map last signal-strengths))]
    (list sum signal-strengths)))

;; (compute-signal-strengths small-input)
;; (13140
;;  ((20 21 420)
;;   (60 19 1140)
;;   (100 18 1800)
;;   (140 21 2940)
;;   (180 16 2880)
;;   (220 18 3960)))

;; (compute-signal-strengths large-input)
;; (17940
;;  ((20 23 460)
;;   (60 24 1440)
;;   (100 24 2400)
;;   (140 17 2380)
;;   (180 21 3780)
;;   (220 34 7480)))



;; Part 2
;; Draw pixels to the CRT based on the signal.
(defn draw-pixel? [cycle x]
  (or (= cycle (- x 1))
      (= cycle x)
      (= cycle (+ x 1))))

(defn draw-line [x-vals start]
  (str/join (map #(if (apply draw-pixel? %) "#" ".")
                 (map vector (range 0 40) (take 40 (drop start x-vals))))))

(defn draw-crt [f]
  (let [x-vals (run-program 1 (read-lines f))]
    (for [y (range 0 6)]
      (draw-line x-vals (* 40 y)))))


;; (draw-crt small-input)
;; ("##..##..##..##..##..##..##..##..##..##.."
;;  "###...###...###...###...###...###...###."
;;  "####....####....####....####....####...."
;;  "#####.....#####.....#####.....#####....."
;;  "######......######......######......####"
;;  "#######.......#######.......#######.....")

;; (draw-crt large-input)
;; ("####..##..###...##....##.####...##.####."
;;  "...#.#..#.#..#.#..#....#.#.......#....#."
;;  "..#..#....###..#..#....#.###.....#...#.."
;;  ".#...#....#..#.####....#.#.......#..#..."
;;  "#....#..#.#..#.#..#.#..#.#....#..#.#...."
;;  "####..##..###..#..#..##..#.....##..####.")
