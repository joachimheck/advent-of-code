(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 11: Dumbo Octopus

;; Part 1
;; How many flashes are there after 100 steps.
;; (defn parse-input [f]
;;   (->> (read-lines f)
;;        (mapv (fn [line] (mapv #(parse-long (str %)) line)))))
(defn parse-input [f]
  (->> f
       read-lines
       str/join
       (partition 1)
       (apply concat)
       (map str)
       (mapv parse-long)))

(defn to-index [[x y]]
  (+ (* 10 y) x))

(defn get-at [grid [x y]]
  (get grid (to-index [x y])))

(defn neighbors [[x y]]
  ;; (println "neighbors" [x y])
  (filter (fn [[i j]] (and (<= 0 i 9)
                           (<= 0 j 9)))
          (list [(dec x) (dec y)]
                [x (dec y)]
                [(inc x) (dec y)]
                [(dec x) y]
                [(inc x) y]
                [(dec x) (inc y)]
                [x (inc y)]
                [(inc x) (inc y)])))

(defn draw-grid [grid width height]
  (str/join
   "\n"
   (for [j (range height)]
     (str/join
      (for [i (range width)]
        (let [o (get-at grid [i j])]
          (if (= o -1) "F" o)))))))

(defn find-flashes [grid]
  (for [j (range 10)
        i (range 10)
        :when (> (get-at grid [i j]) 9)]
    [i j]))

(defn flash-points [grid points]
  (reduce (fn [result [k v]] (assoc result k v))
          grid
          (map list (map to-index points) (repeat -1))))

(defn unflash-grid [grid]
  (mapv (fn [d] (if (= d -1) 0 d)) grid))

(defn inc-points [grid points]
  (reduce (fn [result [x y]]
            (let [index (to-index [x y])]
              (update result index #(if (= % -1) % (inc %)))))
          grid
          points))

(defn process-flashes [initial-grid]
  (loop [grid initial-grid flash-count 0 flash-step 0]
    (let [flashes (find-flashes grid)]
      (if (or (empty? flashes) (= flash-step 100))
        {:grid (unflash-grid grid) :flash-count flash-count}
        (let [flashed-grid (flash-points grid flashes)
              flash-neighbors (remove (set flashes) (mapcat neighbors flashes))]
          (recur (inc-points flashed-grid flash-neighbors) (+ flash-count (count flashes)) (inc flash-step)))))))

(defn process-grid [initial-grid max-steps]
  (loop [grid initial-grid flash-count 0 step 0]
    ;; (printf "After step %d:\n" step)
    ;; (println (draw-grid grid 10 10))
    ;; (newline)
    (if (= step max-steps)
      {:grid grid :flash-count flash-count}
      (let [flashed (process-flashes (mapv inc grid))]
        (recur (flashed :grid) (+ flash-count (flashed :flash-count)) (inc step))))))

(defn count-flashes [f steps]
  (let [grid (parse-input f)]
    (get (process-grid grid steps) :flash-count)))


;; (time (count-flashes small-input 100))
;; "Elapsed time: 57.7483 msecs"
;; 1656

;; (time (count-flashes large-input 100))
;; "Elapsed time: 59.8314 msecs"
;; 1735



;; Part 2
;; Find the first step during which all the octopuses flash.
(defn process-grid-2 [initial-grid]
  (loop [grid initial-grid flash-count 0 step 0]
    ;; (printf "After step %d:\n" step)
    ;; (println (draw-grid grid 10 10))
    ;; (newline)
    (let [flashed (process-flashes (mapv inc grid))]
        (if (= (flashed :flash-count) 100)
          {:synchronized-at-step step}
          (recur (flashed :grid) (+ flash-count (flashed :flash-count)) (inc step))))))

(defn find-synchronized-step [f]
  (let [grid (parse-input f)]
    (inc (get (process-grid-2 grid) :synchronized-at-step))))

;; (time (find-synchronized-step small-input))
;; "Elapsed time: 94.0633 msecs"
;; 195

;; (time (find-synchronized-step large-input))
;; "Elapsed time: 202.5374 msecs"
;; 400
