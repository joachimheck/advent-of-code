(ns advent-24.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (str/split-lines (slurp f)))

(defn parse-line [s]
  (map (comp keyword first) (re-seq #"(e|w|nw|ne|sw|se)" s)))

(defn parse-input [ls]
  (map parse-line ls))

;; Part 1
;; Flip tiles along a path, then count them.
;; Grid coordinates: [x y] where, if y is even, moving ne/se yields the same x
;;   [-1 1]  [0 1]           [0 4]  [1 4]
;; [-1 0] [0 0] [1 0]     [-1 3] [0 3] [1 3]
;;   [-1 -1] [0 -1]          [0 2]  [1 2]

(defn move [[x y] dir]
  (cond
    (= dir :e) [(inc x) y]
    (= dir :w) [(dec x) y]
    (= dir :ne) [(if (even? y) x (inc x)) (inc y)]
    (= dir :se) [(if (even? y) x (inc x)) (dec y)]
    (= dir :nw) [(if (even? y) (dec x) x) (inc y)]
    (= dir :sw) [(if (even? y) (dec x) x) (dec y)]))

(defn tiles-in-path [start path]
  (reduce
   (fn [acc dir]
     (conj acc (move (last acc) dir)))
   [start]
   path))

(defn find-tile [path]
  (last (tiles-in-path [0 0] path)))

(defn flip-tiles [paths]
  (into #{}
        (reduce
         (fn [acc pos]
           (if (some #{pos} acc)
             (remove #(= pos %) acc)
             (conj acc pos)))
         '()
         (map find-tile paths))))


;; (count (flip-tiles (parse-input (read-input small-input))))
;; => 10

;; (count (flip-tiles (parse-input (read-input large-input))))
;; => 549



;; Part 2
;; Game of Life again!
;; A black tile with 0 or more than 2 adjacent black tiles turns white
;; A white tile with exactly 2 adjacent black tiles turns black

(defn surrounding-tiles [[x y]]
  (let [incx (inc x) decx (dec x) incy (inc y) decy (dec y)]
    (if (even? y)
      [[incx y] [x incy] [decx incy] [decx y] [decx decy] [x decy]]
      [[incx y] [incx incy] [x incy] [decx y] [x decy] [incx decy]])))

(defn tile-and-surrounding [tile]
  (conj (surrounding-tiles tile) tile))

(defn count-black-neighbors [tile black-tiles]
  (count (filter (set black-tiles) (surrounding-tiles tile))))

(defn new-color [black-tiles tile]
  (let [neighbors (count-black-neighbors tile black-tiles)
        color (if (some #{tile} black-tiles) :black :white)]
    (list color
          (if (= color :black)
            (if (or (= neighbors 0) (> neighbors 2))
              :white
              :black)
            (if (= neighbors 2)
              :black
              :white)))))

(defn get-active-tiles [tiles]
  (reduce into #{} (map tile-and-surrounding tiles)))

(defn process-tiles [black-tiles]
  (->> black-tiles
       ((fn [tiles] (time (set tiles))))
       ((fn [tiles] (time (get-active-tiles tiles))))
       ((fn [tiles] (time (map (fn [tile] (list tile (new-color black-tiles tile))) tiles))))
       ((fn [tile-colors-list] (time (remove
                                      (fn [[tile [old-color new-color]]]
                                        (= old-color new-color))
                                      tile-colors-list))))

       ((fn make-groups [tile-colors-list] (time
                                            (group-by
                                             (fn old-color [[_ [old-color _]]] old-color) tile-colors-list))))

       ((fn proc-groups [groups] (time (apply disj
                                  (apply conj black-tiles (map first (get groups :white)))
                                  (map first (get groups :black))))))
       ((fn count-tiles [tiles]
          (println (count black-tiles) "black tiles" (count tiles) "active tiles")
          tiles))
       ))

;; (time (count (nth (iterate process-tiles (flip-tiles (parse-input (read-input small-input)))) 10)))
;; => 37
;; "Elapsed time: 26.1478 msecs"

;; (time (count (nth (iterate process-tiles (flip-tiles (parse-input (read-input small-input)))) 100)))
;; => 2208
;; "Elapsed time: 58719.7325 msecs"
;; "Elapsed time: 24193.9398 msecs" - various improvements, especially making black-tiles a set

;; for timing
;;(time (count (nth (iterate process-tiles (flip-tiles (parse-input (read-input small-input)))) 30)))

(time (count (nth (iterate process-tiles (flip-tiles (parse-input (read-input small-input)))) 30)))
