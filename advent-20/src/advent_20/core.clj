(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall
     (reduce
      (fn [[l m] tile-lines]
        (let [id (Long/parseLong (second (re-matches #"Tile (\d+):" (first tile-lines))))
              lines (rest tile-lines)]
          (list (conj l id) (assoc m id lines))))
      '([] {})
      (remove #(= 1 (count %)) (partition-by #(empty? %) (line-seq rdr)))))))

(def input (read-input small-input))
(def tids (first input))
(def tmap (second input))
(def sides '(:top :right :bottom :left))

;; Part 1
;; Match tiles up by their borders. The borders may need to be reversed to match.
(defn get-border [tile side]
  (case side
    :top (first tile)
    :right (reduce #(str %1 (last %2)) "" tile)
    :bottom (last tile)
    :left (reduce #(str %1 (first %2)) "" tile)))

(defn borders
  ([tid] (borders tid false))
  ([tid rev?]
   (let [lines (get tmap tid)
         borders (for [side sides]
                   (get-border lines side))]
     (if rev?
       (concat borders (map str/reverse borders))
       borders))))

(defn find-matching-borders [ts t]
  (list t
        (for [t2 ts
              :when (not= t2 t)
              b1 (borders t true)
              b2 (borders t2)
              :when (= b1 b2)]
          (list t2 b2))))

(defn find-neighbors [ts t]
  (map first (second (find-matching-borders ts t))))

(defn multiply-corners [ts]
  (->> ts
       (map #(list % (find-neighbors ts %))) ; tiles/neighbors
       (filter #(= 2 (count (second %)))) ; corners (2 neighbors)
       (map first) ; id strings
       (reduce * 1)))

;; (multiply-corners tids)
;; 20899048083289



;; Part 2
;; I didn't actually assemble the image in part 1 so I have to do that now
;; Then look for sea monsters!
(defn with-neighbors [ts n]
  (->> ts
       (map #(list % (find-neighbors ts %))) ; tiles/neighbors
       (filter #(= n (count (second %))))
       (map first)
       ;; (map #(get ts (first %)))
       ))

(defn find-corners [ts] (with-neighbors ts 2))
(defn find-sides [ts] (with-neighbors ts 3))

;; To map the edges:
;; start at a corner
;; take a neighbor
;; find its other neighbor, etc
;; until we get back to the start tile

(defn find-next-border-tile [ts t prev]
  (first (remove #(= % prev) (find-neighbors ts t))))

(defn find-border
  ([ts]
   (if (= 1 (count ts)) ts
       (let [border-tiles (concat (find-corners ts) (find-sides ts))
             start-corner (first border-tiles)]
         (find-border border-tiles start-corner start-corner nil))))
  ([ts start t prev]
   (let [next (find-next-border-tile ts t prev)]
     (if (= next start) (list t)
         (cons t (find-border ts start next t))))))
    
;; OK, I need to find the border tiles in successive rings.
;; Start with the outer tiles like I've done.
;; After that ignore those tiles when counting neighbors and look for the next ring in
;; Recurse until all tiles have been used up.
;; Somehow match the position and orientation of the tiles.


;(remove #(some #{tids} %) (find-border tids))

(defn find-rings
  ([ts] (find-rings [] ts))
  ([rings ts]
   (if (empty? ts) rings
       (let [border (find-border ts)]
         (find-rings
          (conj rings border)
          (remove (set (find-border ts)) ts)
          )))))


(defn print-tile-row [ts]
  (print-tiles (map #(get tmap %) ts)))

(defn print-tiles [tiles]
  (map println
       (concat
        (for [i (range 10)]
          (str/join
           " "
           (for [t tiles]
             (nth t i))))
        '(""))))

(defn flip-horizontal [tlines]
  (map str/reverse tlines))

(defn flip-vertical [tlines]
  (reverse tlines))

(defn flip-diagonal [tlines]
  (let [size (count (first tlines))
        joined (str/join tlines)]
    (map str/join
         (for [i (range size)]
           (for [j (range size)]
             (let [idx (+ (* j size) i)]
             (subs joined idx (inc idx))))))))

(defn rotate
  "Rotates the tile clockwise 90 degrees."
  [tlines]
  (let [size (count (first tlines))
        joined (str/join tlines)]
    (map str/join
         (for [i (range size)]
           (for [j (reverse (range size))]
             (let [idx (+ (* j size) i)]
               (subs joined idx (inc idx))))))))



;; (borders (first (take 2 (first (find-rings tids)))))
;; 1951 2311

(for [v1 (map vector sides (borders 1951))
      ;;v2 (map vector sides (borders 2311))
      v2 (map vector sides (borders 2729))
      :when (= (second v1) (second v2))]
  v1)
