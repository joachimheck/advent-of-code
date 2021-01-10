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
  ([tile] (borders tile false))
  ([tile rev?]
   (let [borders (map #(get-border tile %) sides)]
     (if rev?
       (concat borders (map str/reverse borders))
       borders))))

(defn find-matching-borders [ts t]
  (list t
        (for [t2 ts
              :when (not= t2 t)
              b1 (borders (get tmap t) true)
              b2 (borders (get tmap t2))
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

;(defn find-grid
;  [ts]

(defn rotate-until-first [l e]
  (if (or (empty? l) (= e (first l))) l
      (rotate-until-first (concat (list (last l)) (butlast l)) e)))

(defn arrange-rings
  ([rings] (arrange-rings [] rings))
  ([arranged rings]
   (cond (empty? arranged) (arrange-rings [(first rings)] (rest rings))
         (empty? rings) arranged
         :else
         (let [outer (last arranged)
               inner (first rings)
               corner (list (last outer) (second outer))
               first-inner (first (first
                                   (filter #(every? (set (second %)) corner)
                                           (map #(list % (find-neighbors tids %)) inner))))]
           (arrange-rings (conj arranged (rotate-until-first inner first-inner)) (rest rings))
           ;;  (some (set corner)
           ;;  (list corner (first inner))
           ))))


(defn ring-coords
  ([size] (ring-coords size [0 0]))
  ([size [x y]]
   (cond (= size 1) '([0 0])
         (= [x y] [0 1]) (list [0 1])
         (and (= y 0) (< x (dec size))) (concat (list [x y]) (ring-coords size [(inc x) y]))
         (and (= x (dec size)) (< y (dec size))) (concat (list [x y]) (ring-coords size [x (inc y)]))
         (and (= y (dec size)) (> x 0)) (concat (list [x y]) (ring-coords size [(dec x) y]))
         (and (= x 0) (> y 0)) (concat (list [x y]) (ring-coords size [x (dec y)])))))


(defn get-coordinates [rings size]
  (apply concat
         (let [rings (find-rings tids)
               size 3]
           (for [;;ring rings
                 i (range (count rings))
                 ]
             (let [ring (nth rings i)
                   shifted-coords (map (fn [[x y]] [(+ x i) (+ y i)]) (ring-coords (- size (* 2 i))))
                   ]
               (map vector shifted-coords ring)
               )))))

(defn print-tiles [tiles]
  (map println
       (concat
        (for [i (range 10)]
          (str/join
           " "
           (for [t tiles]
             (nth t i))))
        '(""))))

(defn print-tile-row [ts]
  (print-tiles (map #(get tmap %) ts)))

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



;; 1951 2311 3079
;; 2729 1427 2473
;; 2971 1489 1171

;; (borders (first (take 2 (first (find-rings tids)))))

;; Just get the matching border between two tiles
(defn get-border-between [tile1 tile2]
  (some (set (borders tile1 true)) (borders tile2)))

(defn borders-indexed [tile]
  (map vector sides (borders tile)))

(defn get-side [tile bdr]
  (first
   (first
    (filter (fn [x]
              ;;(println (set (list bdr (str/reverse bdr))) x)
              (some (set (list bdr (str/reverse bdr))) x)
) (borders-indexed tile)))))

(defn get-flips [tile]
  (concat (take 4 (iterate rotate tile))
          (take 4 (iterate rotate (flip-horizontal tile)))))

;; nbrs are in clockwise order.
(defn get-adjusted-tile [tile expected-sides nbrs]
  (let [flips (get-flips tile)]
    (first (keep (fn [tile]
                   (if (= expected-sides
                          (list (get-side tile (get-border-between (first nbrs) tile))
                                (get-side tile (get-border-between tile (second nbrs)))))
                     tile))
                 flips))))

(defn get-expected-sides [i side-size]
  (let [sides-list (partition 3 (take 12 (cycle (reverse sides))))
        side-idx (quot i (dec side-size))
        pos (mod i (dec side-size))
        [s1 s2 s3] (nth sides-list side-idx)]
    (if (= 0 pos) (list s2 s3)
        (list s1 s3))))

(def blank-tile (repeat 10 "----------"))

(map print-tiles
     ;;(second
     (let [adjusteds
           (let [triplets (partition 3 1 (let [rings (find-rings tids)
                                               ring (first rings)]
                                           (concat (list (last ring)) ring (list (first ring)))))
                 side-size 3]
             (map-indexed
              ;;#(list %1 %2)
              (fn [i [n1 t n2]]
                (get-adjusted-tile
                 (get tmap t)
                 (get-expected-sides i side-size)
                 (list (get tmap n1) (get tmap n2))))
              triplets)
             )]
       (list
        (take 3 adjusteds)
        (concat (list (last adjusteds)) (list blank-tile) (list (nth adjusteds 3)))
        (reverse (take 3 (take-last 4 adjusteds)))
        )
       )
     )

;; (get-adjusted-tile (get tmap 1951) '(:bottom :right) (map (partial get tmap) '(2729 2311)))


"TODO"
;; Now I know the location of each tile. For each tile, get its neighbors and their locations,
;; then orient the tile using the relative positions of its neighbors.
