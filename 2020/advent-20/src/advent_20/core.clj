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

;;(def input (read-input small-input))
(def input (read-input large-input))
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
;; The eventual operations:
;; Find the corners, then their neighbors, and build the outer ring.
;; Remove the outer ring, repeat above to build succesive rings.
;; Align rings by checking neighbors
;; Compute grid coordinates for tiles
;; Process tiles in grid order, removing outer character ring.
;; Merge lines of tiles by partitioning them by the width of the tile array.
;; Now we have the whole image.
;; Partition the image into three-line (height of sea monster) groups.
;; Partition the groups into 20 (width of sea monster) character wide frames.
;; Check whether sea-monster matches frame.
;; Sum sea monster count, count hashes in image, subtract.
(defn with-neighbors [ts n]
  (->> ts
       (map #(list % (find-neighbors ts %))) ; tiles/neighbors
       (filter #(= n (count (second %))))
       (map first)
       ;; (map #(get ts (first %)))
       ))

(defn find-corners [ts] (with-neighbors ts 2))
(defn find-sides [ts] (with-neighbors ts 3))

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

(defn find-rings
  ([ts] (find-rings [] ts))
  ([rings ts]
   (if (empty? ts) rings
       (let [border (find-border ts)]
         (find-rings
          (conj rings border)
          (remove (set (find-border ts)) ts)
          )))))

(defn rotate-until-first [l e]
  (if (or (empty? l) (= e (first l))) l
      (rotate-until-first (concat (list (last l)) (butlast l)) e)))

(defn print-tiles [tiles]
  (map println
       (concat
        (for [i (range (count (first tiles)))]
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
  (let [xsize (count (first tlines))
        ysize (count tlines)
        joined (str/join tlines)]
    (map str/join
         (for [i (range xsize)]
           (for [j (reverse (range ysize))]
             (let [idx (+ (* j xsize) i)]
               (subs joined idx (inc idx))))))))

;; Just get the matching border between two tiles
(defn get-border-between [tile1 tile2]
  (some (set (borders tile1 false)) (borders tile2 true)))

(defn borders-indexed [tile]
  (map vector sides (borders tile)))

(defn get-side [tile bdr]
  (first
   (first
    (filter (fn [x]
              ;;(println (set (list bdr (str/reverse bdr))) x)
              (some (set (list bdr (str/reverse bdr))) x)) (borders-indexed tile)))))

(defn get-flips [tile]
  (concat (take 4 (iterate rotate tile))
          (take 4 (iterate rotate (flip-horizontal tile)))))

;; nbrs are in clockwise order from top, may be nil.
(defn adjust-tile [tile nbrs]
  (let [flips (get-flips tile)]
    (first (keep (fn [flip]
                   (let [bdrs (borders flip false)]
                     (when (reduce #(and %1 %2)
                                   (for [i (range (count sides))]
                                     (or (nil? (nth nbrs i))
                                         (= (nth bdrs i) (get-border-between flip (nth nbrs i))))))
                       flip)))
                 flips))))

(defn inner-ring-corner [inner corner]
  (first (first
          (filter #(every? (set (second %)) corner)
                  (map #(list % (find-neighbors tids %)) inner)))))

(defn has-neighbor [t t2]
  (some #{t2} (find-neighbors tids t)))

(defn arrange-inner-ring [inner outer]
  (if (= 1 (count inner))
    inner
    (let [corner (list (last outer) (second outer))
          first-inner (inner-ring-corner inner corner)
          corner-matched (rotate-until-first inner first-inner)]
      (let [next-outer (nth outer 2)]
        (if (has-neighbor (second corner-matched) next-outer)
          corner-matched
          (rotate-until-first (reverse corner-matched) first-inner)
          )))))

(defn arrange-rings
  ([rings] (arrange-rings [] rings))
  ([arranged rings]
   (cond (empty? arranged) (arrange-rings [(first rings)] (rest rings))
         (empty? rings) arranged
         :else
         (let [outer (last arranged)
               inner (first rings)]
           (arrange-rings (conj arranged (arrange-inner-ring inner outer)) (rest rings))))))

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
  (reduce
   #(assoc %1 (second %2) (first %2))
   {}
   (apply concat
          (for [i (range (count rings))]
            (let [ring (nth rings i)
                  shifted-coords (map (fn [[x y]] [(+ x i) (+ y i)]) (ring-coords (- size (* 2 i))))]
              (map list ring shifted-coords)
              )))))

(defn adjacent-coords [[x y]]
  (vector [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]))

(defn neighbors [coord-map [x y]]
  (map (partial get coord-map) (adjacent-coords [x y])))

(defn arrange-tiles [size]
  (let [rings (arrange-rings (find-rings tids))
        coords (get-coordinates rings size)]
    (->> coords
         (map #(list % (neighbors coords (first %))))
         (map (fn [[[[x y] t] nbrs]] (list [[x y] t] (adjust-tile (get tmap t) (map #(get tmap %) nbrs)))))
         (reduce (fn [acc [[[x y] t] lines]] (assoc acc [x y] lines)) {})
         (#(for [y (range size) x (range size)] (get % [x y])))
         )))

(defn combine-tiles [size tiles]
  (->> tiles
       (map #(rest (butlast %))) ; Remove top and bottom
       ;; Remove first and last character
       (map (fn [lines]
              (map (fn [l]
                     (subs l 1 (- (count l) 1))) lines)))
       (partition size) ; Group by rows
       ;; Join rows to form wide tile
       (map
        (fn [tiles]
          (map str/join (apply (partial map list) tiles))))
       ;; Join lines to form tall tile
       (reduce concat)
       ))

(def sea-monster
  '("                  # "
    "#    ##    ##    ###"
    " #  #  #  #  #  #   "))

(defn int-sqrt [square]
  (some #(when (= square (* % %)) %) (range square)))

(defn match-hashes [exp target]
  (->> (map vector exp target)
       (reduce
        (fn [acc [e t]] (and acc (or (not= e \#) (= t \#))))
        true)
       ))

(defn count-sea-monsters [image]
  (->> image
       (partition 3 1)
       (map
        (fn [three-lines]
          (->> three-lines
               (map #(map (partial apply str) (partition 20 1 %)))
               ((fn [row] (for [i (range 3)] (nth row i))))
               ((fn [segs] (apply #(map list %1 %2 %3) segs)))
               (map (fn [frame]
                      (list
                       frame
                       (for [i (range 3)]
                         (match-hashes
                          (nth sea-monster i)
                          (nth frame i))
                         ))))
               (keep (fn [[f [a b c]]] (when (and a b c) true))))))
       (map count)
       (reduce +)))

(defn count-hashes [tile]
  (reduce + (map (fn [line] (count (keep #{\#} line))) tile)))

(defn compute-water-roughness []
  (let [size (int-sqrt (count tids))]
    (->> (arrange-tiles size)
         (combine-tiles size)
         get-flips
         (map (fn [flip] (list (count-sea-monsters flip) (count-hashes flip) flip)))
         (keep #(when (> (first %) 0) %))
         first
         ;; 15 hashes per sea monster
         (#(- (second %) (* 15 (first %)))))))

;; (compute-water-roughness)
;; 1792
