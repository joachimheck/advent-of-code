(ns advent-23.core)

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

;; Day 23: Experimental Emergency Teleportation

;; Part 1
;; How many nanobots are in range of the strongest nanobot?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(let [[_ x y z r] (re-matches #"pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)" %)] {(mapv parse-long [x y z]) (parse-long r)}))
       (apply merge)))

(defn get-strongest-bot [bots]
  (apply max-key val bots))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2)) (abs (- z1 z2))))

(defn in-range [[pos range] bots]
  (filter (fn [[t-pos _]] (<= (distance pos t-pos) range)) bots))

(defn count-in-range-of-strongest [bots]
  (let [strongest (get-strongest-bot bots)]
    (count (in-range strongest bots))))

;; (time (count-in-range-of-strongest (parse-input small-input)))
;; "Elapsed time: 0.526101 msecs"
;; 7

;; (time (count-in-range-of-strongest (parse-input large-input)))
;; "Elapsed time: 5.3118 msecs"
;; 599



;; Part 2
;; What's the distance from the origin to the closest point in range of the most nanobots?
(defn get-bounds [bots]
  (reduce (fn [[[minx miny minz] [maxx maxy maxz]] [[x y z] _]]
            [[(min minx x) (min miny y) (min minz z)] [(max maxx x) (max maxy y) (max maxz z)]])
          [[Integer/MAX_VALUE Integer/MAX_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]]
          bots))

;; (defn volume-in-range [[x y z] r]
;;   r = 0 : 1
;;   r = 1 : 7 (6 + 1)
;;   r = 2 : 19 (12 + 7)
;; )

(defn pairs [coll]
  (cond (< (count coll) 2) nil
        (= (count coll) 2) (list (into #{} coll))
        :else (concat (map (fn [x] #{(first coll) x}) (rest coll))
                      (pairs (rest coll)))))

(defn overlap? [[pos-a r-a] [pos-b r-b]]
  (<= (distance pos-a pos-b) (+ r-a r-b)))

;; (defn overlap-counts [bots]
;;   (let [bot-pairs (pairs bots)
;;         overlaps (apply merge (map (fn [pair] {pair (apply overlap? pair)}) bot-pairs))]
;;     (apply merge
;;            (for [bot bots]
;;              {bot
;;               (count
;;                (for [member-pair (filter #(some #{bot} %) bot-pairs)
;;                      :when (get overlaps member-pair)]
;;                  member-pair))}))))

(defn overlap-counts [bots]
  (let [bot-pairs (pairs bots)
        overlaps (filter #(apply overlap? %) bot-pairs)]
    (reduce (fn [acc pair]
              (let [[a b] (seq pair)]
                  (-> acc
                      (update a (fnil inc 0))
                      (update b (fnil inc 0)))))
            {}
            overlaps)))

;; (time (apply max-key second (overlap-counts (parse-input small-input))))
;; "Elapsed time: 1.9619 msecs"
;; [[[0 0 0] 4] 8]

(defn overlap-sets [bots]
  (let [bot-pairs (pairs bots)
        self-pairs (map (fn [b] (list b b)) bots)
        overlaps (concat self-pairs
                         (apply concat (map (fn [x] (list x (reverse x))) (map seq (filter #(apply overlap? %) bot-pairs)))))]
    ;; overlaps
    (apply merge-with
           #(apply conj %1 %2)
           (map (fn [[a b]] {a #{b}}) overlaps))
    ))

(def test-input {[0 0 0] 2 [1 0 0] 1 [2 0 0] 2 [5 0 0] 2})

(defn largest-multi-overlap [bots]
  (let [overlap-sets (overlap-sets bots)]
    (first
     (apply max-key #(count (second %))
            (apply merge-with concat
                   (apply concat
                          (for [bot bots
                                :let [overlaps (get overlap-sets bot)]]
                            (map (fn [overlap] {overlap (list bot)})
                                 (distinct (for [o overlaps
                                                 :when (not= o bot)]
                                             (set/intersection overlaps (get overlap-sets o))))))))))))

(def small-input-2 "small-input-2.txt")

;; (largest-multi-overlap (parse-input small-input-2))
;; #{[[16 12 12] 4] [[10 12 12] 2] [[10 10 10] 5] [[50 50 50] 200] [[14 14 14] 6] [[12 14 12] 2]}
;; Seems to be wrong - [10 10 10] shouldn't be in an overlap with everything else.

(defn points-in-range [[[x y z] r]]
  (for [i (range (- x r) (+ x r 1))
        j (range (- y r) (+ y r 1))
        k (range (- z r) (+ z r 1))
        :when (<= (distance [x y z] [i j k]) r)]
    [i j k]))

;; This function is very slow when handling large ranges.
(defn find-best-coordinate [bots]
  (let [overlap (sort-by second (largest-multi-overlap bots))]
    (println "all overlaps" overlap)
    (loop [bots (rest overlap)
           points (set (points-in-range (first overlap)))
           i 0]
      (println "#points" (count points) "bot" (first bots))
      (if (or (= i 10) (empty? bots) (<= (count points) 1))
        points
        (recur (rest bots) (set/intersection points (set (points-in-range (first bots)))) (inc i))))))

;; It works ok when it quickly narrows down the range and each volume is small.
;; (time (find-best-coordinate (parse-input small-input-2)))
;; all overlaps ([[10 12 12] 2] [[12 14 12] 2] [[16 12 12] 4] [[14 14 14] 6] [[50 50 50] 200])
;; #points 25 bot [[12 14 12] 2]
;; #points 3 bot [[16 12 12] 4]
;; #points 1 bot [[14 14 14] 6]
;; "Elapsed time: 1.9154 msecs"
;; #{[12 12 12]}

;; Gotta do it geometrically, I guess.
;; Looks like the equation of a 45 degree plane is z = +/-x +/-y + C, where C is a sum of the
;; X/Y positions of the bot and its range.

(defn get-bot-bounds [[[x y z] r]]
  [[(- x r) (- y r) (- z r)] [(+ x r) (+ y r) (+ z r)]])

(defn bounds-overlap [[[minx1 miny1 minz1] [maxx1 maxy1 maxz1]] [[minx2 miny2 minz2] [maxx2 maxy2 maxz2]]]
  [[(max minx1 minx2) (max miny1 miny2) (max minz1 minz2)] [(min maxx1 maxx2) (min maxy1 maxy2) (min maxz1 maxz2)]])

(defn cube-size [[[minx1 miny1 minz1] [maxx1 maxy1 maxz1]]]
  [(- maxx1 minx1) (- maxy1 miny1) (- maxz1 minz1)])

(defn reduce-cubes [bots]
  (let [bot-bounds (map get-bot-bounds bots)]
    (reduce (fn [acc bounds] (bounds-overlap acc bounds)) (first bot-bounds) (rest bot-bounds))))

(defn center [[[ax ay az] [bx by bz]]]
  [(- bx ax) (- by ay) (- bz az)])

;; I think I will have to find a way to represent irregular octohedra, or whatever shape I get
;; from the intersection of two octohedra, in order to solve this. The cubic intersections don't
;; get very small.

;; advent-23.core> (time (reduce-cubes (parse-input large-input)))
;; "Elapsed time: 587263.2027 msecs"
;; [[23599608 38673221 15966726] [29463190 52155743 26664909]]
;; advent-23.core> (cube-size [[23599608 38673221 15966726] [29463190 52155743 26664909]])
;; [5863582 13482522 10698183]
;; advent-23.core> (apply * [5863582 13482522 10698183])
;; Execution error (ArithmeticException) at java.lang.Math/multiplyExact (REPL:-1).
;; long overflow

(defn vertices [[[x y z] r :as bot]]
  (list
   [(+ x r) y z]
   [x (+ y r) z]
   [(- x r) y z]
   [x (- y r) z]
   [x y (+ z r)]
   [x y (- z r)]))

(defn in-range? [pos [b-pos b-r :as bot]]
  (<= (distance pos b-pos) b-r))

(defn in-range-of-all? [pos bots]
  (every? #(in-range? pos %) bots))

(defn find-minimum-vertices [bots]
  (let [overlapping (largest-multi-overlap bots)
        _ (println "Found" (count overlapping) "overlapping bots.")
        o-vertices (apply concat (map vertices overlapping))]
    (sort (distinct (filter #(in-range-of-all? % overlapping) o-vertices)))))


;; (time (find-minimum-vertices (parse-input large-input)))
;; Found 977 overlapping bots.
;; "Elapsed time: 537500.8867 msecs"
;; ()


;; Planes
;; For [[0 0 0] 2]
;; For ([2 0 0] [0 2 0] [0 0 2]): 4x + 4y + 4z + -8 = 0
;; For ([-2 0 0] [0 2 0] [0 0 2]): 4x + -4y + -4z + 8 = 0
;; For ([-2 0 0] [0 -2 0] [0 0 2]): -4x + -4y + 4z + -8 = 0
;; For ([2 0 0] [0 -2 0] [0 0 2]): -4x + 4y + -4z + 8 = 0
(defn plane-equation [[alpha-x alpha-y alpha-z] [beta-x beta-y beta-z] [gamma-x gamma-y gamma-z]]
  (let [a (- (* (- beta-y alpha-y) (- gamma-z alpha-z)) (* (- gamma-y alpha-y) (- beta-z alpha-z)))
        b (- (* (- beta-z alpha-z) (- gamma-x alpha-x)) (* (- gamma-z alpha-z) (- beta-x alpha-x)))
        c (- (* (- beta-x alpha-x) (- gamma-y alpha-y)) (* (- gamma-x alpha-x) (- beta-y alpha-y)))
        d (- (+ (* a alpha-x) (* b alpha-y) (* c alpha-z)))]
    (list a b c d)))


;; Need to compute the vertices of the overlapping shape.
(defn overlapping-vertices [[[ax ay az] ar :as a] [[bx by bz] br :as b]]
  (let [a-vertices (vertices a)
        b-vertices (vertices b)
        a-in-range (filter #(<= (distance % [bx by bz]) br) a-vertices)
        b-in-range (filter #(<= (distance % [ax ay az]) ar) b-vertices)]
    (list a-in-range b-in-range)))

(defn adjacent-vertices [[[x y z] r :as bot] [vx vy vz :as vertex]]
  (let [[diff-x diff-y diff-z] [(- vx x) (- vy y) (- vz z)]
        opposite [(- x diff-x) (- y diff-y) (- z diff-z)]]
    (remove #{vertex opposite} (vertices bot))))

(defn line-segments-from-vertices [vs]
  (let [max-x (apply max-key #(nth % 0) vs)
        min-x (apply min-key #(nth % 0) vs)
        max-y (apply max-key #(nth % 1) vs)
        min-y (apply min-key #(nth % 1) vs)
        max-z (apply max-key #(nth % 2) vs)
        min-z (apply min-key #(nth % 2) vs)]
    (list #{max-x max-y} #{max-x min-y} #{max-x max-z} #{max-x min-z}
          #{max-y max-z} #{max-z min-y} #{min-y min-z} #{min-z max-y}
          #{min-x max-y} #{min-x min-y} #{min-x max-z} #{min-x min-z})))

(defn planes-from-vertices [vs]
  (let [max-x (apply max-key #(nth % 0) vs)
        min-x (apply min-key #(nth % 0) vs)
        max-y (apply max-key #(nth % 1) vs)
        min-y (apply min-key #(nth % 1) vs)
        max-z (apply max-key #(nth % 2) vs)
        min-z (apply min-key #(nth % 2) vs)]
    (list (plane-equation max-x max-y max-z)
          (plane-equation max-x max-z min-y)
          (plane-equation max-x min-y min-z)
          (plane-equation max-x min-z max-y)
          (plane-equation min-x max-y max-z)
          (plane-equation min-x max-z min-y)
          (plane-equation min-x min-y min-z)
          (plane-equation min-x min-z max-y))))

;; Hm, this depends on plane-equation, which doesn't work for the big numbers we have in the input.
;; Furthermore, I guess the intersection of two octrahedra is not an octahedron after all.
;; Maybe I need to make a list of convex volumes?
;; I saved a diagram of intersecting pyramids in geogebra.org - it's a roof-like shape.
