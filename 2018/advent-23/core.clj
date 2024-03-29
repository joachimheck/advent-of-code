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

(defn cube-sizes [[[minx1 miny1 minz1] [maxx1 maxy1 maxz1]]]
  [(- (inc maxx1) minx1) (- (inc maxy1) miny1) (- (inc maxz1) minz1)])

(defn cube-size [[[minx1 miny1 minz1] [maxx1 maxy1 maxz1] :as cube]]
  (long (apply max (cube-sizes cube))))

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



;; Ok, so reddit says the trick is to repeatedly divide the total space into cubes, and for each
;; cube, count how many bots could have a space in that cube in range. That gives an upper bound
;; to the number of overlaps inside that cube, meaning you can ignore that whole cube if you've
;; already found a _point_ with a better overlap number.
(defn divide-cube [[[minx miny minz] [maxx maxy maxz] :as cube]]
  (let [xhalf-1 (+ minx (quot (- maxx minx) 2))
        xhalf-2 (inc xhalf-1)
        yhalf-1 (+ miny (quot (- maxy miny) 2))
        yhalf-2 (inc yhalf-1)
        zhalf-1 (+ minz (quot (- maxz minz) 2))
        zhalf-2 (inc zhalf-1)]
    (list [[(long minx) (long miny) (long minz)] [(long xhalf-1) (long yhalf-1) (long zhalf-1)]]
          [[(long xhalf-2) (long miny) (long minz)] [(long maxx) (long yhalf-1) (long zhalf-1)]]
          [[(long minx) (long yhalf-2) (long minz)] [(long xhalf-1) (long maxy) (long zhalf-1)]]
          [[(long xhalf-2) (long yhalf-2) (long minz)] [(long maxx) (long maxy) (long zhalf-1)]]
          [[(long minx) (long miny) (long zhalf-2)] [(long xhalf-1) (long yhalf-1) (long maxz)]]
          [[(long xhalf-2) (long miny) (long zhalf-2)] [(long maxx) (long yhalf-1) (long maxz)]]
          [[(long minx) (long yhalf-2) (long zhalf-2)] [(long xhalf-1) (long maxy) (long maxz)]]
          [[(long xhalf-2) (long yhalf-2) (long zhalf-2)] [(long maxx) (long maxy) (long maxz)]])))

(defn get-bounding-cube [bots]
  (let [[[minx miny minz] _ :as bounds] (get-bounds bots)
        bounds-size (cube-size bounds)
        cube-size (dec (loop [p 0]
                         (if (> (Math/pow 2 p) bounds-size)
                           (Math/pow 2 p)
                           (recur (inc p)))))]
    [[minx miny minz] [(+ minx cube-size) (+ miny cube-size) (+ minz cube-size)]]))

(defn cubes-overlap? [[[minx1 miny1 minz1] [maxx1 maxy1 maxz1] :as cube1] [[minx2 miny2 minz2] [maxx2 maxy2 maxz2] :as cube2]]
  ;; (println "cubes-overlap" cube1 cube2 (or (and (<= minx1 minx2 maxx1) (<= miny1 miny2 maxy1) (<= minz1 minz2 maxz1))
  ;;                                          (and (<= minx1 maxx2 maxx1) (<= miny1 maxy2 maxy1) (<= minz1 maxz2 maxz1))))
  (or (and (<= minx1 minx2 maxx1) (<= miny1 miny2 maxy1) (<= minz1 minz2 maxz1))
      (and (<= minx1 maxx2 maxx1) (<= miny1 maxy2 maxy1) (<= minz1 maxz2 maxz1))
      (and (<= minx2 minx1 maxx2) (<= miny2 miny1 maxy2) (<= minz2 minz1 maxz2))
      (and (<= minx2 maxx1 maxx2) (<= miny2 maxy1 maxy2) (<= minz2 maxz1 maxz2))))

(defn cube-vertices [[[minx miny minz] [maxx maxy maxz] :as cube]]
  (list [minx miny minz]
        [maxx miny minz]
        [minx maxy minz]
        [maxx maxy minz]
        [minx miny maxz]
        [maxx miny maxz]
        [minx maxy maxz]
        [maxx maxy maxz]))

(defn closest-vertex [[x y z :as pos] [[minx miny minz] [maxx maxy maxz] :as cube]]
  (first (sort-by #(distance pos %) (cube-vertices cube))))

(defn in-range-of-cube? [[[bx by bz :as bot-pos] br :as bot] [[minx miny minz :as min-pos] [maxx maxy maxz] :as cube]]
  (if (= 1 (cube-size cube))
    (in-range? min-pos bot)
    (let [[x-radius y-radius z-radius] [(/ (- (inc maxx) minx) 2) (/ (- (inc maxy) miny) 2) (/ (- (inc maxz) minz) 2)]
          [cx cy cz :as center] [(+ minx x-radius) (+ miny y-radius) (+ minz z-radius)]
          distance-to-cube (cond (and (<= minx bx maxx) (<= miny by maxy) (<= minz bz maxz))
                                 0
                                 (and (<= miny by maxy) (<= minz bz maxz))
                                 (- (abs (- bx cx)) x-radius)
                                 (and (<= minx bx maxx) (<= minz bz maxz))
                                 (- (abs (- by cy)) y-radius)
                                 (and (<= minx bx maxx) (<= miny by maxy))
                                 (- (abs (- bz cz)) z-radius)
                                 :else
                                 (distance bot-pos (closest-vertex bot-pos cube)))]
      (<= distance-to-cube br))))

(defn assign-bots-to-sub-cubes [bots bounding-cube]
  (let [sub-cubes (divide-cube bounding-cube)]
    (for [[min-pos _ :as sub-cube] sub-cubes]
      (let [bots-in-range (for [bot bots
                                :when (in-range-of-cube? bot sub-cube)]
                            bot)]
        [sub-cube bots-in-range (count bots-in-range)]))))

(defn cube-vertices [[[minx miny minz] [maxx maxy maxz]]]
  (list [minx miny minz]
        [maxx miny minz]
        [minx maxy minz]
        [maxx maxy minz]
        [minx miny maxz]
        [maxx miny maxz]
        [minx maxy maxz]
        [maxx maxy maxz]))

(defn cube-distance-to-origin [cube]
  (apply min (map (fn [v] (distance v [0 0 0])) (cube-vertices cube))))

(defn compare-cubes-with-bots [[c1 b1 n1] [c2 b2 n2]]
  (if (= n1 n2)
    (let [c1-size (cube-size c1)
          c2-size (cube-size c2)]
      (if (= c1-size c2-size)
        (- (cube-distance-to-origin c1) (cube-distance-to-origin c2))
        (- c2-size c1-size)))
    (- n2 n1)))

(defn output-map [open-set i]
  (let [[cube bots bot-count] (first open-set)
        max-bots (if (empty? open-set) 0 (apply max (map (fn [[c b n]] n) open-set)))
        cubes-with-max (for [[c b n] open-set :when (= n max-bots)] c)
        maxed-cube-count (count cubes-with-max)]
   {:cube cube
    :distance (cube-distance-to-origin cube)
    :bots bot-count
    :size (cube-size cube)
    :max-bots max-bots
    :cubes (count open-set)
    :maxed-cube-count maxed-cube-count
    :smallest-cube (if (empty? open-set) nil (apply min (map (fn [[c b n]] (cube-size c)) open-set)))
    :iterations i
    :cubes-with-max (if (and (= 1 (cube-size cube)) (< maxed-cube-count 10)) cubes-with-max)}))

(defn find-result-subdivision [bots max-i]
  (loop [open-set (sorted-set-by compare-cubes-with-bots [(get-bounding-cube bots) (seq bots) 1])
         i 0]
    ;; (println "loop open-set" open-set)
    (let [[cube cube-bots cube-count :as current] (first open-set)]
     (if (or (empty? open-set)
             (= 1 (cube-size cube))
             (= i max-i))
       (output-map open-set i)
       (let [_ (if (= 0 (mod i 20)) (println (output-map open-set i)))
             bots-by-cube (assign-bots-to-sub-cubes bots cube)
             new-open-set (into (disj open-set current) bots-by-cube)
             ]
         (recur new-open-set (inc i)))))))

;; Looks like the cube subdivision method has an off-by-one error.

;; (find-result-subdivision (parse-input large-input) 3)
;; {:best [[9223372036854775807 9223372036854775807 9223372036854775807] 0], :max-bots 516, :smallest-cube 6.7108863E7}
;; advent-23.core> (Math/pow 2 26)
;; 6.7108864E7


;; 86532477
;; 86803689
;; ---> answer <---
;; 120911897 ; after 50 iterations

;; Other incorrect answers:
;; 94481124
;; 94481126

(defn count-bots-visible [bots pos]
  (count (filter true? (map (fn [bot] (in-range? pos bot)) bots))))


;; This finally got it! I found a Reddit comment that explained my bug: when sorting the cubes,
;; I wasn't taking into account their size. The sort criteria need to be: number of bots visible
;; from within the cube; _the_cube_size!_; and the distance of the cube from the origin. With that
;; change in place, not only is the solution correct, but it's also discovered very quickly.

;; (time (find-result-subdivision (parse-input large-input) 10000))
;; {:maxed-cube-count 1, :bots 1, :size 536870912, :iterations 0, :cube [[-178506040 -141655043 -96576988] [3.58364871E8 3.95215868E8 4.40293923E8]], :cubes 1, :distance 416738071, :smallest-cube 536870912, :max-bots 1, :cubes-with-max nil}
;; {:maxed-cube-count 3, :bots 977, :size 65536, :iterations 20, :cube [[26752712 46564349 20994596] [26818247 46629884 21060131]], :cubes 134, :distance 94311657, :smallest-cube 65536, :max-bots 977, :cubes-with-max nil}
;; {:maxed-cube-count 2, :bots 977, :size 512, :iterations 40, :cube [[26794696 46607357 21078564] [26795207 46607868 21079075]], :cubes 257, :distance 94480617, :smallest-cube 256, :max-bots 977, :cubes-with-max nil}
;; {:maxed-cube-count 2, :bots 977, :size 4, :iterations 60, :cube [[26794904 46607437 21078784] [26794907 46607440 21078787]], :cubes 383, :distance 94481125, :smallest-cube 2, :max-bots 977, :cubes-with-max nil}
;; "Elapsed time: 3034.8029 msecs"
;; {:maxed-cube-count 1,
;;  :bots 977,
;;  :size 1,
;;  :iterations 63,
;;  :cube [[26794906 46607439 21078785] [26794906 46607439 21078785]],
;;  :cubes 404,
;;  :distance 94481130,
;;  :smallest-cube 1,
;;  :max-bots 977,
;;  :cubes-with-max ([[26794906 46607439 21078785] [26794906 46607439 21078785]])}
;; advent-23.core> 
