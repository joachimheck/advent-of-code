(ns advent-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (->> input
       (read-lines)
       (map #(str/split % #","))
       (mapv #(mapv parse-long %))))
    

;; Part 1
;; Find the largest rectangle whose opposite corners are red tiles.

(defn distance [[x y] [i j]]
  (math/sqrt
   (+ (math/pow (- y j) 2)
      (math/pow (- x i) 2))))

(defn rect-area [[x y] [i j]]
  (* (inc (abs (- x i))) (inc (abs (- y j)))))

(defn pairs [points]
  (loop [points points
         pairs []]
    (if (empty? points)
      pairs
      (let [f (first points)
            r (rest points)]
        (recur r (concat pairs (map #(vector f %) r)))))))

(defn cross [as bs]
  (loop [as as
         pairs []]
    (if (empty? as)
      pairs
      (let [a (first as)
            a-rest (rest as)]
        (recur a-rest (concat pairs (map #(vector a %) bs)))))))

(defn find-largest-rectangle [input]
  (last (last (sort-by last (map (fn [[a b]] (list a b (rect-area a b))) (pairs (parse-input input)))))))


;; (time (find-largest-rectangle small-input))
;; "Elapsed time: 1.0978 msecs"
;; 50
;; (time (find-largest-rectangle large-input))
;; "Elapsed time: 11246.9088 msecs"
;; 4774877510


;; Part 2
;; Find the largest red-cornered rectangle entirely within the bounds of the shape.
(defn draw [red green]
  (let [[xmin ymin xmax ymax :as bounds] [(apply min (map first red)) (apply min (map second red))
                                          (apply max (map first red)) (apply max (map second red))]]
    (for [j (range ymin (inc ymax))]
      (println
       (str/join             
        (for [i (range xmin (inc xmax))]
          (cond (some #{[i j]} red) "#"
                (some #{[i j]} green) "X"
                :else " ")))))))

(defn define-shape [points]
  (let [points (conj points (first points))]
    (partition 2 1 points)))

(defn find-green-lines [red]
  (let [red (conj red (first red))
        pairs (partition 2 1 red)]
    (loop [pairs pairs
           green []]
      (if (empty? pairs)
        green
        (recur (rest pairs)
               (concat green
                       (let [[[x y :as a] [i j :as b]] (first pairs)
                             inc-fn (cond (and (= x i) (>= j y)) (fn [[x y]] (vector x (inc y)))
                                          (and (= x i) (< j y)) (fn [[x y]] (vector x (dec y)))
                                          (and (= y j) (>= i x)) (fn [[x y]] (vector (inc x) y))
                                          (and (= y j) (< i x)) (fn [[x y]] (vector (dec x) y)))]
                         (loop [[x y] [x y]
                                points []]
                           (if (= [x y] [i j])
                             points
                             (recur (inc-fn [x y]) (conj points [x y])))))))))))

(defn get-direction [[x y] [i j]]
  (cond (and (= x i) (>= j y)) :s
        (and (= x i) (< j y)) :n
        (and (= y j) (>= i x)) :e
        (and (= y j) (< i x)) :w))

(def directions [:e :s :w :n])

(defn right-turn [d]
  (second (drop-while #(not= % d) (flatten (repeat 2 directions)))))

(defn opposite [d]
  (d {:e :w :s :n :w :e :n :s}))

(defn get-turn [current next]
  (if (or (and (= current :e) (= next :s))
          (and (= current :s) (= next :w))
          (and (= current :w) (= next :n))
          (and (= current :n) (= next :e)))
    :r :l))

(defn inside-point [red shape-dir]
  (let [[i j :as a] (last red)
        b (first red)
        [k l :as c] (second red)
        dir-ab (get-direction a b)
        dir-bc (get-direction b c)
        start-turn (get-turn dir-ab dir-bc)
        x-fn (if (= :r shape-dir)
               (cond (and (>= k i) (>= l j)) (fn [[x y]] [(dec x) (inc y)])
                     (and (>= k i) (< l j)) (fn [[x y]] [(inc x) (inc y)])
                     (and (< k i) (>= l j)) (fn [[x y]] [(dec x) (dec y)])
                     (and (< k i) (< l j)) (fn [[x y]] [(inc x) (dec y)]))
               (cond (and (>= k i) (>= l j)) (fn [[x y]] [(inc x) (dec y)])
                     (and (>= k i) (< l j)) (fn [[x y]] [(dec x) (dec y)])
                     (and (< k i) (>= l j)) (fn [[x y]] [(inc x) (inc y)])
                     (and (< k i) (< l j)) (fn [[x y]] [(dec x) (inc y)])))
        ]
    (x-fn (first red))))

(deftest test-inside-point
  (is (= [1 1] (inside-point [[0 0] [2 0] [2 2] [0 2]] :r)))
  (is (= [1 1] (inside-point [[2 0] [2 2] [0 2] [0 0]] :r)))
  (is (= [1 1] (inside-point [[2 2] [0 2] [0 0] [2 0]] :r)))
  (is (= [1 1] (inside-point [[0 2] [0 0] [2 0] [2 2]] :r)))
  (is (= [1 1] (inside-point [[0 0] [0 2] [2 2] [2 0]] :l)))
  (is (= [1 1] (inside-point [[0 2] [2 2] [2 0] [0 0]] :l)))
  (is (= [1 1] (inside-point [[2 2] [2 0] [0 0] [0 2]] :l)))
  (is (= [1 1] (inside-point [[2 0] [0 0] [0 2] [2 2]] :l))))

(defn find-inside [red]
  (let [looped-red (conj red (first red))
        pairs (partition 2 1 looped-red)
        directions (map (fn [[[x y] [i j]]] (get-direction [x y] [i j])) pairs)
        turns (loop [directions directions
                     turns {:r 0 :l 0}]
                (if (<= (count directions) 1)
                  turns
                  (let [current (first directions)
                        next (first (rest directions))]
                    (recur (rest directions)
                           (update turns (get-turn current next) inc)))))
        shape-dir (if (> (:r turns) (:l turns)) :r :l)]
    (inside-point red shape-dir)))

(defn adjacent [[x y]]
  [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]])

(defn fill [red]
  (let [lines (set (find-green-lines red))]
    (loop [current [(find-inside red)]
           filled #{}]
      (if (empty? current)
        {:red red :green (concat lines filled)}
        (recur (remove filled (remove lines (mapcat adjacent current)))
               (set (concat filled current)))))))


(defn line-segments [red]
  (map (fn [[[i j :as a] [k l :as b]]]
         (let [dir-ab (get-direction a b)
               right (right-turn dir-ab)]
           [a b right]))
       (partition 2 1 (conj red (first red)))))

(defn inside-dirs [segments]
  (let [dirs (mapv last segments)
        turns (map #(apply get-turn %) (partition 2 1 (conj dirs (first dirs))))
        turn-freqs (into {:r 0 :l 0} (frequencies turns))]
    (if (> (:r turn-freqs) (:l turn-freqs))
      segments
      (map (fn [[a b d]] [a b (opposite d)]) segments))))

(deftest test-inside-dirs
  (is (= :s (last (first (inside-dirs (line-segments [[0 0] [2 0] [2 2] [0 2]]))))))
  (is (= :e (last (first (inside-dirs (line-segments [[0 0] [0 2] [2 2] [2 0]])))))))

(defn perpendicular [[[lx1 ly1] [lx2 ly2] :as line] [[sx1 sy1] [sx2 sy2] :as segment]]
  (or (and (= lx1 lx2) (not= sx1 sx2))
      (and (not= lx1 lx2) (= sx1 sx2))))

(defn between-inclusive? [n a b]
  (if (< a b) (<= a n b) (<= b n a)))

(defn between-exclusive? [n a b]
  (if (< a b) (< a n b) (< b n a)))

(defn intersect? [[[a b] [c d] :as s1] [[e f] [g h] :as s2]]
  (or (and (= a c) (between? f b d) (between? a e g))
      (and (= b d) (between? b f h) (between? e a c))))

(defn intersection-point [[[a b] [c d] :as s1] [[e f] [g h] :as s2]]
  (cond (and (= a c) (= f h)) [f c]
        (and (= b d) (= e g)) [e b]))

(defn in-segment? [[x y] [p1 p2]]
  (let [[[sx1 sy1] [sx2 sy2]] (sort [p1 p2])]
    (if (= x sx1 sx2) (<= sy1 y sy2)
        (<= sx1 x sx2))))

(defn adjacent-points [[[a b] [c d] :as line] segment]
  (let [[x y :as ip] (intersection-point line segment)
        raw-adjacents (if (= a c) [[x (inc y)] [x (dec y)]]
                          [[(inc x) y] [(dec x) y]])
        adjacents (filter #(in-segment? % line) raw-adjacents)]
    adjacents))

(defn intersections [[[lx1 ly1] [lx2 ly2] :as line] segments]
  (remove empty?
          (for [[[sx1 sy1] [sx2 sy2] :as segment] segments]
            (if (intersect? line segment)
              {:line line :segment segment}))))

(defn rect-lines [[[x y] [i j]]]
  [[[x y] [i y]]
   [[i y] [i j]]
   [[i j] [x j]]
   [[x j] [x y]]])

(defn crossing? [[[a b] [c d] :as s1] [[e f] [g h] :as s2]]
  (or (and (= a c) (= f h) (between-exclusive? f b d) (between-exclusive? a e g))
      (and (= b d) (= e g) (between-exclusive? b f h) (between-exclusive? e a c))))

(defn overlapping? [[[a b] [c d] :as s1] [[e f] [g h] :as s2]]
  (or (and (= a c e g) (or (between-inclusive? b f h) (between-inclusive? d f h)))
      (and (= b d f h) (or (between-inclusive? a e g) (between-inclusive? c e g)))))

(defn adjacent-dir [[x y :as p] dir]
  (case dir
    :e [(inc x) y]
    :s [x (inc y)]
    :w [(dec x) y]
    :n [x (dec y)]))

(defn dir-to [[x y :as a] [i j :as b]]
  (cond (and (= x i) (> j y)) :s
        (and (= x i) (< j y)) :n
        (and (= y j) (> i x)) :e
        (and (= y j) (< i x)) :w))
        
(defn other-point [p [a b]]
  (if (= p a) b a))

;; TODO: Find the outside points around each corner, then determine whether overlapping
;; lines travel through the outside points.

(defn find-largest-interior-rectangle [input]
  (let [red (parse-input input)
        segments (line-segments red)
        corners (pairs red)
        rectangles (map #(assoc {} :corners % :lines (rect-lines %) :area (apply rect-area %)) corners)
        rectangle-crossings (map (fn [{:keys [corners lines] :as rectangle}]
                                   (let [with-crossings (assoc rectangle :any-crossings?
                                                               (not (empty? (filter #(true? (:crossing? %))
                                                                                    (map (fn [[l s]] {:line l :segment s :crossing? (crossing? l s)}) (cross lines segments))))))
                                         with-overlaps (assoc with-crossings :any-overlaps?
                                                              (not (empty? (filter #(true? (:overlapping? %))
                                                                                   (map (fn [[l s]] {:line l :segment s :overlapping? (overlapping? l s)}) (cross lines segments))))))]
                                     with-overlaps))
                                 rectangles)
        test-corners [[7 1] [11 7]]
        test-rect (rect-lines test-corners)
        crossing-map (filter #(true? (:crossing? %))
                             (map (fn [[l s]] {:line l :segment s :crossing? (crossing? l s)}) (cross test-rect segments)))
        ]
    ;; (remove :any-overlaps? (remove :any-crossings? rectangle-crossings))
    rectangle-crossings
    ))

;;  01234567890123
;; 0..............
;; 1.......#XXX#..
;; 2.......X...X..
;; 3..#XXXX#...X..
;; 4..X........X..
;; 5..#XXXXXX#.X..
;; 6.........X.X..
;; 7.........#X#..
;; 8..............

