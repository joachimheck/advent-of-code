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
       (mapv #(map parse-long %))))
    

;; Part 1
;; Find the largest rectangle whose opposite corners are red tiles.

(defn distance [[x y] [i j]]
  (math/sqrt
   (+ (math/pow (- y j) 2)
      (math/pow (- x i) 2))))

(defn rect-area [[x y] [i j]]
  (abs (* (inc (- x i)) (inc (- y j)))))

(defn pairs [points]
  (loop [points points
         pairs []]
    (if (empty? points)
      pairs
      (let [f (first points)
            r (rest points)]
        (recur r (concat pairs (map #(list f %) r)))))))

(defn find-largest-rectangle [input]
  (last (last (sort-by last (map (fn [[a b]] (list a b (rect-area a b))) (pairs (parse-input input)))))))


;; (time (find-largest-rectangle small-input))
;; "Elapsed time: 1.0978 msecs"
;; 50
;; (time (find-largest-rectangle large-input))
;; "Elapsed time: 11246.9088 msecs"
;; 4774877510


;; Part 2
;; Find the largest rectangle entirely within the bounds of the shape.
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

           
