(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many steps to get to the opposite side of the loop?
(defn parse-input [input]
  (let [vectorized (mapv vec (read-lines input))]
    (into {}
          (for [j (range (count vectorized))
                i (range (count (first vectorized)))]
            [[i j] (get-in vectorized [j i])]))))

(defn get-connected [[x y] grid]
  (let [pipe (get grid [x y])]
    (concat []
            (cond (= pipe \|) [[x (dec y)] [x (inc y)]]
                  (= pipe \-) [[(dec x) y] [(inc x) y]]
                  (= pipe \L) [[x (dec y)] [(inc x) y]]
                  (= pipe \J) [[x (dec y)] [(dec x) y]]
                  (= pipe \7) [[(dec x) y] [x (inc y)]]
                  (= pipe \F) [[(inc x) y] [x (inc y)]]))))

(defn get-adjacent [[x y]]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn get-adjacent-connected [[x y] grid]
  (for [p (get-adjacent [x y])
         :let [[ap cs] (list p (set (get-connected p grid)))]
         :when (contains? cs [x y])]
     ap))

(defn map-loop [grid]
  (let [start-pos (first (first (filter (fn [[k v]] (= \S v)) grid)))
        p1 (first (get-adjacent-connected start-pos grid))]
    (loop [path [start-pos p1]]
      (let [current (last path)
            prev (last (butlast path))
            next-p (first (filter #(not= % prev) (get-connected current grid)))]
        (if (= next-p start-pos)
          path
          (recur (conj path next-p)))))))

(defn steps-to-opposite-side [input]
  (quot (count (map-loop (parse-input input))) 2))

;; (time (steps-to-opposite-side small-input))
;; "Elapsed time: 1.2343 msecs"
;; 8

;; (time (steps-to-opposite-side large-input))
;; "Elapsed time: 12193.2141 msecs"
;; 7030



;; Part 2
;; How many tiles are enclosed by the loop?
(defn symbol-from-neighbors [p n1 n2]
  (let [relative (set [(mapv - n1 p) (mapv - n2 p)])]
    (cond (= relative #{[0 -1] [0 1]}) \|
          (= relative #{[-1 0] [1 0]}) \-
          (= relative #{[0 -1] [1 0]}) \L
          (= relative #{[0 -1] [-1 0]}) \J
          (= relative #{[0 1] [-1 0]}) \7
          (= relative #{[0 1] [1 0]}) \F)))

(deftest test-symbol-from-neighbors
  (is (= \| (symbol-from-neighbors [3 3] [3 2] [3 4])))
  (is (= \- (symbol-from-neighbors [3 3] [2 3] [4 3])))
  (is (= \L (symbol-from-neighbors [3 3] [3 2] [4 3])))
  (is (= \J (symbol-from-neighbors [3 3] [3 2] [2 3])))
  (is (= \7 (symbol-from-neighbors [3 3] [2 3] [3 4])))
  (is (= \F (symbol-from-neighbors [3 3] [4 3] [3 4]))))

(defn print-grid [grid]
  (let [dim-x (inc (apply max (map first (keys grid))))
        dim-y (inc (apply max (map second (keys grid))))
        output (str/join "\n"
                         (for [j (range dim-y)]
                           (str/join
                            (for [i (range dim-x)]
                              (or (get grid [i j]) " ")))))]
    (println output)))

(defn are-connected? [a b grid]
  (and (contains? (set (get-connected a grid)) b)
       (contains? (set (get-connected b grid)) a)))

(defn make-double-grid [grid]
  (let [dim-x (inc (apply max (map first (keys grid))))
        dim-y (inc (apply max (map second (keys grid))))
        base (into {} (map (fn [[x y]] [[x y] \space])
                           (for [i (range (* 2 dim-x)) j (range (* 2 dim-y))] [i j])))
        base (into base (map (fn [[[x y] v]] [[(* 2 x) (* 2 y)] v]) grid))]
    (reduce (fn [acc [x y]]
              (let [[n1 n2] (if (even? x)
                              [[(quot x 2) (quot (dec y) 2)] [(quot x 2) (quot (inc y) 2)]]
                              [[(quot (dec x) 2) (quot y 2)] [(quot (inc x) 2) (quot y 2)]])]
                (conj acc [[x y] (if (are-connected? n1 n2 grid)
                                   \+
                                   \space)])))
            base
            (for [i (range (* 2 dim-x)) j (range (* 2 dim-y)) :when (or (and (odd? i) (even? j)) (and (even? i) (odd? j)))] [i j]))))

(defn prep-grid [grid]
  (let [start-pos (first (first (filter (fn [[k v]] (= \S v)) grid)))
        path (map-loop grid)
        grid (assoc grid start-pos (symbol-from-neighbors start-pos (second path) (last path)))
        path-set (set path)
        grid (into {} (map (fn [[k v]] [k (if (contains? path-set k) v \.)]) grid))
        double-grid (make-double-grid grid)]
    double-grid))

(defn in-grid? [[x y] dim-x dim-y]
  (and (< -1 x dim-x) (< -1 y dim-y)))

(defn count-enclosed [input]
  (let [grid (prep-grid (parse-input input))
        outside (loop [open-set #{[0 0]}
                       visited #{}
                       loop-count 0]
                  (let [new-open-set (set (remove visited (filter #(some #{\space \.} (list (get grid %))) (mapcat get-adjacent open-set))))]
                    (if (or (empty? open-set) (= new-open-set open-set))
                      visited
                      (recur new-open-set
                             (into visited open-set)
                             (inc loop-count)))))
        outside-marked (into {} (map (fn [[k v]] (if (contains? outside k) [k \o] [k v])) grid))]
    (count (filter #(= \. %) (vals outside-marked)))))

;; (time (count-enclosed small-input-2))
;; "Elapsed time: 4.829 msecs"
;; 4

;; (time (count-enclosed large-input))
;; "Elapsed time: 16130.7834 msecs"
;; 285
