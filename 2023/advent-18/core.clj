(ns advent-18.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many cubic meters will be dug out according to the dig plan?
(def dir-map {"R" :right "D" :down "L" :left "U" :up})

(defn parse-input [input]
  (->> (read-lines input)
       (map #(re-matches #"(.) (\d+) \(#(..)(..)(..)\)" %))
       (map rest)
       (map (fn [[dir dist r g b]] {:direction (get dir-map dir) :distance (parse-long dist) :r r :g g :b b}))))

(defn move [[x y] dir amount]
  (loop [points [[x y]]
         remaining amount]
    (if (= 0 remaining)
      (rest points)
      (let [[x y] (last points)
            next (case dir
                   :right [(inc x) y]
                   :down [x (inc y)]
                   :left [(dec x) y]
                   :up [x (dec y)])]
        (recur (conj points next) (dec remaining))))))

(defn dig-perimeter [plan]
  (let [interim (reduce (fn [{:keys [grid bounds]} {:keys [direction distance]}]
                          (let [[minx miny maxx maxy] bounds
                                points (move (last grid) direction distance)]
                            {:grid (apply conj grid points)
                             :bounds [(apply min (conj (map first points) minx))
                                      (apply min (conj (map second points) miny))
                                      (apply max (conj (map first points) maxx))
                                      (apply max (conj (map second points) maxy))]}))
                        {:grid [[0 0]] :bounds [0 0 0 0]}
                        plan)]
    (assoc interim :grid (set (:grid interim)))))

(defn pattern-to-string [{:keys [grid bounds]}]
  (let [[minx miny maxx maxy] bounds]
    (str/join "\n"
              (for [j (range miny (inc maxy))]
                (str/join
                 (for [i (range minx (inc maxx))]
                   (if (contains? grid [i j]) \# \.)))))))

(defn get-adjacent [[x y]]
  #{[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]})

(defn in-bounds [[minx miny maxx maxy] points]
  (filter (fn [[x y]] (and (< (dec minx) x (inc maxx)) (< (dec miny) y (inc maxy)))) points))

(defn find-outside [{:keys [grid bounds] :as dig-state}]
  (let [[minx miny maxx maxy] bounds
        [minx miny maxx maxy :as bounds] [(dec minx) (dec miny) (inc maxx) (inc maxy)]]
    (loop [open-set #{[minx miny]}
           visited #{}]
      (let [adjacent (in-bounds bounds (mapcat get-adjacent open-set))
            new-open-set (set (remove visited (remove grid adjacent)))]
        (if (empty? new-open-set)
          (into visited open-set)
          (recur new-open-set (into visited open-set)))))))

;; (println (pattern-to-string {:grid outside :bounds bounds}))
(defn find-inside [{:keys [grid bounds] :as dig-state}]
  (let [outside-state (find-outside dig-state)
        [minx miny maxx maxy] bounds]
    {:grid (set (for [i (range minx (inc maxx))
                      j (range miny (inc maxy))
                      :when (not (contains? outside-state [i j]))]
                  [i j]))
     :bounds bounds}))

(defn lagoon-size [input]
  (count (:grid (find-inside (dig-perimeter (parse-input input))))))

;; (lagoon-size small-input)
;; 62

;; (lagoon-size large-input)
;; 39194



;; Part 2
;; Correctly parse the input and compute the size of the lagoon again.
(def dir-hex-map {"0" :right "1" :down "2" :left "3" :up})

(defn parse-input-2 [input]
  (->> (read-lines input)
       (map #(re-matches #".+\(#(.....)(.)\)" %))
       (map rest)
       (map (fn [[dist dir]] {:direction (get dir-hex-map dir) :distance (Integer/parseInt dist 16)}))))

(defn move-2 [[x y] dir amount]
  (case dir
    :right [(+ x amount) y]
    :down [x (+ y amount)]
    :left [(- x amount) y]
    :up [x (- y amount)]))

(defn get-perimeter [digs]
  (loop [current [0 0]
         digs digs
         segments []]
    (if (empty? digs)
      segments
      (let [{:keys [direction distance]} (first digs)
            end (move-2 current direction distance)]
        (recur end
               (rest digs)
               (conj segments (vec (sort [current end]))))))))

(defn find-segment-joins [segments]
  (let [points (set (mapcat identity segments))
        x-values (sort (distinct (map first points)))
        y-values (sort (distinct (map second points)))
        vertical (filter (fn [[[a _] [b _]]] (= a b)) segments)
        horizontal (filter (fn [[[_ a] [_ b]]] (= a b)) segments)]
    (partition 2 1 (map (fn [x] (filter #(= (first (first %)) x) vertical)) x-values))))

(defn get-y-ranges [segments]
  ;; (println "get-y-ranges" segments)
  (map (fn [[[_ a] [_ b]]] (vec (sort [a b]))) segments))

(defn connect-ranges [left-ranges right-ranges]
  (println "connect-ranges" left-ranges right-ranges)
  (if (empty? left-ranges)
    ;;{:overlap right-ranges}
    {:inside '() :outside '() :overlap [right-ranges]}
    (let [inside (for [[r-min r-max :as r] right-ranges
                       [l-min l-max :as l] left-ranges
                       :when (<= l-min r-min r-max l-max)]
                   [l r])
          
          outside (for [[r-min r-max :as r] right-ranges
                        [l-min l-max :as l] left-ranges
                        :when (<= r-min l-min l-max r-max)]
                    [l r])
          overlap (for [[r-min r-max :as r] right-ranges
                        [l-min l-max :as l] left-ranges
                        :when (or (= r-min l-max) (= r-max l-min))]
                    [l r])]
      {:inside inside :outside outside :overlap overlap}
      ;; (println "inside" inside "overlap" overlap "result" (vec (concat (apply concat inside) (apply concat overlap))))
      ;; (vec (concat (apply concat inside) (apply concat overlap)))
      )))

(defn combine-connected-ranges [[[a b] [c d]]]
  (cond (= b c) [a d]
        (= b d) [c d]
        (= a d) [c b]
        (= a c) [c d]))

(defn split-inside-ranges [[[a b] [c d]]]
  (list [a c] [c d] [d b]))

;; (apply connect-ranges (map get-y-ranges (first (find-segment-joins (get-perimeter (parse-input-2 small-input))))))

;; [0 10] [10 15] => [0 15]
;; [0 10] [5 10] => [5 10]
;; [0 10] [-5 0] => [-5 10]
;; [0 10] [0 5] => [0 5]

;; Generate input for https://www.desmos.com/calculator grapher.
;; (let [points (reductions (fn [p {:keys [direction distance]}]
;;                            (move-2 p direction distance))
;;                          [0 0]
;;                          (parse-input-2 small-input))]
;;   (->> points
;;        (map (fn [[x y]] (format "(%d,%d)" x y)))
;;        (str/join ",")
;;        (format "polygon(%s)")))


(defn find-subranges [left-ranges right-ranges]
  (->> (concat left-ranges right-ranges)
       (flatten)
       (distinct)
       (sort)
       (partition 2 1)
       (map vec)))

(defn range-inside? [[lo1 hi1 :as r1] [lo2 hi2 :as r2]]
  (<= lo2 lo1 hi1 hi2))

(defn range-inside-any? [r1 ranges]
  (boolean
   (seq
    (for [r2 ranges
          :when (range-inside? r1 r2)]
      r1))))

;; for each sub-range r
;; inside right, inside left: turn region off
;; inside right, not inside left: turn region on
;; not inside right, inside left: turn region on
;; not inside right, not inside left: turn region off

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(defn combine-adjacent-ranges [ranges]
  (reduce (fn [acc [c d]]
            (let [[a b] (last acc)]
              (if (= b c)
                (conj (vec (butlast acc)) [a d])
                (conj acc [c d]))))
          [(first ranges)]
          (rest ranges)))

(defn area-for-ranges [x-diff ranges]
  (->> ranges
       (map (fn [[lo hi]] (- (inc hi) lo)))
       (map #(* x-diff %))
       (apply +)))

(defn get-area [input]
  (let [
        ;; segments (get-perimeter (parse-input-2 input))
        segments (get-perimeter (parse-input input))
        points (set (mapcat identity segments))
        x-values (sort (distinct (map first points)))
        vertical (filter (fn [[[a _] [b _]]] (= a b)) segments)
        grouped-segments (group-by (fn [[[a _] _]] a) vertical)
        initial-left-y-ranges (sort (get-y-ranges (get grouped-segments 0)))]
    (reduce (fn [acc x]
              (let [left-ranges (:left-y-ranges acc)
                    right-ranges (get-y-ranges (get grouped-segments x))
                    sub-ranges (find-subranges left-ranges right-ranges)
                    {on-regions true off-regions false} (group-by (fn [r]
                                                                    (xor (range-inside-any? r right-ranges)
                                                                         (range-inside-any? r left-ranges)))
                                                                  sub-ranges)
                    x-diff (inc (- x (:prev-x acc)))
                    left-combined (combine-adjacent-ranges left-ranges)
                    area (area-for-ranges x-diff left-combined)
                    _ (println "x" x "prev-x" (:prev-x acc) [(:prev-x acc) x] "x-diff" x-diff
                               "left-ranges" left-ranges "combined" left-combined "area" area)
                    ]
                {:left-y-ranges on-regions
                 :prev-left-x x
                 :prev-x (inc x)
                 :area (+ (:area acc) area)
                 :history (conj (vec (:history acc)) 
                                {:x x
                                 :left-ranges left-ranges
                                 :right-ranges right-ranges
                                 :sub-ranges sub-ranges
                                 :on-regions on-regions
                                 :off-regions off-regions
                                 :area area
                                 })}))
            {:left-y-ranges initial-left-y-ranges :prev-left-x 0 :prev-x 1 :area (area-for-ranges 1 initial-left-y-ranges) :history []}
            (rest x-values))))

;; 1: [0 0]
;; 2: [1 1]
;; 4: [2 4]
;; 6: [5 6]

;; 0: [0 0]
;; 1: [1 1]
;; 2: [2 2]
;; 4: [3 4]
;; 6: [5 6]


;; I'm seconds away from finishing this but my brain is tired.
;; I just have to get through an off-by-one error.
;; (:area (get-area small-input))
