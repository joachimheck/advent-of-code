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
  (println "get-y-ranges" segments)
  (map (fn [[[_ a] [_ b]]] (vec (sort [a b]))) segments))

(defn connect-ranges [left-ranges right-ranges]
  (println "connect-ranges" left-ranges right-ranges)
  (if (empty? left-ranges)
    ;;{:overlap right-ranges}
    right-ranges
    (let [inside (for [[r-min r-max :as r] right-ranges
                       [l-min l-max :as l] left-ranges
                       :when (< l-min r-min r-max l-max)]
                   [l r])
          outside (for [[r-min r-max :as r] right-ranges
                        [l-min l-max :as l] left-ranges
                        :when (< r-min l-min l-max r-max)]
                    [l r])
          overlap (for [[r-min r-max :as r] right-ranges
                        [l-min l-max :as l] left-ranges
                        :when (or (= r-min l-max) (= r-max l-min))]
                    [l r])]
      ;; {:inside inside :outside outside :overlap overlap}
      (println "inside" inside "overlap" overlap)
      (vec (apply concat inside) (apply concat overlap)))))

;; (apply connect-ranges (map get-y-ranges (first (find-segment-joins (get-perimeter (parse-input-2 small-input))))))

;; [0 10] [10 15] => [0 15]
;; [0 10] [5 10] => [5 10]
;; [0 10] [-5 0] => [-5 10]
;; [0 10] [0 5] => [0 5]


(defn combine-connected-ranges [[[a b] [c d]]]
  (cond (= b c) [a d]
        (= b d) [c d]
        (= a d) [c b]
        (= a c) [c d]))

(defn split-inside-ranges [[[a b] [c d]]]
  (list [a c] [c d] [d b]))



;; (let [segments (get-perimeter (parse-input-2 small-input))
;;                       points (set (mapcat identity segments))
;;                       x-values (sort (distinct (map first points)))
;;                       vertical (filter (fn [[[a _] [b _]]] (= a b)) segments)
;;                       grouped-segments (group-by (fn [[[a _] _]] a) vertical)]
;;                   (reduce (fn [acc x]
;;                             (let [right-segments (get grouped-segments x)
;;                                   connected-ranges (connect-ranges (:left-y-ranges acc) (get-y-ranges right-segments))
;;                                   _ (println "connected-ranges" connected-ranges)]
;;                               {:left-y-ranges connected-ranges
;;                                :history (conj (:history acc) 
;;                                               {:x x
;;                                                :connected (map combine-connected-ranges (:overlap connected-ranges))
;;                                                :inside (map split-inside-ranges (:inside connected-ranges))})}))
;;                           {:left-y-ranges [] :history []}
;;                           x-values))
