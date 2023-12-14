(ns advent-14.core)

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
;; With the platform tilted toward the north, what is the total load?
(defn get-spaces [width height grid]
  {:column-spaces (let [columns (for [i (range width)] (for [j (range height)] (list [i j] (get grid [i j]))))]
                    (->> columns
                         (map (fn [c] (partition-by #(= \# (second %)) c)))
                         (mapcat #(remove (fn [coll] (= \# (second (first coll)))) %))
                         (map #(map first %))))
   :row-spaces (let [rows (for [j (range height)] (for [i (range width)] (list [i j] (get grid [i j]))))]
                 (->> rows
                      (map (fn [c] (partition-by #(= \# (second %)) c)))
                      (mapcat #(remove (fn [coll] (= \# (second (first coll)))) %))
                      (map #(map first %))))})

(defn spaces-by-direction [spaces]
  {:north (:column-spaces spaces)
   :west (:row-spaces spaces)
   :south (map reverse (:column-spaces spaces))
   :east (map reverse (:row-spaces spaces))})

(defn stone-positions [grid]
  (map first (filter (fn [[k v]] (= \O v)) grid)))

(defn parse-pattern [input]
  (let [vectorized (mapv vec input)
        height (count vectorized)
        width (count (first vectorized))
        grid (into {}
                   (for [j (range height)
                         i (range width)
                         :let [c (get-in vectorized [j i])]]
                     [[i j] c]))]
    {:width width
     :height height
     :grid grid
     :spaces (spaces-by-direction (get-spaces width height grid))
     :empty-grid (reduce (fn [acc p] (assoc acc p \.))
                           grid
                           (stone-positions grid))}))

(defn parse-input [input]
  (->> (read-lines input)
       (parse-pattern)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (get grid [i j]))))))

(def dir-fns {:north (fn [[x y]] [x (dec y)])
              :west (fn [[x y]] [(dec x) y])
              :south (fn [[x y]] [x (inc y)])
              :east (fn [[x y]] [(inc x) y])})

(def pos-fns {:north (fn [{:keys [width height grid] :as pattern}]
                       (for [j (range height) i (range width) :when (= \O (get grid [i j]))] [i j]))
              :west (fn [{:keys [width height grid] :as pattern}]
                      (for [i (range width) j (range height) :when (= \O (get grid [i j]))] [i j]))
              :south (fn [{:keys [width height grid] :as pattern}]
                       (for [j (reverse (range height)) i (range width) :when (= \O (get grid [i j]))] [i j]))
              :east (fn [{:keys [width height grid] :as pattern}]
                      (for [i (reverse (range width)) j (range height) :when (= \O (get grid [i j]))] [i j]))})

(defn tilt [{:keys [width height grid] :as pattern} direction]
  (let [pos-fn (direction pos-fns)
        dir-fn (direction dir-fns)
        positions (pos-fn pattern)]
    (assoc pattern :grid
           (loop [grid grid
                  positions positions]
             (if (empty? positions)
               grid
               (recur (loop [grid grid p (first positions)]
                        (let [next-p (dir-fn p)]
                          (if (not= \. (get grid next-p))
                            grid
                            (recur (assoc (assoc grid p \.) next-p \O) next-p))))
                      (rest positions)))))))

(defn tilt-2 [{:keys [width height grid spaces empty-grid] :as pattern} direction]
  (let [positions (set (stone-positions grid))
        new-positions (mapcat #(take (count (set/intersection (set %) positions)) %) (direction spaces))]
    (assoc pattern :grid
           (reduce (fn [acc p] (assoc acc p \O))
                   empty-grid
                   new-positions))))

(defn compute-load [{:keys [width height grid] :as pattern}]
  (apply +
         (for [j (range height) i (range width) :when (= \O (get grid [i j]))]
           (- height j))))

(defn tilt-north-and-compute-load [input]
  (compute-load (tilt (parse-input input) :north)))


;; (tilt-north-and-compute-load small-input)
;; 136

;; (tilt-north-and-compute-load large-input)
;; 110407



;; Part 2
;; Cycle the platform many times, then compute the load.
(defn cycle [{:keys [width height grid] :as pattern} cycles]
  (loop [pattern pattern
         cycles cycles]
    (if (= cycles 0)
      pattern
      (recur (tilt (tilt (tilt (tilt pattern :north) :west) :south) :east)
             (dec cycles)))))

(defn cycle-and-compute-load [input cycles]
  (compute-load (cycle (parse-input input) cycles)))



(defn cycle-2 [{:keys [width height grid] :as pattern} cycles]
  (loop [pattern pattern
         cycles cycles
         loads []]
    (if (= cycles 0)
      pattern
      (let [new-pattern (tilt-2 (tilt-2 (tilt-2 (tilt-2 pattern :north) :west) :south) :east)]
       (recur new-pattern
              (dec cycles)
              (conj loads (compute-load new-pattern)))))))

(defn cycle-and-compute-load-2 [input cycles]
  (compute-load (cycle-2 (parse-input input) cycles)))


(defn detect-cycle [{:keys [width height grid] :as pattern} max-iterations]
  (loop [patterns [pattern]
         iteration 0]
    ;; (println "iteration" iteration)
    ;; (println (pattern-to-string (last patterns)))
    ;; (newline)
    (if (>= iteration max-iterations)
      {:pattern nil}
      (let [new-pattern (cycle-2 (last patterns) 1)
            matching (first (first (filter #(= new-pattern (second %)) (map-indexed list patterns))))]
        (if matching
          {:pattern new-pattern
           :iteration (inc iteration)
           :previous matching
           :patterns patterns}
          (recur (conj patterns new-pattern)
                 (inc iteration)))))))

(defn cycle-and-compute-load-3 [input cycles]
  (let [{:keys [pattern iteration previous patterns]} (detect-cycle (parse-input input) 1000)]
    (if (nil? pattern)
      :max-cycle-depth-exceeded
      (let [cycle-size (- iteration previous)
            full-cycles (quot (- cycles previous) cycle-size)
            remainder (- cycles (+ previous (* full-cycles cycle-size)))
            last-pattern (get patterns (+ previous remainder))]
        (compute-load last-pattern)))))

;; (cycle-and-compute-load-3 small-input 1000000000)
;; 64

;; (cycle-and-compute-load-3 large-input 1000000000)
;; 87273
