(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many tiles are energized?
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
     :grid grid}))

(defn parse-input [input]
  (->> (read-lines input)
       (parse-pattern)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (or (get grid [i j]) \.))))))

(defn move [x y dir]
  (case dir
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]
    :up [x (dec y)]))

(defn next-tiles [{:keys [width height grid]} [x y dir]]
  (let [t (get grid [x y])]
    (filter (fn [[x y dir]] (and (< -1 x width) (< -1 y height)))
            (case t
              \. [(conj (move x y dir) dir)]
              \\ (case dir
                   :right [[x (inc y) :down]]
                   :down [[(inc x) y :right]]
                   :left [[x (dec y) :up]]
                   :up [[(dec x) y :left]])
              \/ (case dir
                   :right [[x (dec y) :up]]
                   :down [[(dec x) y :left]]
                   :left [[x (inc y) :down]]
                   :up [[(inc x) y :right]])
              \| (case dir
                   :right [[x (dec y) :up] [x (inc y) :down]]
                   :down [(conj (move x y dir) dir)]
                   :left [[x (dec y) :up] [x (inc y) :down]]
                   :up [(conj (move x y dir) dir)])
              \- (case dir
                   :right [(conj (move x y dir) dir)]
                   :down [[(dec x) y :left] [(inc x) y :right]]
                   :left [(conj (move x y dir) dir)]
                   :up [[(dec x) y :left] [(inc x) y :right]])))))

(defn propagate-light [{:keys [width height grid] :as pattern} [start-x start-y start-dir]]
  (loop [open-set [[start-x start-y start-dir]]
         beam #{[start-x start-y start-dir]}]
    (if (empty? open-set)
      (into {} (map (fn [[x y d]] [[x y] \#]) beam))
      (recur (remove beam (mapcat (partial next-tiles pattern) open-set))
             (apply conj beam open-set)))))

(defn count-illuminated [input]
  (count (propagate-light (parse-input input) [0 0 :right])))

;; (count-illuminated small-input)
;; 46

;; (count-illuminated large-input)
;; 7496



;; Part 2
;; Bring the light in at other tiles.
(defn entry-tiles [width height]
  (concat (apply concat (for [x (range 0 width)]
                          [[x 0 :down] [x (dec height) :up]]))
          (apply concat (for [y (range 0 height)]
                          [[0 y :right] [(dec width) y :left]]))))

(defn count-illuminated-multi-config [input]
  (let [{:keys [width height grid] :as pattern} (parse-input input)
        entries (entry-tiles width height)]
    (apply max
           (for [entry entries]
             (count (propagate-light pattern entry))))))

;; (count-illuminated-multi-config small-input)
;; 51

;; (count-illuminated-multi-config large-input)
;; 7932
