(ns advent-17.core)

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
;; What is the least heat loss a crucible can incur moving across the region?
(defn parse-pattern [input]
  (let [vectorized (mapv vec input)
        height (count vectorized)
        width (count (first vectorized))
        grid (into {}
                   (for [j (range height)
                         i (range width)
                         :let [c (parse-long (str (get-in vectorized [j i])))]]
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
                 (get grid [i j]))))))

(defn move [x y dir amount]
  (case dir
    :right [(+ x amount) y]
    :down [x (+ y amount)]
    :left [(- x amount) y]
    :up [x (- y amount)]))

(defn direction [[ax ay] [bx by]]
  (if (not (or (nil? ax) (nil? ay) (nil? bx) (nil? by)))
    (if (= ax bx)
      (if (> by ay) :down :up)
      (if (> bx ax) :right :left))))

(defn opposite [dir]
  (case dir
    :right :left
    :down :up
    :left :right
    :up :down))

(defn adjacent [[x y :as p]]
  #{[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]})

(defn prev-straight [[x y :as current] came-from dir]
  (if (= current [1 2])
    (println "prev-straight" (came-from current) (came-from (came-from current)) (came-from (came-from (came-from current)))))
  (count (take-while (fn [[to from]] (= dir (direction from to))) (partition 2 1 (iterate #(get came-from %) current)))))

(defn reconstruct-path [came-from end]
  (loop [current end
         path '()]
    (if (some #{current} (keys came-from))
      (recur (get came-from current)
             (conj path current))
      (map :point path))))

(defn in-bounds [points width height]
  (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))) points))

(defn find-neighbors [{:keys [point prevs] :as p} came-from width height]
  (let [[x y] point
        dir (if (first prevs)
              (direction (first prevs) point)
              :right)
        prevs (conj prevs point)
        inaccessible #{(move x y (opposite dir) 1)
                       (if (or (apply = (map first prevs)) (apply = (map second prevs)))
                         (move x y dir 1))}]
    (map (fn [p] {:point p :prevs (take 3 prevs)}) (in-bounds (remove inaccessible (adjacent point)) width height))))

(defn manhattan-distance [[x y :as p] [goalx goaly]]
  (+ (abs (- x goalx)) (abs (- y goaly))))

(defn point-with-lowest-f-score [points f-scores]
  (->> points
       (map #(list (:point %) (get f-scores % Integer/MAX_VALUE) %))
       (sort-by first)
       (sort-by second)
       (first)
       (last)))

(defn find-optimal-path [{:keys [width height grid]} h-fn n-fn prev-count]
  (let [start {:point [0 0] :prevs (repeat prev-count nil)}
        goal [(dec width) (dec height)]]
    (loop [open-set #{start}
           came-from {}
           g-scores {start 0}
           f-scores {start (h-fn (:point start) goal)}
           i 0]
      (if (empty? open-set)
        :failure-no-path
        (let [current (point-with-lowest-f-score open-set f-scores)]
          (if (= (:point current) goal)
            (reconstruct-path came-from current)
            (let [neighbors (n-fn current came-from width height)
                  [new-open-set new-came-from new-g-scores new-f-scores]
                  (reduce (fn [[open-set came-from g-scores f-scores :as acc] n]
                            (let [tentative-g-score (+ (get g-scores current) (get grid (:point n)))]
                              (if (< tentative-g-score (get g-scores n Integer/MAX_VALUE))
                                [(conj open-set n)
                                 (assoc came-from n current)
                                 (assoc g-scores n tentative-g-score)
                                 (assoc f-scores n (+ tentative-g-score (h-fn (:point n) goal)))]
                                acc)))
                          [open-set came-from g-scores f-scores]
                          neighbors)]
              (recur (set (remove #{current} new-open-set))
                     new-came-from
                     new-g-scores
                     new-f-scores
                     (inc i)))))))))

(defn min-path-heat-loss [input]
  (let [{:keys [width height grid] :as pattern} (parse-input input)
        path (find-optimal-path pattern manhattan-distance find-neighbors 2)]
    (apply + (map #(get grid %) path))))

(defn draw-path [input n-fn prev-count]
  (let [{:keys [width height grid] :as pattern} (parse-input input)
        path (find-optimal-path pattern manhattan-distance n-fn prev-count)
        grid-with-path (reduce (fn [acc [[x y] [prev-x prev-y]]]
                                 (assoc acc [x y] (case (direction [prev-x prev-y] [x y])
                                                    :right \>
                                                    :down \v
                                                    :left \<
                                                    :up \^)))
                               grid
                               (map list path (concat [[0 0]] path)))]
    (println (pattern-to-string (assoc pattern :grid grid-with-path)))))

;; (time (min-path-heat-loss small-input))
;; "Elapsed time: 2506.7858 msecs"
;; 102

;; (time (min-path-heat-loss large-input))
;; "Elapsed time: 1.47727566268E7 msecs"
;; 928



;; Part 2
;; Ultra Crucibles!
(defn turns [dir]
  (let [dirs [:right :down :left :up]
        index (.indexOf dirs dir)
        dirs+ (vec (concat [:up] dirs [:right]))]
    [(get dirs+ index) (get dirs+ (+ 2 index))]))

(defn find-neighbors-ultra [{:keys [point prevs] :as p} came-from width height]
  (let [[x y] point
        dir (if (first prevs)
              (direction (first prevs) point)
              :right)
        prev-10 (conj prevs point)
        prev-5 (take 5 prev-10)
        inaccessible (set (concat (list (move x y (opposite dir) 1))
                                  (if (or (apply = (map first prev-10)) (apply = (map second prev-10)))
                                    (list (move x y dir 1)))
                                  (if (not (or (apply = (map first prev-5)) (apply = (map second prev-5))))
                                    (map #(move x y % 1) (turns dir)))))]
    ;; (println "inaccessible" point inaccessible prev-10)
    (map (fn [p] {:point p :prevs (take 10 prev-10)}) (in-bounds (remove inaccessible (adjacent point)) width height))))

(defn min-path-heat-loss-ultra [input]
  (let [{:keys [width height grid] :as pattern} (parse-input input)
        path (find-optimal-path pattern manhattan-distance find-neighbors-ultra 9)]
    ;; (println "path" path)
    (if (= path :failure-no-path)
      path
      (apply + (map #(get grid %) path)))))
