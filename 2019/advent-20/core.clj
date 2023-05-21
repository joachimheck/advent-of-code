(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 20: Donut Maze

;; Part 1
;; How many steps from AA to ZZ?
(def small-input-2 "small-input-2.txt")

(def letters #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z})

(defn outside? [[x y] width height]
  (or (= x 2) (= y 2) (= x (- width 3)) (= y (- height 3))))

(defn find-label-locations [grid width height]
  ;; (apply concat (vals (group-by #(inside? % 10 10) '([2 5] [3 7] [4 3] [8 4]))))
  (apply merge-with (fn [a b] (if (outside? a width height) [a b] [b a]))
         (remove nil?
                 (for [i (range width)
                       j (range height)]
                   (let [middle (get grid [i j])
                         ;; left (get grid [(dec i) j])
                         right (get grid [(inc i) j])
                         ;; up (get grid [i (dec j)])
                         down (get grid [i (inc j)])]
                     (cond (and (some #{middle} letters)
                                (some #{right} letters)
                                (= \. (get grid [(+ i 2) j])))
                           {(str middle right) [(+ i 2) j]}
                           (and (some #{middle} letters)
                                (some #{right} letters)
                                (= \. (get grid [(dec i) j])))
                           {(str middle right) [(dec i) j]}
                           (and (some #{middle} letters)
                                (some #{down} letters)
                                (= \. (get grid [i (+ j 2)])))
                           {(str middle down) [i (+ j 2)]}
                           (and (some #{middle} letters)
                                (some #{down} letters)
                                (= \. (get grid [i (dec j)])))
                           {(str middle down) [i (dec j)]}))))))

(defn parse-input [f]
  (let [vec-input (->> f
                       (read-lines)
                       (mapv vec))
        width (count (first vec-input))
        height (count vec-input)
        grid (into {} (for [i (range width)
                            j (range height)
                            :when (not= (get-in vec-input [j i]) \space)]
                        [[i j] (get-in vec-input [j i])]))
        label-locations (find-label-locations grid width height)]
    {:grid grid
     :width width
     :height height
     :start (get label-locations "AA")
     :end (get label-locations "ZZ")
     :label-locations (dissoc label-locations "AA" "ZZ")}))

(defn simple-adjacent [[x y]]
  (list [x (dec y)]
        [(inc x) y]
        [x (inc y)]
        [(dec x) y]))

(defn tele-adjacent [p label-locations]
  (remove nil? (map (fn [[k v]] (if (some #{p} v) (first (remove #{p} v)))) label-locations)))

(defn adjacent [[x y :as p] {:keys [grid label-locations] :as maze}]
  (filter (fn [p] (= \. (get grid p)))
          (concat (simple-adjacent p) (tele-adjacent p label-locations))))

(defn solve-maze [maze]
  (loop [open-paths (list [(:start maze)])
         visited #{}
         complete-paths []]
    (if (empty? open-paths)
      complete-paths
      (let [current (first open-paths)
            p (last current)
            new-visited (conj visited p)
            adjacent-points (remove visited (adjacent p maze))
            extended-paths (map (fn [p] (conj current p)) adjacent-points)
            new-paths (concat (rest open-paths) extended-paths)
            new-complete (if (= p (:end maze)) (conj complete-paths current) complete-paths)]
        (recur new-paths new-visited new-complete)))))

(defn shortest-path [maze]
  (let [shortest (first (sort-by count (solve-maze maze)))]
    (dec (count shortest))))

;; (time (shortest-path (parse-input small-input)))
;; "Elapsed time: 6.7201 msecs"
;; 23
;; (time (shortest-path (parse-input small-input-2)))
;; "Elapsed time: 28.989 msecs"
;; 58
;; (time (shortest-path (parse-input large-input)))
;; "Elapsed time: 376.0124 msecs"
;; 522



;; Part 2
;; Recursive spaces.
(defn simple-adjacent-recursive [[x y z]]
  (list [x (dec y) z]
        [(inc x) y z]
        [x (inc y) z]
        [(dec x) y z]))

(defn tele-adjacent-recursive [[x y z :as p] label-locations]
  (remove nil? (map (fn [[label [outside inside]]] (cond (and (= outside [x y]) (> z 0)) (conj inside (dec z))
                                                         (= inside [x y]) (conj outside (inc z))))
                    label-locations)))

(defn adjacent-recursive [[x y z :as p] {:keys [grid label-locations start end] :as maze}]
  (remove (fn [[i j k]] (and (> k 0) (or (= [i j] start) (= [i j] end))))
          (filter (fn [[i j k]] (= \. (get grid [i j])))
                  (concat (simple-adjacent-recursive p) (tele-adjacent-recursive p label-locations)))))

(defn build-adjacency-map [{:keys [grid label-locations start end] :as maze}]
  (let [spaces (filter #(= (get grid %) \.)  (keys grid))]
    (into {}
     (for [[x y :as space] spaces]
       (let [simple-adjacent (simple-adjacent-recursive [x y 1])
             tele-adjacent (tele-adjacent-recursive [x y 1] label-locations)
             open (filter (fn [[i j k]] (= \. (get grid [i j]))) (concat simple-adjacent tele-adjacent))]
         [space (map (fn [[i j k]] [i j (case k
                                          0 dec
                                          1 identity
                                          2 inc)])
                     open)])))))

(defn is-label? [p1 p2 p3 grid]
  (and (some #{(get grid p1)} letters)
       (some #{(get grid p2)} letters)
       (= \. (get grid p3))))

(defn full-label-name [p1 p2 p3 grid width height]
  (str/join (list (str (get grid p1))
                  (str (get grid p2))
                  (if (outside? p3 width height)
                    "-out"
                    "-in"))))

(defn find-label-locations-2 [grid width height]
  (apply merge
         (remove nil?
                 (for [i (range width)
                       j (range height)]
                   (let [middle [i j]
                         right [(inc i) j]
                         down [i (inc j)]]
                     (cond (is-label? middle right [(+ i 2) j] grid)
                           {(full-label-name middle right [(+ i 2) j] grid width height) [(+ i 2) j]}
                           (is-label? middle right [(dec i) j] grid)
                           {(full-label-name middle right [(dec i) j] grid width height) [(dec i) j]}
                           (is-label? middle down [i (+ j 2)] grid)
                           {(full-label-name middle down [i (+ j 2)] grid width height) [i (+ j 2)]}
                           (is-label? middle down [i (dec j)] grid)
                           {(full-label-name middle down [i (dec j)] grid width height) [i (dec j)]}))))))

(defn parse-input-2 [f]
  (let [vec-input (->> f
                       (read-lines)
                       (mapv vec))
        width (count (first vec-input))
        height (count vec-input)
        grid (into {} (for [i (range width)
                            j (range height)
                            :when (not= (get-in vec-input [j i]) \space)]
                        [[i j] (get-in vec-input [j i])]))
        label-locations (find-label-locations-2 grid width height)
        maze {:grid grid
              :width width
              :height height
              :start (get label-locations "AA")
              :end (get label-locations "ZZ")
              :label-locations label-locations}]
    (assoc maze :adjacency-map (build-adjacency-map maze))))

(defn adjacent-from-map [[x y z] adjacency-map]
  (for [[i j k-fn :as p] (get adjacency-map [x y])
        :when (>= (k-fn z) 0)]
    [i j (k-fn z)]))

(defn build-new-paths [open-paths current adjacent-points current-length shortest-length]
  (let [paths (if (>= current-length shortest-length)
                (rest open-paths)
                (let [extended-paths (map (fn [p] (conj current p)) adjacent-points)]
                  (concat (rest open-paths) (remove #(>= (count %) shortest-length) extended-paths))))]

    ;; (->> paths
    ;;      ;; (sort-by #(last (last %)))
    ;;      ;; (sort-by count)
    ;;      (sort-by #(+ (* 10000 (count %)) (last (last %))))
    ;;      )
    paths
    ))

(defn available-adjacent [p visited maze]
  (remove visited (adjacent-from-map p (:adjacency-map maze))))

(defn add-to-visited [visited p]
  (conj visited p))

(defn compute-max-depth [points max-depth]
  (if (empty? points)
    max-depth
    (max max-depth (apply max (map last points)))))

(defn get-first-path [paths]
  (first paths))

(defn get-path-length [path]
  (count path))

(defn get-last-point [path]
  ;; (last path)
  (get path (dec (count path))))

(defn solve-maze-recursive [maze]
  (let [start (conj (:start maze) 0)
        end (conj (:end maze) 0)]
    (loop [open-paths [[start]]
           visited #{}
           shortest-path nil
           shortest-length Integer/MAX_VALUE
           max-depth 0]
      ;; (println "open-paths" open-paths)
      (if (empty? open-paths)
        (if shortest-path
          {:path shortest-path
           :steps (dec shortest-length)
           :depth (apply max (map last shortest-path))
           :max-depth max-depth}
          {:steps :no-shortest-path})
        (let [
              ;; current (first open-paths)
              current (get-first-path open-paths)
              ;; current-length (count current)
              current-length (get-path-length current)
              ;; p (last current)
              p (get-last-point current)
              ;; new-visited (conj visited p)
              new-visited (add-to-visited visited p)
              ;; adjacent-points (remove visited (adjacent-from-map p (:adjacency-map maze)))
              adjacent-points (available-adjacent p visited maze)
              new-paths (build-new-paths open-paths current adjacent-points current-length shortest-length)
              [new-shortest new-shortest-length] (if (and (= p end)
                                                          (< current-length shortest-length))
                                                   [current current-length]
                                                   [shortest-path shortest-length])
              ;; new-max-depth (if (empty? adjacent-points)
              ;;                 max-depth
              ;;                 (max max-depth (apply max (map last adjacent-points))))
              ;; new-max-depth (compute-max-depth adjacent-points max-depth)
              new-max-depth 0
              ]
          (if (not= shortest-path new-shortest) (println "Found new shortest path with steps:" (dec new-shortest-length)))
          ;; (if (not= max-depth new-max-depth) (println "Hit new max depth:" new-max-depth))
          (recur
           new-paths
           new-visited
           new-shortest
           new-shortest-length
           new-max-depth))))))

(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")

;; (time (let [solution (solve-maze-recursive (parse-input-2 large-input))]
;;                         (dissoc solution :path)))
;; Execution error (StackOverflowError) at (REPL:1).

(defn adjacent-spaces [[x y :as p] {:keys [grid label-locations] :as maze}]
  ;; (println "adjacent-spaces" [x y] (filter (fn [p] (= \. (get grid p))) (simple-adjacent p)))
  (filter (fn [p] (= \. (get grid p))) (simple-adjacent p)))

(defn reachable-labels [start-label {:keys [label-locations] :as maze}]
  (let [location-labels (set/map-invert label-locations)]
    (loop [open-set (list (get label-locations start-label))
           visited #{}
           labels {}
           steps 0]
      ;; (println "reachable-labels" "open-set" open-set)
      (if (empty? open-set)
        labels
        (let [new-open-set (remove visited (apply concat (map #(adjacent-spaces % maze) open-set)))
              new-visited (apply conj visited open-set)
              new-labels (merge labels (into {} (for [p open-set
                                                      :let [label (get location-labels p)]
                                                      :when (and label (> steps 0))]
                                                  [label steps])))]
          ;; (println "new-open-set" new-open-set)
          (recur new-open-set new-visited new-labels (inc steps)))))))

(defn find-paths [{:keys [grid label-locations start end width height] :as maze}]
  (loop [open-set (keys label-locations)
         paths {}]
    (if (empty? open-set)
      paths
      (let [current (first open-set)]
        (recur (rest open-set) (assoc paths current (reachable-labels current maze)))))))

(defn get-portal-point [point level]
  (let [[_ label suffix] (re-matches #"([A-Z][A-Z])(-in|-out)" point)]
   (cond (or (= label "AA") (= label "ZZ"))
         nil
         (= suffix "-in")
         [(str/join (list label "-out")) (inc level)]
         (and (= suffix "-out") (> level 0))
         [(str/join (list label "-in")) (dec level)])))

(defn find-reachable [path visited paths]
  (let [[point level] (last path)
        available-paths (get paths point)
        available-points (keys available-paths)
        local (if (> level 0)
                (remove #{"AA-in" "AA-out" "ZZ-in" "ZZ-out"} available-points)
                available-points)
        with-level (map (fn [p] [p level]) local)
        portal-point (get-portal-point point level)
        with-portal (if portal-point (conj with-level portal-point) with-level)
        without-visited (remove visited with-portal)]
    (map (fn [p] [p (get available-paths (first p) 1)]) without-visited)))

(defn compare-paths [{path1 :path length1 :length} {path2 :path length2 :length}]
  (let [comp (compare length1 length2)]
    (if (not= comp 0) comp (compare path1 path2))))

(defn solve-maze-paths [{:keys [grid label-locations start end width height] :as maze}]
  (let [maze-paths (find-paths maze)
        start [(first (filter #(str/starts-with? % "AA") (keys maze-paths))) 0]
        end [(first (filter #(str/starts-with? % "ZZ") (keys maze-paths))) 0]]
    (loop [;; open-paths [{:path [start] :length 0}]
           ;; open-paths (sorted-set-by #(compare (:length %1) (:length %2)) {:path [start] :length 0})
           open-paths (sorted-set-by compare-paths {:path [start] :length 0})
           shortest-path nil
           shortest-length Integer/MAX_VALUE
           longest-reported 0
           visited #{}]
      (if (empty? open-paths)
        shortest-path
        (let [{:keys [path length] :as current} (first open-paths)]
          (if (> length shortest-length)
            (recur (disj open-paths current) shortest-path shortest-length longest-reported visited)
            (let [[last-point level] (last path)
                  new-shortest? (and (= end (last path)) (< length shortest-length))
                  _ (if new-shortest? (println "new shortest path with length" length))
                  previous-point (first (last (drop-last current)))
                  reachable (find-reachable path visited maze-paths)
                  extended-paths (map (fn [[p steps]] {:path (conj path p) :length (+ steps length)}) reachable)]
              (recur (doall (apply conj (disj open-paths current) extended-paths))
                     (if new-shortest? current shortest-path)
                     (if new-shortest? length shortest-length)
                     (if (> length longest-reported) (+ longest-reported 10) longest-reported)
                     (apply conj visited (map first reachable))))))))))

;; (time (dissoc (solve-maze-paths (parse-input-2 small-input)) :path))
;; new shortest path with length 26
;; "Elapsed time: 8.1338 msecs"
;; {:length 26}
;; (time (dissoc (solve-maze-paths (parse-input-2 small-input-3)) :path))
;; new shortest path with length 396
;; "Elapsed time: 41.0859 msecs"
;; {:length 396}
;; (time (dissoc (solve-maze-paths (parse-input-2 large-input)) :path))
;; new shortest path with length 6300
;; "Elapsed time: 423.443 msecs"
;; {:length 6300}
