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
        label-locations (find-label-locations grid width height)]
    {:grid grid
     :width width
     :height height
     :start (get label-locations "AA")
     :end (get label-locations "ZZ")
     :label-locations (dissoc label-locations "AA" "ZZ")}))

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

(defn solve-maze-recursive [maze]
  (let [start (conj (:start maze) 0)
        end (conj (:end maze) 0)]
    (loop [open-paths (list [start])
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
              current (first open-paths)
              current-length (count current)
              p (last current)
              adjacent-points (remove #{(get current (- current-length 2))} (adjacent-recursive p maze))
              extended-paths (map (fn [p] (conj current p)) adjacent-points)
              new-paths (concat (rest open-paths) (remove #(>= (count %) shortest-length) extended-paths))
              [new-shortest new-shortest-length] (if (and (= p end)
                                                          (< current-length shortest-length))
                                                   [current current-length]
                                                   [shortest-path shortest-length])
              new-max-depth (if (empty? adjacent-points) max-depth (max max-depth (apply max (map last adjacent-points))))]
          (if (not= shortest-path new-shortest) (println "Found new shortest path with steps:" (dec new-shortest-length)))
          ;; (if (not= max-depth new-max-depth) (println "Hit new max depth:" new-max-depth))
          (recur
           new-paths
           ;; (->> new-paths
           ;;      (sort-by count)
           ;;      (sort-by #(last (last %)))
           ;;      )
           new-shortest
           new-shortest-length
           new-max-depth))))))

(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")

;; (time (let [solution (solve-maze-recursive (parse-input-2 large-input))]
;;                         (dissoc solution :path)))
;; Execution error (StackOverflowError) at (REPL:1).

