(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many steps long is the longest hike that doesn't step on any tile more than once?
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
                 (get grid [i j]))))))

(defn find-endpoints [{:keys [width height grid]}]
  {:start (first (for [i (range width)
                       :let [c (get grid [i 0])]
                       :when (= c \.)]
                   [i 0]))
   :end (first (for [i (range width)
                     :let [c (get grid [i (dec height)])]
                     :when (= c \.)]
                 [i (dec height)]))})

(def ^:dynamic slippery true)

(defn move-to-able [[x y] grid]
  (let [unfiltered (if slippery
                     (case (get grid [x y])
                       \> [[(inc x) y]]
                       \v [[x (inc y)]]
                       \< [[(dec x) y]]
                       \^ [[x (dec y)]]
                       \. [(if (some #{\<\.} (list (get grid [(dec x) y]))) [(dec x) y])
                           (if (some #{\^\.} (list (get grid [x (dec y)]))) [x (dec y)])
                           (if (some #{\>\.} (list (get grid [(inc x) y]))) [(inc x) y])
                           (if (some #{\v\.} (list (get grid [x (inc y)]))) [x (inc y)])])
                     [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]])]
    (set
     (for [p unfiltered
           :when (some #{(get grid p)} [\. \> \v \< \^])]
       p))))

(defn extensions [path grid]
  (remove (set path) (move-to-able (last path) grid)))

(defn extend [p {:keys [width height grid]}]
  (loop [paths (map #(vector p %) (move-to-able p grid))
         finished []]
    (if (empty? paths)
      finished
      (let [path (first paths)
            nexts (extensions path grid)]
        (if (= 1 (count nexts)) (recur (vec (conj (rest paths) (apply conj path nexts))) finished)
            (recur (rest paths) (conj finished path)))))))

(defn find-segments [{:keys [width height grid] :as pattern}]
  (let [{:keys [start end]} (find-endpoints pattern)]
    (loop [starts #{start}
           segments {}]
      ;; (println "find-segments" "starts" starts "segments" segments)
      (if (empty? starts)
        segments
        (let [extendeds (mapcat #(extend % pattern) starts)
              formatteds (map (fn [p] {:start (first p) :end (last p) :length (dec (count p))}) extendeds)
              grouped (map (fn [[p segs]] [p (into {} (map (fn [[p2 segs2]] [p2 (first segs2)]) (group-by :end segs)))]) (group-by :start formatteds))
              new-segments (into segments grouped)
              new-starts (remove (set (keys segments)) (set (map :end formatteds)))
              ;;_ (println "extendeds" extendeds "new-segments" new-segments "new-starts" new-starts)
              ]
          (recur new-starts new-segments))))))

(defn find-paths-with-segments [{:keys [width height grid] :as pattern} start end]
  (let [segments (find-segments pattern)
        paths (loop [open-paths [[start]]
                     paths []]
                ;; (println "open-paths" open-paths "paths" paths)
                (if (empty? open-paths)
                  paths
                  (let [current (first open-paths)
                        current-segments (dissoc (get segments (last current)) current)
                        ;; _ (println "current" current "current-segments" current-segments)
                        ]
                    (if (empty? current-segments)
                      (recur (rest open-paths) paths)
                      (let [extended (map #(conj current %) (keys current-segments))
                            {finished true unfinished false} (group-by #(= end (last %)) extended)
                            ;;_ (println "finished" finished "unfinished" unfinished)
                            ]
                        (recur (concat (rest open-paths) unfinished) (apply conj paths finished)))))))]
    (->> paths
         (map #(partition 2 1 %))
         (map #(map (fn [[start end]] (get (get segments start) end)) %))
         (map #(map :length %))
         (map #(apply + %))
         )))

(defn find-paths [{:keys [width height grid]} start end]
  (loop [open-paths [[start]]
         paths []]
    ;; (println "open-paths" open-paths "paths" paths)
    (if (empty? open-paths)
      paths
      (let [current (first open-paths)
            ;; _ (println "current" current "extensions" (extensions current grid))
            moveable (extensions current grid)]
        (if (empty? moveable)
          (recur (rest open-paths) paths)
          (let [extended (map #(conj current %) moveable)
                ;; _ (println "extensions" moveable "extended" extended)
                {finished true unfinished false} (group-by #(= end (last %)) extended)]
            (recur (concat (rest open-paths) unfinished) (apply conj paths finished))))))))

(defn longest-path-length [input]
  (let [pattern (parse-input input)
        {:keys [start end]} (find-endpoints pattern)]
    (apply max (map dec (map count (find-paths pattern start end))))))


;; (time (longest-path-length small-input))
;; "Elapsed time: 9.6158 msecs"
;; 94

;; (time (longest-path-length large-input))
;; "Elapsed time: 58536.488 msecs"
;; 2034



;; Part 2
;; Ignore slippery slopes.
(defn longest-path-length-2 [input]
  (binding [slippery false]
    (let [pattern (parse-input input)
          {:keys [start end]} (find-endpoints pattern)]
      (apply max (map dec (map count (find-paths pattern start end)))))))


;; TODO: I'm missing two paths. I think I need to compute all segments by
;; finding every intersection and all the segments from each one.

;; (let [pattern (parse-input small-input)
;;                       {:keys [start end]} (find-endpoints pattern)]
;;                   (find-paths-with-segments pattern start end))
;; (90 82 82 94)
