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
  ;; (println "move-to-able" [x y] (get grid [x y]))
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

;; (defn extend [p {:keys [width height grid]}]
;;   (loop [paths (map #(vector p %) (move-to-able p grid))
;;          finished []]
;;     (if (empty? paths)
;;       finished
;;       (let [path (first paths)
;;             nexts (extensions path grid)]
;;         (if (= 1 (count nexts)) (recur (vec (conj (rest paths) (apply conj path nexts))) finished)
;;             (recur (rest paths) (conj finished path)))))))

;; (defn find-segments [{:keys [width height grid] :as pattern}]
;;   (let [{:keys [start end]} (find-endpoints pattern)]
;;     (loop [starts #{start}
;;            segments {}]
;;       ;; (println "find-segments" "starts" starts "segments" segments)
;;       (if (empty? starts)
;;         segments
;;         (let [extendeds (mapcat #(extend % pattern) starts)
;;               formatteds (map (fn [p] {:start (first p) :end (last p) :length (dec (count p))}) extendeds)
;;               grouped (map (fn [[p segs]] [p (into {} (map (fn [[p2 segs2]] [p2 (first segs2)]) (group-by :end segs)))]) (group-by :start formatteds))
;;               new-segments (into segments grouped)
;;               new-starts (remove (set (keys segments)) (set (map :end formatteds)))
;;               ;;_ (println "extendeds" extendeds "new-segments" new-segments "new-starts" new-starts)
;;               ]
;;           (recur new-starts new-segments))))))
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

(defn get-surrounding [[x y] grid]
  (remove #{\#} [(get grid [(inc x) y]) (get grid [x (inc y)]) (get grid [(dec x) y]) (get grid [x (dec y)])]))

(defn ways-in-and-out [[x y :as p] grid]
  (let [at-p (get grid p)
        surrounding [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]
        all-ways-in {[(inc x) y] \< [x (inc y)] \^ [(dec x) y] \> [x (dec y)] \v}
        all-ways-out {[(inc x) y] \> [x (inc y)] \v [(dec x) y] \< [x (dec y)] \^}]
    (reduce (fn [acc sp]
              (let [at-sp (get grid sp)]
                (if slippery
                  (if (some #{at-sp} [\. (get all-ways-out sp) (get all-ways-in sp)]) (conj acc sp) acc)
                  (if (not= at-sp \#) (conj acc sp) acc))))
            []
            surrounding)))

;; (defn at-intersection? [p grid]
;;   (let [at-p (get grid p)
;;         surrounding (get-surrounding p grid)]
;;     (and (= at-p \.)
;;          (or (> (count surrounding) 2)
;;              (some nil? surrounding)))))

(defn at-intersection? [[x y :as p] {:keys [width height grid] :as pattern}]
  (and (not= \# (get grid p))
       (or (or (= x 0) (= x (dec width)) (= y 0) (= y (dec height)))
           (> (count (ways-in-and-out p grid)) 2))))

(defn find-intersections [{:keys [width height grid] :as pattern}]
  (for [x (range width)
        y (range height)
        :when (at-intersection? [x y] pattern)]
    [x y]))

(defn extend [p {:keys [width height grid] :as pattern}]
  (loop [paths (map #(vector p %) (move-to-able p grid))
         finished []]
    (if (empty? paths)
      finished
      (let [path (first paths)
            nexts (remove #{(get path (- (count path) 2))} (move-to-able (last path) grid))]
        (if (and (= 1 (count nexts)) (not (at-intersection? (last path) pattern)))
          (recur (vec (conj (rest paths) (apply conj path nexts))) finished)
          (recur (rest paths) (conj finished path)))))))

(defn find-segments [{:keys [width height grid] :as pattern}]
  (let [{:keys [start end]} (find-endpoints pattern)
        grouped (apply concat
                       (for [intersection (find-intersections pattern)]
                         (let [extendeds (extend intersection pattern)
                               formatteds (map (fn [p] {:start (first p) :end (last p) :length (dec (count p))}) extendeds)
                               grouped (map (fn [[p segs]]
                                              [p (into {} (map (fn [[p2 segs2]]
                                                                 [p2 (first segs2)]) (group-by :end segs)))]) (group-by :start formatteds))]
                           grouped)))]
    (into {} grouped)))

(defn path-to-length [path segments]
  (->> path
       (partition 2 1)
       (map (fn [[start end]] (get (get segments start) end)))
       (map :length)
       (apply +)))

(defn find-paths-with-segments [{:keys [width height grid] :as pattern} start end max-paths]
  (let [segments (find-segments pattern)]
    (println "Found" (count segments) "segments.")
    (loop [open-paths [[start]]
           path-lengths []
           path-segment-counts []]
      ;; (println "open-paths" open-paths "path-lengths" path-lengths)
      (if (or (>= (count path-lengths) max-paths) (empty? open-paths))
        {:path-lengths path-lengths
         :path-segment-counts path-segment-counts}
        (let [current-path (first open-paths)
              current-segments (apply dissoc (get segments (last current-path)) current-path)
              ;; _ (println "current-path" current-path "current-segments" current-segments)
              ]
          (let [extended (map #(conj current-path %) (keys current-segments))
                {finished true unfinished false} (group-by #(= end (last %)) extended)
                ;;_ (println "finished" finished "unfinished" unfinished)
                new-path-lengths (apply conj path-lengths (map #(path-to-length % segments) finished))
                new-path-segment-counts (apply conj path-segment-counts (map count finished))
                ;; _ (if (not= (count path-lengths) (count new-path-lengths)) (println "path-lengths" (sort new-path-lengths)))
                ]
            (recur (doall (concat (rest open-paths) unfinished)) new-path-lengths new-path-segment-counts)))))))

(defn longest-path-length-2 [input max-paths]
  (binding [slippery false]
    (let [pattern (parse-input input)
          {:keys [start end]} (find-endpoints pattern)
          {:keys [path-lengths path-segment-counts]} (find-paths-with-segments pattern start end max-paths)]
      ;; (println "found" (count path-lengths) "paths")
      ;; (println "path-lengths" path-lengths)
      {:path-count (count path-lengths)
       :longest (apply max path-lengths)
       :most-segments (apply max path-segment-counts)})))


;; 2034
;; 2482
;; 3218
;; ---> answer <---

;; other answers
;; 5658
;; 6194

;; (time (longest-path-length-2 large-input 100))
;; "Elapsed time: 2064.9518 msecs"
;; 2034
;; advent-23.core> (time (longest-path-length-2 large-input 500))
;; "Elapsed time: 17056.2807 msecs"
;; 2482
;; advent-23.core> (time (longest-path-length-2 large-input 1000))
;; "Elapsed time: 38162.203101 msecs"
;; 2482
;; advent-23.core> (time (longest-path-length-2 large-input 10000))
;; "Elapsed time: 3480178.673201 msecs"
;; 3218
;; advent-23.core> 

(defn find-segment-points [{:keys [width height grid] :as pattern}]
  (let [{:keys [start end]} (find-endpoints pattern)]
    (->> pattern
         (find-intersections)
         (mapcat #(extend % pattern))
         (map (fn [p] [[(first p) (last p)] p]))
         (into {}))))

(defn find-segments-basic [{:keys [width height grid] :as pattern}]
  (let [{:keys [start end]} (find-endpoints pattern)]
    (->> pattern
         (find-intersections)
         (mapcat #(extend % pattern))
         (map (fn [p] [[(first p) (last p)] (dec (count p))]))
         ;; I use negative distances to find the _longest_ path with bellman-ford.
         ;; (map (fn [[k v]] [k (- v)]))
         (into {}))))

(defn longest-path-bellman-ford [input]
  (binding [slippery false]
    (let [pattern (parse-input input)
          {:keys [start end]} (find-endpoints pattern)
          vertices (find-intersections pattern)
          edges (find-segments-basic pattern)]
      (loop [distances (assoc (into {} (map (fn [v] [v Integer/MAX_VALUE]) vertices)) start 0)
             predecessors (into {} (map (fn [v] [v nil]) vertices))
             i 0]
        (if (= i (dec (count vertices)))
          {:distances distances :predecessors predecessors}
          (let [[new-distances new-predecessors]
                (reduce (fn [[distances predecessors :as acc] [[start end] distance]]
                          (if (< (+ (get distances start) distance) (get distances end))
                            [(assoc distances end (+ (get distances start) distance))
                             (assoc predecessors end start)]
                            acc))
                        [distances predecessors]
                        edges)]
            (recur new-distances new-predecessors (inc i))))))))

(defn path-to-length-2 [path segment-map]
  (->> path
       (partition 2 1)
       (map #(get segment-map (reverse %)))
       (apply +)))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# ~exp
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn find-paths-with-segments-2 [{:keys [width height grid] :as pattern} start end max-paths]
  (let [segment-map (find-segments-basic pattern)
        connections (into {} (map (fn [[k v]] [k (map second v)]) (group-by first (keys segment-map))))]
    (println "Found" (count segment-map) "segments.")
    (loop [
           ;; open-paths [[start]]
           open-paths [(list start)]
           path-lengths []
           ;; path-points #{}
           ;; path-segment-counts []
           longest-path nil
           ]
      (if (or (>= (count path-lengths) max-paths) (empty? open-paths))
        (do
          (println "Remaining open paths:" (count open-paths))
          {:path-lengths path-lengths
           ;; :path-segment-counts path-segment-counts
           ;; :longest-path longest-path
           })
        (let [current-path (first open-paths)
              current-connections (profile "find connections" (get connections (first current-path)))

              ;; nexts (remove (set current-path) current-connections)
              nexts (profile "find nexts"
                             (for [connection current-connections
                                   :when (not (some #{connection} current-path))]
                               connection))

              extended (profile "extend" (map #(conj current-path %) nexts))
              {finished true unfinished false} (profile "group" (group-by #(= end (first %)) extended))
              new-path-lengths (profile "find path lengths" (apply conj path-lengths (map #(path-to-length-2 % segment-map) finished)))
              ;; new-path-segment-counts (apply conj path-segment-counts (map count finished))
              paths-by-length (group-by #(path-to-length-2 % segment-map) (conj finished longest-path))
              new-longest-path (first (val (apply max-key key paths-by-length)))
              old-max (apply max (conj path-lengths 0))
              new-max (apply max (conj new-path-lengths 0))

              ;; new-path-points (if (> (count new-longest-path) (count longest-path))
              ;;                   (if (or (empty? longest-path) (empty? new-longest-path))
              ;;                     (set new-longest-path)
              ;;                     (set/intersection longest-path (set new-longest-path))))
              ;; _ (if (> (count new-longest-path) (count longest-path))
              ;;     (println "new-longest-path" new-longest-path "longest-path" longest-path
              ;;              "intersection" (set/intersection longest-path (set new-longest-path))))
              _ (if (> new-max old-max)
                  (println (str (new java.util.Date (System/currentTimeMillis))) "New max:" new-max
                           "\n" (reverse new-longest-path)))
              ]
          (recur (profile "new open paths" (doall (apply conj (rest open-paths) unfinished)))
                 new-path-lengths
                 ;; new-path-points
                 ;; new-path-segment-counts
                 new-longest-path
                 ))))))

(defn longest-path-length-3 [input slippery-in max-paths]
  (reset! profile-times {})
  (profile "test" (list 1 2 3))
  (binding [slippery slippery-in]
    (println "Slippery:" slippery)
    (let [pattern (parse-input input)
          {:keys [start end]} (find-endpoints pattern)
          {:keys [path-lengths path-segment-counts longest-path]} (find-paths-with-segments-2 pattern start end max-paths)]
      ;; (println "found" (count path-lengths) "paths")
      ;; (println "path-lengths" path-lengths)
      ;; {:path-count (count path-lengths)
      ;;  :most-segments (apply max (conj path-segment-counts 0))
      ;;  :longest-length (apply max (conj path-lengths 0))
      ;;  :longest-path longest-path}
      (println "finished finding paths.")
      (list (apply max (conj path-lengths 0)) (str "Considered " (count path-lengths) " paths.")))))

;; (time (longest-path-length-3 large-input 500))
;; Slippery: true
;; Found 122 segments.
;; "Elapsed time: 19311.1165 msecs"
;; {:path-count 500,
;;  :most-segments 15,
;;  :longest-length 2482,
;;  :longest-path [[1 0] [5 5] [17 41] [9 57] [5 89] [19 105] [37 137] [67 135] [65 99] [87 111] [85 137] [109 133] [133 125] [139 140]]}

;; TODO: longest-path-length-3 gives the wrong answer for part 1. Draw the path to see if it makes any sense. It gives the same
;; answer whether slippery is true or false. Then, is it faster than longest-path-length-2?

(defn write-path-to-pattern [{:keys [width height grid] :as pattern} path]
  (let [segment-map (find-segment-points pattern)
        points (mapcat #(get segment-map %) (partition 2 1 path))]
    (println "Path length" (count points))
    (assoc pattern :grid (reduce (fn [acc p] (if (= \. (get acc p)) (assoc acc p \O) acc)) grid points))))
