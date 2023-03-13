(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 22: Mode Maze

;; Part 1
;; What is the total risk level for the smallest rectangle that includes 0,0 and the target's coordinates?
(def g-indices (atom (transient {})))
(def levels (atom (transient {})))

(defn parse-input [f]
  (let [[line1 line2] (read-lines f)]
    (reset! g-indices (transient {}))
    (reset! levels (transient {}))
    {:depth (parse-long (second (re-find #"(\d+)" line1)))
     :target (mapv parse-long (rest (re-find #"(\d+),(\d+)" line2)))}))

;; To allow the circular reference between the two functions.
(declare geologic-index erosion-level)

(defn geologic-index [[x y :as pos] depth target]
  (cond (or (= pos [0 0]) (= pos target))
        0
        (= y 0)
        (* x 16807)
        (= x 0)
        (* y 48271)
        :else
        (if-let [index (get @g-indices pos)]
          index
          (do
            (reset! g-indices (assoc! @g-indices pos (* (erosion-level [(dec x) y] depth target) (erosion-level [x (dec y)] depth target))))
            (get @g-indices pos)))))

(defn erosion-level [pos depth target]
  (if-let [level (get @levels pos)]
    level
    (do
      (reset! levels (assoc! @levels pos (mod (+ (geologic-index pos depth target) depth) 20183)))
      (get @levels pos))))

(defn terrain-type [[x y :as pos] depth target]
  (mod (erosion-level pos depth target) 3))

(defn draw-cave [{:keys [depth target] :as input}]
  (let [[[minx miny] [maxx maxy]] [[0 0] target]]
    (str/join "\n"
              (for [j (range miny (inc maxy))]
                (str/join
                 (for [i (range minx (inc maxx))]
                   (case (terrain-type [i j] depth target)
                     0 "."
                     1 "="
                     2 "|")))))))


(deftest test-process-map
  (is (=
       (str/join "\n"
                 '(".=.|=.|.|=."
                   ".|=|=|||..|"
                   ".==|....||="
                   "=.|....|.=="
                   "=|..==...=."
                   "=||.=.=||=|"
                   "|.=.===|||."
                   "|..==||=.|="
                   ".=..===..=|"
                   ".======|||="
                   ".===|=|===."))
       (draw-cave (parse-input small-input)))))

(defn risk-level [{:keys [depth target] :as input}]
  (let [[x y] target]
    (apply + (for [i (range (inc x))
                   j (range (inc y))]
               (terrain-type [i j] depth target)))))

;; (risk-level (parse-input small-input))
;; 114

;; (risk-level (parse-input large-input))
;; 4479



;; Part 2
;; What is the fewest number of minutes you can take to reach the target?
(defn manhattan-distance [[[x1 y1] _ :as a] [[x2 y2] _ :as b]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn manhattan-8x [[[x1 y1] _ :as a] [[x2 y2] _ :as b]]
  (* 8 (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(defn manhattan-with-min [[[x1 y1] _ :as a] [[x2 y2] _ :as b]]
  (max 1 (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(defn reconstruct-path [froms start goal]
  (loop [path (list {:from goal :cost 0})
         cost 0]
    (let [head (:from (first path))]
      (if (= head start)
        {;; :path path
         :cost cost}
        (let [{next-node :from to :to next-cost :cost :as combined} (get froms head)]
          (recur (conj path combined) (+ cost next-cost)))))))

(defn get-adjacent [[x y]]
  (filter (fn [[x y]] (and (>= x 0) (>= y 0)))
          (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])))

(def valid-equipment {0 #{:gear :torch} 1 #{:gear :neither} 2 #{:torch :neither}})

(defn get-move-costs [[pos equipment] depth target]
  (let [terrain (terrain-type pos depth target)
        adjacent (get-adjacent pos)]
    (into {}
          (concat
           [[[pos (first (set/difference (get valid-equipment terrain) #{equipment}))] 7]]
           (for [a-pos adjacent
                 :let [adjacent-terrain (terrain-type a-pos depth target)
                       valid-adjacent (get valid-equipment adjacent-terrain)]
                 :when (some #{equipment} valid-adjacent)]
             [[a-pos equipment] 1])))))

(defn process-node [current open-set f-scores g-scores froms goal h-fn depth target]
  (let [open-set (disj open-set current)
        move-costs (get-move-costs current depth target)
        neighbors (keys move-costs)]
    (loop [neighbors neighbors
           open-set open-set
           f-scores f-scores
           g-scores g-scores
           froms froms]
      (if (empty? neighbors)
        [open-set f-scores g-scores froms]
        (let [neighbor (first neighbors)
              tentative-g-score (+ (get g-scores current) (get move-costs neighbor))]
          ;; (if (or (< (apply max (first neighbor)) 2) (< (apply max (first current)) 2))
          ;;   (println "For" "current" current "neighbor" neighbor
          ;;            "tentative-g-score" (get g-scores current) "+" (get move-costs neighbor) "=" tentative-g-score
          ;;            "previous g-score" (get g-scores neighbor)
          ;;            "update?" (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))))
          (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
            (recur (rest neighbors)
                   (conj open-set neighbor)
                   (assoc! f-scores neighbor (+ tentative-g-score (h-fn neighbor goal)))
                   (assoc! g-scores neighbor tentative-g-score)
                   (assoc! froms neighbor {:from current :to neighbor :cost (get move-costs neighbor) :f-score (+ tentative-g-score (h-fn neighbor goal))}))
            (recur (rest neighbors) open-set f-scores g-scores froms)))))))

(defn find-path [{:keys [depth target] :as input} h-fn]
  (let [start [[0 0] :torch]
        goal [target :torch]]
    (loop [open-set #{start}
           f-scores (transient {start (h-fn start goal)})
           g-scores (transient {start 0})
           froms (transient {})
           i 0
           maxx 0 maxy 0]
      (if (empty? open-set)
        :error-no-open-nodes
        (let [[[x y] _ :as current] (first (sort-by #(get f-scores %) open-set))
              maxx (max maxx x)
              maxy (max maxy y)]
          ;; (if (= 0 (mod i 1000))
          ;;   (do
          ;;     (printf "Iteration %d: max coordinates [%3d,%3d] open-set size %d\n" i maxx maxy (count open-set))
          ;;     (flush)))
          (if (= current goal)
            (reconstruct-path (persistent! froms) start current)
            (let [[open-set f-scores g-scores froms] (process-node current open-set f-scores g-scores froms goal h-fn depth target)]
              (recur open-set f-scores g-scores froms (inc i) maxx maxy))))))))

;; (time (find-path (parse-input small-input) manhattan-distance))
;; "Elapsed time: 21.6376 msecs"
;; {:cost 45}

;; (time (find-path (parse-input large-input) manhattan-distance))
;; "Elapsed time: 337723.6452 msecs"
;; {:cost 1032}
