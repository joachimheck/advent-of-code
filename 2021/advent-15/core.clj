(ns advent-15.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Chiton

;; Part 1
;; Find the lowest-risk path through the room.
(defn parse-input [f]
  (->> (read-lines f)
       (mapv (fn [v] (mapv (fn [c] (parse-long (str c))) v)))))

(defn get-point [grid [x y]]
  (get-in grid [y x]))

(defn manhattan-distance [[x y] [a b]]
  (+ (abs (- b y)) (abs (- a x))))

(defn neighbors [[x y] grid]
  (let [width (count (first grid))
        height (count grid)]
   (remove (fn [[x y]] (and (or (< x 0) (< y 0) (>= x width) (>= y height))))
           (list [x (dec y)]
                 [(inc x) y]
                 [x (inc y)]
                 [(dec x) y]))))

(defn find-path-in-froms [froms start goal]
  (loop [path (list goal)]
    (let [head (first path)]
      (if (= head start)
        path
        (recur (conj path (get froms head)))))))

(defn find-path [grid]
  (let [width (count (first grid))
        height (count grid)
        start [0 0]
        goal [(dec width) (dec height)]
        max-iterations 1000]
    (loop [open-set #{start}
           froms {}
           f-scores {start (manhattan-distance start goal)}
           g-scores {start 0}
           iterations 0]
      (cond (empty? open-set)
            :failure-no-open-nodes
            ;; (= iterations max-iterations)
            ;; (list :fail-over-time iterations)
            :else
            (let [current (first (first (sort-by second (map #(list % (get f-scores %)) open-set))))]
              ;; (println "picking" current "from open-set" (sort-by second (map #(list % (get f-scores %)) open-set)))
              (if (= current goal)
                (find-path-in-froms froms start goal)
                (let [open-set (disj open-set current)
                      neighbors (neighbors current grid)]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                        (reduce (fn [result neighbor]
                                  (let [tentative-g-score (+ (get g-scores current) (get-point grid neighbor))]
                                    (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                      (-> result
                                          (assoc-in [:froms neighbor] current)
                                          (assoc-in [:g-scores neighbor] tentative-g-score)
                                          (assoc-in [:f-scores neighbor] (+ tentative-g-score (manhattan-distance neighbor goal)))
                                          (update :open-set (fn [s] (conj s neighbor))))
                                      result)))
                                {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                                neighbors)]
                    (recur new-open-set new-froms new-f-scores new-g-scores (inc iterations))))))))))

(defn compute-risk [f]
  (let [grid (parse-input f)
        path (find-path grid)]
    (->> path
         rest ;; Don't count the starting position's risk.
         (map (fn [pos] (get-point grid pos)))
         (apply +))))

;; (time (compute-risk small-input))
;; "Elapsed time: 5.4497 msecs"
;; 40

;; (time (compute-risk large-input))
;; "Elapsed time: 4491.1654 msecs"
;; 472



;; Part 2
;; Expand the map four times in each direction (into a 5x5 grid), increasing all values by one (mod 10) with each step.
(defn inc-mod10 [x]
  (if (< x 9)
    (inc x)
    1))

(defn inc-line [l]
  (mapv inc-mod10 l))

(defn widen-line [line]
  (vec (apply concat (take 5 (iterate inc-line line)))))

(defn expand-grid [grid]
 (let [widened-grid (mapv widen-line grid)
       deepened-lines (map (fn [line] (take 5 (iterate inc-line line))) widened-grid)]
   (vec (apply concat (apply map vector deepened-lines)))))

(defn compute-risk-expanded [f]
  (let [grid (parse-input f)
        expanded-grid (expand-grid grid)
        path (find-path expanded-grid)]
    (->> path
         rest ;; Don't count the starting position's risk.
         (map (fn [pos] (get-point expanded-grid pos)))
         (apply +))))


;; (time (compute-risk-expanded small-input))
;; "Elapsed time: 346.6713 msecs"
;; 315

;; (time (compute-risk-expanded large-input))
;; "Elapsed time: 423371.5081 msecs"
;; 2851


;; It works, but it's slow. Can I speed it up with transient collections?
(defn find-path-transient [grid]
  (let [width (count (first grid))
        height (count grid)
        start [0 0]
        goal [(dec width) (dec height)]]
    (loop [open-set #{start}
           froms (transient {})
           f-scores (transient {start (manhattan-distance start goal)})
           g-scores (transient {start 0})]
      (cond (zero? (count open-set))
            :failure-no-open-nodes
            :else
            (let [current (first (first (sort-by second (map #(list % (get f-scores %)) open-set))))]
              ;; (println "picking" current "from open-set" (sort-by second (map #(list % (get f-scores %)) open-set)))
              (if (= current goal)
                (find-path-in-froms (persistent! froms) start goal)
                (let [open-set (disj open-set current)
                      neighbors (neighbors current grid)]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores}
                        (loop [neighbors neighbors
                               froms froms
                               f-scores f-scores
                               g-scores g-scores
                               open-set open-set]
                          (if (empty? neighbors)
                            {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                            (let [neighbor (first neighbors)
                                  tentative-g-score (+ (get g-scores current) (get-point grid neighbor))]
                              ;;(println "g-score" [1 1] (get g-scores [1 1]))
                              ;; (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                              ;;   (println "updating g-score " current "->" neighbor "from" (get g-scores neighbor) "to" tentative-g-score))
                              (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                (recur
                                 (remove #{neighbor} neighbors)
                                 (assoc! froms neighbor current)
                                 (assoc! f-scores neighbor (+ tentative-g-score (manhattan-distance neighbor goal)))
                                 (assoc! g-scores neighbor tentative-g-score)
                                 (conj open-set neighbor))
                                (recur (remove #{neighbor} neighbors) froms f-scores g-scores open-set)))))]
                    (recur new-open-set new-froms new-f-scores new-g-scores)))))))))

(defn compute-risk-expanded-transient [f]
  (let [grid (parse-input f)
        expanded-grid (expand-grid grid)
        path (find-path-transient expanded-grid)]
    ;; (println "path" path)
    (if (not (seq? path))
      path
      (->> path
           rest ;; Don't count the starting position's risk.
           (map (fn [pos] (get-point expanded-grid pos)))
           (apply +)
           ))))

;; This is faster, but not much.
;; (time (compute-risk-expanded-transient small-input))
;; "Elapsed time: 265.2823 msecs"
;; 315

;; (time (compute-risk-expanded-transient large-input))
;; "Elapsed time: 349066.9539 msecs"
;; 2851


;; What about using a sorted set for open-set?
(defn compare-seconds [x y]
  (let [c (compare (second x) (second y))]
    (if (not= c 0)
      c
      (compare (first x) (first y)))))

(defn find-path-transient-sorted [grid]
  (let [width (count (first grid))
        height (count grid)
        start [0 0]
        goal [(dec width) (dec height)]]
    (loop [open-set (sorted-set-by compare-seconds (list start 0))
           froms (transient {})
           f-scores (transient {start (manhattan-distance start goal)})
           g-scores (transient {start 0})]
      (cond (zero? (count open-set))
            :failure-no-open-nodes
            :else
            (let [[current _ :as current-pair] (first open-set)]
              (if (= current goal)
                (find-path-in-froms (persistent! froms) start goal)
                (let [open-set (disj open-set current-pair)
                      neighbors (neighbors current grid)]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores}
                        (loop [neighbors neighbors
                               froms froms
                               f-scores f-scores
                               g-scores g-scores
                               open-set open-set]
                          (if (> (count open-set) 20) :too-big-open-set)
                          (if (empty? neighbors)
                            {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                            (let [neighbor (first neighbors)
                                  tentative-g-score (+ (get g-scores current) (get-point grid neighbor))]
                              (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                (recur
                                 (remove #{neighbor} neighbors)
                                 (assoc! froms neighbor current)
                                 (assoc! f-scores neighbor (+ tentative-g-score (manhattan-distance neighbor goal)))
                                 (assoc! g-scores neighbor tentative-g-score)
                                 (conj open-set (list neighbor (+ tentative-g-score (manhattan-distance neighbor goal)))))
                                (recur (remove #{neighbor} neighbors) froms f-scores g-scores open-set)))))]
                    (recur new-open-set new-froms new-f-scores new-g-scores)))))))))

(defn compute-risk-expanded-transient-sorted [f]
  (let [grid (parse-input f)
        expanded-grid (expand-grid grid)
        path (find-path-transient-sorted expanded-grid)]
    (if (not (seq? path))
      path
      (->> path
           rest ;; Don't count the starting position's risk.
           (map (fn [pos] (get-point expanded-grid pos)))
           (apply +)
           ))))


;; (time (compute-risk-expanded-transient-sorted small-input))
;; "Elapsed time: 71.6524 msecs"
;; 315

;; (time (compute-risk-expanded-transient-sorted large-input))
;; "Elapsed time: 8143.6049 msecs"
;; 2851
