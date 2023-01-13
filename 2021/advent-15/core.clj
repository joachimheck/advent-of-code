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
                (let [open-set (into #{} (remove #{current} open-set))
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



