(ns advent-04.core)

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

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (let [grid (->> input
                  (read-lines)
                  (map vec)
                  vec)
        height (count grid)
        width (count (first grid))]
    {:grid (into {}
                 (for [j (range height)
                       i (range width)]
                   (vector [i j] (get-in grid [j i]))))
     :bounds [width height]}
    ))
    

;; Part 1
;; How many rolls of paper can be accessed by a forklift?

(defn adjacent-points [[x y] {:keys [grid bounds] :as state}]
  (let [[b-x b-y] bounds]
    (filter (fn [[x y]] (and (< -1 x b-x) (< -1 y b-y)))
            (list [(dec x) (dec y)]
                  [x (dec y)]
                  [(inc x) (dec y)]
                  [(dec x) y]
                  [(inc x) y]
                  [(dec x) (inc y)]
                  [x (inc y)]
                  [(inc x) (inc y)]))))

(defn adjacent [pos {:keys [grid bounds] :as state}]
  (map (fn [[x y]] (get grid [x y])) (adjacent-points pos state)))

(defn count-moveable-rolls [{:keys [grid bounds] :as state}]
  (->> (keys grid)
       (filter #(= \@ (get grid %)))
       (map #(adjacent % state))
       (map #(filter #{\@} %))
       (map count)
       (filter #(< % 4))
       (count)))

;; (time (count-moveable-rolls (parse-input small-input)))
;; "Elapsed time: 5.5283 msecs"
;; 13
;; (time (count-moveable-rolls (parse-input large-input)))
;; "Elapsed time: 149.152 msecs"
;; 1587


;; Part 2
;; How many rolls can be removed?

(defn get-moveable-rolls [{:keys [grid bounds] :as state}]
  (->> (keys grid)
       (filter #(= \@ (get grid %)))
       (map (fn [pos] (list pos (count (filter #{\@} (adjacent pos state))))))
       (sort-by first)
       (filter #(< (second %) 4))
       (map first)
       ))

(defn remove-and-count-rolls [{:keys [grid bounds] :as state}]
  (loop [removed 0
         state state]
    ;; (println "nodes remaining"
    ;;          (->> grid
    ;;               (filter #(#{\@} (second %)))
    ;;               (count)))
    (let [to-remove (sort (get-moveable-rolls state))
          ;; _ (println "assoc vec" (mapcat list to-remove (repeat \.)))
          grid (:grid state)]
      (if (zero? (count to-remove))
          removed
          (recur (+ removed (count to-remove))
                 (assoc state :grid
                        (apply assoc grid (mapcat list to-remove (repeat \.)))))))))

;; (time (remove-and-count-rolls (parse-input small-input)))
;; "Elapsed time: 6.5521 msecs"
;; 43
;; (time (remove-and-count-rolls (parse-input large-input)))
;; "Elapsed time: 2039.9603 msecs"
;; 8946
