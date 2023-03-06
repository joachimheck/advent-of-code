(ns advent-18.core)

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

;; Day 18: Settlers of the North Pole

;; Part 1
;; What will the total resource value of the lumber collection area be after 10 minutes?
(defn parse-input [f]
  (let [grid (->> f
                  (read-lines)
                  (mapv vec))
        bounds [(count (first grid)) (count grid)]]
    {:grid grid :bounds bounds}))

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
  (map (fn [[x y]] (get-in grid [y x])) (adjacent-points pos state)))

(defn new-state [[x y :as pos] {:keys [grid bounds] :as state}]
  (let [acre (get-in grid [y x])
        adjacent-acres (adjacent pos state)
        adjacent-trees (filter #{\|} adjacent-acres)
        adjacent-lumberyards (filter #{\#} adjacent-acres)]
    (case acre
      \. (if (>= (count adjacent-trees) 3) \| \.)
      \| (if (>= (count adjacent-lumberyards) 3) \# \|)
      \# (if (and (> (count adjacent-lumberyards) 0)
                  (> (count adjacent-trees) 0)) \# \.))))

(defn process-area [{:keys [grid bounds] :as state}]
  (assoc state
         :grid
         (let [[b-x b-y] bounds]
           (vec
            (for [j (range b-y)]
              (vec
               (for [i (range b-x)]
                 (new-state [i j] state))))))))

(defn draw-area [{:keys [grid bounds] :as state}]
  (let [[b-x b-y] bounds]
    (str/join
     "\n"
     (for [j (range b-y)]
       (str/join (get grid j))))))

(defn process-multiply [initial-state n]
  (loop [i 0 state initial-state]
    (if (= i n)
      state
      (recur (inc i) (process-area state)))))

(defn get-resource-value [{:keys [grid bounds] :as state}]
  (let [flat (flatten grid)
        trees (filter #{\|} flat)
        lumberyards (filter #{\#} flat)]
    (* (count trees) (count lumberyards))))

;; (time (get-resource-value (process-multiply (parse-input small-input) 10)))
;; "Elapsed time: 13.2711 msecs"
;; 1147

;; (time (get-resource-value (process-multiply (parse-input large-input) 10)))
;; "Elapsed time: 228.6936 msecs"
;; 514944



;; Part 2
;; What will the total resource value of the lumber collection area be after 1000000000 minutes?
;; (defn find-recurring-state [{:keys [grid bounds] :as initial-state} max-iterations]
;;   (loop [i 0 state initial-state states {}]
;;     (let [resource-value (get-resource-value state)]
;;      (cond (= i max-iterations)
;;            "Reached maximum iterations without finding a recurring state."
;;            (get states state)
;;            (format "Found recurring state at iteration %d (previously at %d).\n"
;;                    i (get states state))
;;            :else
;;            (recur (inc i) (process-area state) (assoc states i state))))))

(defn get-resource-values [{:keys [grid bounds] :as initial-state} max-iterations]
  (loop [i 0 state initial-state results []]
    (if (= i max-iterations)
      results
      (recur (inc i) (process-area state) (conj results (get-resource-value state))))))

(defn find-recurring-state [{:keys [grid bounds] :as initial-state} max-iterations]
  (loop [i 0 state initial-state resource-values {}]
    (let [resource-value (get-resource-value state)]
      (if (get resource-values resource-value)
       (printf "Found recurring resource value %d at iteration %d (previously at %d).\n" resource-value i (get resource-values resource-value)))
      (if (= i max-iterations)
        "Reached maximum iterations without finding a recurring state."
        (recur (inc i) (process-area state) (assoc resource-values resource-value i))))))

;; Found loop from iteration 606 to 634, so the length is 28.
(defn process-multiply-2 [initial-state n]
  ;; Assume (> n 605)
  (let [loop-n (mod (- n 606) 28)
        state-606 (process-multiply initial-state 606)]
   (loop [i 0 state state-606]
     (if (= i loop-n)
       state
       (recur (inc i) (process-area state))))))

;; (time (get-resource-value (process-multiply-2 (parse-input large-input) 1000000000)))
;; "Elapsed time: 10988.1713 msecs"
;; 193050
