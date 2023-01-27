(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 23: Amphipod

;; Part 1
;; What's the least energy required to organize the amphipods?
(defn simple-neighbors [[x y]]
  (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]))

(defn find-neighbors [spaces]
  (reduce (fn [acc space]
            (let [neighbors (simple-neighbors space)]
              (assoc acc space (filter spaces neighbors))))
          {}
          spaces))

(defn parse-input [f]
  (let [grid (->> f
                  read-lines
                  (mapv vec))
        width (count (first grid))
        height (count grid)
        combo-map (apply merge-with concat (for [j (range height)
                                                 i (range width)
                                                 c (list (get-in grid [j i]))
                                                 :when (not= c \#)]
                                             (if (= c \.)
                                               {:spaces (list [i j])}
                                               (assoc {:spaces (list [i j])} [i j] c))))
        positions (dissoc combo-map :spaces)
        spaces (set (get combo-map :spaces))]
    {:spaces (find-neighbors spaces)
     :start-positions positions
     :rooms {\A #{[3 2] [3 3]} \B #{[5 2] [5 3]} \C #{[7 2] [7 3]} \D #{[9 2] [9 3]}}
     :room-bottoms #{[3 3] [5 3] [7 3] [9 3]}
     :energy {\A 1 \B 10 \C 100 \D 1000}}))

(defn in-correct-room? [[pos c :as position] state]
  (boolean (some (get (:rooms state) c) (list pos))))

(defn want-to-move? [[[x y :as pos] c :as position] state]
  (not (and (in-correct-room? position state)
            (or (= y 3)
                (= c (get (:start-positions state) [x (inc y)]))))))

(defn distance [pos1 pos2 spaces]
  (loop [visited #{pos1} i 0]
    (if (contains? visited pos2)
      i
      (recur (apply conj visited (mapcat #(get spaces %) visited)) (inc i)))))

(defn combined-distance [start1 start2 goal1 goal2 spaces]
  (min
   (+ (distance start1 goal1 spaces) (distance start2 goal2 spaces))
   (+ (distance start1 goal2 spaces) (distance start2 goal1 spaces))))

(defn total-energy-to-goal [positions state]
  (let [rooms (:rooms state)
        amphi-locations (reduce (fn [acc [k v]] (update acc v (fn [old] (conj old k)))) {} positions)
        moves-to-goal (map #(list % (apply combined-distance (concat (get amphi-locations %) (sort (get rooms %)) (list (:spaces state)))))
                           [\A \B \C \D])
        total-energy (reduce (fn [acc [amphi steps]] (+ acc (* steps (get (:energy state) amphi)))) 0 moves-to-goal)]
    total-energy))

(defn find-path [state h-fn]
  (let [start (:start-positions state)]
   (loop [open-set #{}
          froms {}
          f-scores {start (h-fn start state)}
          g-scores {start 0}]
     (cond (empty? open-set)
           :failure-no-open-nodes
           :else
           (let [current (first (sort-by #(get f-scores %) open-set))]
                 ;;(first (first (sort-by second (map #(list % (get f-scores %)) open-set))))
             (if (= 0 (total-energy-to-goal current state))
               (reconstruct-path froms start goal)
               (let [open-set (disj open-set current)
                     neighbors (state-neighbors current state)]
                 (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                       (reduce (fn [acc neighbor]
                                 (let [tentative-g-score (+ (get g-scores current) (cost current neighbor state))]
                                   (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                     (-> result
                                         (assoc-in [:froms neighbor] current)
                                         (assoc-in [:g-scores neighbor] tentative-g-score)
                                         (assoc-in [:f-scores neighbor] (+ tentative-g-score (h-fn neighbor state)))
                                         (update :open-set (fn [s] (conj s neighbor))))
                                     result)))
                               {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                               neighbors)]
                   (recur new-open-set new-froms new-f-scores new-g-scores)))))))))
