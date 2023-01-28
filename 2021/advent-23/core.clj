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

(defn generate-neighbors [spaces]
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
    {:spaces (generate-neighbors spaces)
     :start-positions positions
     :rooms {\A #{[3 2] [3 3]} \B #{[5 2] [5 3]} \C #{[7 2] [7 3]} \D #{[9 2] [9 3]}}
     :room-tops {\A [3 2] \B [5 2] \C [7 2] \D [9 2]}
     :room-bottoms {\A [3 3] \B [5 3] \C [7 3] \D [9 3]}
     :room-blockers #{[3 1] [5 1] [7 1] [9 1]}
     :energy {\A 1 \B 10 \C 100 \D 1000}}))

(defn in-correct-room? [[pos amphi] state]
  (some (get (:rooms state) amphi) (list pos)))

(defn in-final-position? [[[x y] c :as position] positions state]
  (and (in-correct-room? position state)
       (or (= y 3)
           (= c (get positions [x (inc y)])))))

(defn in-room? [[[x y] amphi]]
  (> y 1))

(defn reachable [[[x y] c :as amphi-pos] positions state]
  (let [occupied (set (keys positions))]
    (loop [open-set #{[x y]} visited #{}]
      (if (empty? open-set)
        (remove (apply conj (:room-blockers state) #{[x y]}) visited)
        (let [neighbors (apply concat (map #(get (:spaces state) %) open-set))]
          (recur (->> neighbors
                      (remove visited)
                      (remove occupied)
                      (remove open-set)
                      set)
                 (apply conj visited open-set)))))))

(defn hallway-destinations [destinations state]
  (->> destinations
       (filter (fn [[x y]] (= y 1)))
       (remove (:room-blockers state))
       set))

(defn room-destination [amphi reachable positions state]
  (let [room-top (get (:room-tops state) amphi)
        room-bottom (get (:room-bottoms state) amphi)]
    (cond (some #{room-bottom} reachable) #{room-bottom}
          (and (some #{room-top} reachable) (= amphi (get positions room-bottom))) #{room-top}
          :else #{})))

(defn available-destinations [[[x y] amphi :as amphi-pos] positions state]
  (if (in-final-position? amphi-pos positions state)
    #{}
    (let [reachable (reachable amphi-pos positions state)]
      (if (in-room? amphi-pos)
        (apply conj (hallway-destinations reachable state)
               (room-destination amphi reachable positions state))
        (room-destination amphi reachable positions state)))))

(defn distance [pos1 pos2 spaces]
  (loop [visited #{pos1} i 0]
    (if (contains? visited pos2)
      i
      (recur (apply conj visited (mapcat #(get spaces %) visited)) (inc i)))))

(defn combined-distance [start1 start2 goal1 goal2 spaces]
  (min
   (+ (distance start1 goal1 spaces) (distance start2 goal2 spaces))
   (+ (distance start1 goal2 spaces) (distance start2 goal1 spaces))))

(defn reconstruct-path [froms goal state]
  (loop [path (list {:from goal :cost 0}) cost 0]
    (let [head (:from (first path))]
      (if (= head (:start-positions state))
        (list path cost)
        (let [{next-node :from next-cost :cost :as combined} (get froms head)]
         (recur (conj path combined) (+ cost next-cost)))))))

(defn move-amphi [positions from to]
  (-> positions
      (dissoc from)
      (assoc to (get positions from))))

(defn state-neighbors [positions state]
  (reduce (fn [acc [[x y] c :as pos]]
            (let [cost (get (:energy state) c)]
              (merge acc (reduce (fn [acc2 dest]
                                   (assoc acc2 (move-amphi positions [x y] dest) (* cost (distance [x y] dest (:spaces state)))))
                                 acc
                                 (available-destinations pos positions state)))))
          {}
          positions))

(defn draw [positions]
  (str/join
   (list "#############\n"
         "#"
         (str/join
          (for [i (range 1 12)]
            (get positions [i 1] ".")))
         "#\n"
         "###" (get positions [3 2] ".") "#" (get positions [5 2] ".") "#" (get positions [7 2] ".") "#" (get positions [9 2] ".") "###\n"
         "  #" (get positions [3 3] ".") "#" (get positions [5 3] ".") "#" (get positions [7 3] ".") "#" (get positions [9 3] ".") "#  \n"
         "  #########  \n"
         )))

(defn multimap-invert [m]
  (reduce (fn [acc [k v]] (update acc v (fn [old] (conj old k)))) {} m))

;; A heuristic function used to estimate the remaining distance to the goal (h-fn).
(defn total-energy-to-goal [positions state]
  (let [rooms (:rooms state)
        amphi-locations (multimap-invert positions)
        moves-to-goal (map #(list % (apply combined-distance (concat (get amphi-locations %) (sort (get rooms %)) (list (:spaces state)))))
                           [\A \B \C \D])
        total-energy (reduce (fn [acc [amphi steps]] (+ acc (* steps (get (:energy state) amphi)))) 0 moves-to-goal)]
    total-energy))

;; Another h-fn.
(defn amphis-out-of-place [positions state]
  (- 8 (count (filter #(in-final-position? % positions state) positions))))

(defn find-path [state h-fn]
  (let [start (:start-positions state)]
    (loop [open-set #{start}
           froms {}
           f-scores {start (h-fn start state)}
           g-scores {start 0}
           iteration 0]
      ;; (let [costs (map #(get f-scores %) open-set)
      ;;       min-cost (apply min costs)
      ;;       min-count (count (filter #(= min-cost %) costs))
      ;;       with-min (filter #(= min-cost (get f-scores %)) open-set)]
      ;;   (println "iteration" iteration (count open-set) "open nodes" "with" min-count "sharing the minimum estimated cost of" min-cost)
      ;;   (doall (map #(println (draw %)) with-min)))
      (cond (empty? open-set)
            :failure-no-open-nodes
            (= iteration 5000)
            {:failure-iterations (first (sort-by #(get f-scores %) open-set))}
            :else
            (let [current (first (sort-by #(get f-scores %) open-set))]
              ;; (println (draw current))
              (if (= 0 (total-energy-to-goal current state))
                (reconstruct-path froms current state)
                (let [open-set (disj open-set current)
                      move-costs (state-neighbors current state)
                      neighbors (keys move-costs)]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                        (reduce (fn [acc neighbor]
                                  (let [tentative-g-score (+ (get g-scores current) (get move-costs neighbor))]
                                    ;; (if (and (get g-scores neighbor) (< tentative-g-score (get g-scores neighbor)))
                                    ;;   (println "Found lower score for" neighbor))
                                    (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                      (-> acc
                                          (assoc-in [:froms neighbor] {:from current :cost (get move-costs neighbor)})
                                          (assoc-in [:g-scores neighbor] tentative-g-score)
                                          (assoc-in [:f-scores neighbor] (+ tentative-g-score (h-fn neighbor state)))
                                          (update :open-set (fn [s] (conj s neighbor))))
                                      acc)))
                                {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                                neighbors)]
                    ;; (doall (map (fn [pos]
                    ;;               (println "f-score" (get new-f-scores pos))
                    ;;               (println (draw pos))
                    ;;               (newline))
                    ;;             (sort-by #(get new-f-scores %) new-open-set)))
                    (recur new-open-set new-froms new-f-scores new-g-scores (inc iteration))))))))))

;; (time (let [state (parse-input small-input)]
;;                         (second (find-path state total-energy-to-goal))))
;; "Elapsed time: 9214.8808 msecs"
;; 12521

;; (time (let [state (parse-input large-input)]
;;                         (second (find-path state total-energy-to-goal))))
;; "Elapsed time: 42834.7085 msecs"
;; 15538
