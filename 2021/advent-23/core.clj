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

(defn pairs [coll]
  (if (>= (count coll) 2)
    (concat (map #(set [(first coll) %]) (rest coll))
            (pairs (rest coll)))
    '()))

(defn distance [pos1 pos2 spaces]
  (loop [visited #{pos1} i 0]
    (if (contains? visited pos2)
      i
      (recur (apply conj visited (mapcat #(get spaces %) visited)) (inc i)))))

(defn parse-input [f]
  (let [grid (->> f
                  read-lines
                  (mapv vec))
        width (count (first grid))
        height (count grid)
        combo-map (apply merge-with concat (for [j (range height)
                                                 i (range width)
                                                 c (list (get-in grid [j i] \space))
                                                 :when (and (not= c \#) (not= c \space))]
                                             (if (= c \.)
                                               {:spaces (list [i j])}
                                               (assoc {:spaces (list [i j])} [i j] c))))
        positions (dissoc combo-map :spaces)
        spaces (set (get combo-map :spaces))
        spaces-map (generate-neighbors spaces)]
    {:spaces spaces-map
     :distances (reduce (fn [acc [p1 p2]] (assoc acc #{p1 p2} (distance p1 p2 spaces-map))) {} (map seq (pairs spaces)))
     :start-positions positions
     :rooms {\A (filter #(some #{%} spaces) '([3 5] [3 4] [3 3] [3 2]))
             \B (filter #(some #{%} spaces) '([5 5] [5 4] [5 3] [5 2]))
             \C (filter #(some #{%} spaces) '([7 5] [7 4] [7 3] [7 2]))
             \D (filter #(some #{%} spaces) '([9 5] [9 4] [9 3] [9 2]))}
     :room-blockers #{[3 1] [5 1] [7 1] [9 1]}
     :energy {\A 1 \B 10 \C 100 \D 1000}
     :max-y (apply max (map second spaces))}))

(defn in-correct-room? [[pos amphi] state]
  (some (set (get (:rooms state) amphi)) (list pos)))

(defn in-final-position? [[[x y] c :as position] positions state]
  (and (in-correct-room? position state)
       (or (= y (:max-y state))
           (every? #(= c %) (for [j (range (inc y) (inc (:max-y state)))] (get positions [x j]))))))

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
  (let [room (get (:rooms state) amphi)]
    (loop [room room]
      (let [space (first room)]
        (if (some #{space} reachable)
          #{space}
          (if (not= (get positions space) amphi)
            #{}
            (recur (rest room))))))))

(defn available-destinations [[[x y] amphi :as amphi-pos] positions state]
  (if (in-final-position? amphi-pos positions state)
    #{}
    (let [reachable (reachable amphi-pos positions state)]
      (if (in-room? amphi-pos)
        (apply conj (hallway-destinations reachable state)
               (room-destination amphi reachable positions state))
        (room-destination amphi reachable positions state)))))

(defn permutations [coll]
  (cond (< (count coll) 2) (list coll)
        (= (count coll) 2) (list coll (reverse coll))
        :else
        (apply concat (for [i (range (count coll))]
                        (map #(concat (list (nth coll i)) %) (permutations (concat (take i coll) (drop (inc i) coll))))))))

(defn paired-permutations [c1 c2]
  (for [p1 (permutations c1)
        p2 (permutations c2)]
    (list p1 p2)))

(defn get-distance [p1 p2 state]
  (if (= p1 p2)
    0
    (get (:distances state) (set [p1 p2]))))

(defn combined-distance [starts goals state]
  (apply + (map (fn [s g] (get-distance s g state)) starts goals)))

;; The above seems to work even though it doesn't compute the minimum distance.
;; (let [distances (map (fn [[s g]]
;;                        (apply + (map #(get-distance %1 %2 state) s g)))
;;                      (paired-permutations starts goals))]
;;   (apply min distances))

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
                                   (assoc acc2 (move-amphi positions [x y] dest) (* cost (get-distance [x y] dest state))))
                                 acc
                                 (available-destinations pos positions state)))))
          {}
          positions))

(defn draw [positions]
  (let [is-big (> (apply max (map second (map first positions))) 3)]
    (str/join
     (concat
      (list "#############\n"
            "#"
            (str/join
             (for [i (range 1 12)]
               (get positions [i 1] ".")))
            "#\n"
            "###" (get positions [3 2] ".") "#" (get positions [5 2] ".") "#" (get positions [7 2] ".") "#" (get positions [9 2] ".") "###\n"
            "  #" (get positions [3 3] ".") "#" (get positions [5 3] ".") "#" (get positions [7 3] ".") "#" (get positions [9 3] ".") "#  \n")
      (if is-big
        (list "  #" (get positions [3 4] ".") "#" (get positions [5 4] ".") "#" (get positions [7 4] ".") "#" (get positions [9 4] ".") "#  \n"
              "  #" (get positions [3 5] ".") "#" (get positions [5 5] ".") "#" (get positions [7 5] ".") "#" (get positions [9 5] ".") "#  \n")
        '())
      (list "  #########  \n")))))

(defn multimap-invert [m]
  (reduce (fn [acc [k v]] (update acc v (fn [old] (conj old k)))) {} m))

;; A heuristic function used to estimate the remaining distance to the goal (h-fn).
(defn total-energy-to-goal [positions state]
  (let [rooms (:rooms state)
        amphi-locations (multimap-invert positions)
        moves-to-goal (map (fn [amphi]
                             (let [room (get rooms amphi)
                                   amphi-locations (get amphi-locations amphi)
                                   out-of-room (remove (set room) amphi-locations)
                                   open-in-room (remove (set amphi-locations) room)]
                               (list amphi (combined-distance out-of-room open-in-room state))))
                           [\A \B \C \D])
        total-energy (reduce (fn [acc [amphi steps]] (+ acc (* steps (get (:energy state) amphi)))) 0 moves-to-goal)]
    total-energy))

;; A bad h-fn.
(defn amphis-out-of-place [positions state]
  (- (count positions) (count (filter #(in-final-position? % positions state) positions))))

(defn in-right-room-or-hallway [positions state]
  (let [in-room (map #(get (:energy state) (second %)) (filter #(in-final-position? % positions state) positions))
        in-hallway (map #(get (:energy state) (second %)) (filter #(= 1 (second (first %))) positions))]
    (- (+ (apply + in-hallway) (* 10 (apply + in-room))))))

(defn cost-to-move-remaining [positions state]
  (let [to-move (remove #(in-final-position? % positions state) positions)
        {in-room true in-hallway false} (group-by in-room? to-move)
        base-costs (map (fn [pos-list] (->> pos-list
                                            (map second)
                                            (map #(get (:energy state) %))
                                            (apply +)))
                        (list in-room in-hallway))]
    (+ (* 2 (first base-costs)) (second base-costs))))

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
      ;;   (println "iteration" iteration (count open-set) "open nodes" "with" min-count "sharing the minimum estimated cost of" min-cost))
      ;;        (doall (map #(println (draw %)) with-min))
      (cond (empty? open-set)
            {:status "error"
             :error "no open nodes"
             :iterastions iteration}
            ;; (= iteration 10000)
            ;; {:status "error"
            ;;  :error "reached maximum iterations"
            ;;  :current-state (first (sort-by #(get f-scores %) open-set))}
            :else
            (let [current (first (sort-by #(get f-scores %) open-set))]
              ;; (println (draw current))
              (if (= 0 (total-energy-to-goal current state))
                (let [[path cost] (reconstruct-path froms current state)]
                 {:status "success"
                  :path path
                  :cost cost
                  :iterations iteration})
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



;; (time (let [state (parse-input small-input)]
;;                         (dissoc (find-path state total-energy-to-goal) :path)))
;; "Elapsed time: 6279.9049 msecs"
;; {:status "success", :cost 12521, :iterations 1189}

;; (time (let [state (parse-input large-input)]
;;                         (dissoc (find-path state total-energy-to-goal) :path)))
;; "Elapsed time: 35045.0699 msecs"
;; {:status "success", :cost 15538, :iterations 2819}




;; Part 2
;; The rooms have doubled in size.
(def small-input-2 "small-input-2.txt")
(def large-input-2 "large-input-2.txt")

;; I never came up with faster code; I just let it run.

;; (time (let [state (parse-input small-input-2)]
;;                         (assoc (find-path state total-energy-to-goal) :path nil)))
;; "Elapsed time: 6104800.7486 msecs"
;; {:status "success", :path nil, :cost 44169, :iterations 78131}

;; (time (let [state (parse-input large-input-2)]
;;                         (dissoc (find-path state total-energy-to-goal) :path)))
;; "Elapsed time: 4435404.6526 msecs"
;; {:status "success", :cost 47258, :iterations 71291}
