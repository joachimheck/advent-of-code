(ns advent-15.core)

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

;; Day 15: Beverage Bandits

;; Part 1
;; What's the outcome of the combat?
(defrecord FieldPoint [x y type hp])

(defn parse-input [f]
  (let [vectors (->> f
                     (read-lines)
                     (map #(str/replace % #" +$" ""))
                     (mapv vec))
        width (count (first vectors))
        height (count vectors)
        combatant-id (atom 0)]
    (apply merge
           (for [j (range height)
                 i (range width)
                 :let [c (get-in vectors [j i])]
                 :when (not= c \.)]
             (let [result {[i j] (case c
                        \# {:type :wall}
                        \E {:type :elf :hp 200 :id @combatant-id :power 3}
                        \G {:type :goblin :hp 200 :id @combatant-id :power 3})}]
               (if (or (= c \E) (= c \G)) (swap! combatant-id inc))
               result)))))

(defn sort-positions [positions]
  (sort-by (fn [[[x y] _]] (+ (* 1000 y) x)) positions))

(defn combatants [field]
  (filter (fn [[_ {:keys [type]}]] (or (= type :elf) (= type :goblin))) field))

(defn find-targets [[[x y] {:keys [type]}] field]
  (filter (fn [[_ {other-type :type} :as input]]  (not= type other-type)) (combatants field)))

(defn adjacent-points [[x y]]
  (list [(inc x) y]
        [(dec x) y]
        [x (inc y)]
        [x (dec y)]))

(defn adjacent [[x y] field]
  (map (fn [p] [p (get field p)]) (adjacent-points [x y])))

(defn find-open-adjacent [targets field]
  (distinct
   (apply concat
          (for [target targets]
            (let [[[x y] _] target]
              (for [[p v] (adjacent [x y] field)
                    :when (nil? v)]
                [p v]))))))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn compute-path [start-p end-p distance visited]
  (loop [path (list end-p) i distance]
    (if (= (first path) start-p)
      path
      (recur (conj path (let [the-adjacent (adjacent-points (first path))]
                          (first (first (filter #(= (dec i) (second %)) (filter #(some #{(first %)} the-adjacent) visited))))))
             (dec i)))))

(defn path [[start-p _] [end-p _] field]
  (loop [open-set #{start-p} distance 0 visited {}]
    (cond (empty? open-set)
          nil
          (some #{end-p} open-set)
          (compute-path start-p end-p distance visited)
          :else
          (let [new-open-set (set (remove (set (keys visited)) (map first (find-open-adjacent (map (fn [p] [p nil]) open-set) field))))
;;                _ (println "new-open-set" new-open-set)
                new-distance (inc distance)
;;                _ (println "new-distance" new-distance)
                new-visited (reduce (fn [acc p] (if (get acc p) acc (assoc acc p distance))) visited open-set)
;;                _ (println "new-visited" new-visited)
                ]
            (recur new-open-set new-distance new-visited)))))

(defn list-with-distance [[p _ :as start] options]
  (map (fn [[o-p _ :as option]] (list option (distance p o-p))) options))

(defn list-with-path-length [[p _ :as start] options field]
  (map (fn [[o-p _ :as option]] (list option
                                      (let [the-path (path start option field)]
                                        (if the-path
                                          (dec (count the-path))))))
       options))

;; (defn find-closest [[p _ :as start] options field]
;;   (let [options-distance (remove #(nil? (second %)) (list-with-path-length start options field))
;;         sorted (sort-by second options-distance)
;;         min-distance (second (first sorted))
;;         at-min-distance (filter #(= min-distance (second %)) sorted)
;;         re-sorted (sort-positions (map first at-min-distance))]
;;     (first re-sorted)))

(defn find-closest [unit targets field]
  (let [targets-distance (sort-by second (list-with-distance unit targets))]
    (first (reduce (fn [[closest-target closest-path-length :as closest] [target distance]]
                     ;; (println "reduce-fn" closest [target distance])
                     (if (> distance closest-path-length)
                       closest
                       (let [target-path-length (count (path unit target field))]
                         (cond (> target-path-length closest-path-length)
                               closest
                               (< target-path-length closest-path-length)
                               [target target-path-length]
                               (= target-path-length closest-path-length)
                               [(first (sort-positions (list closest-target target))) target-path-length]))))
                   [nil Integer/MAX_VALUE]
                   targets-distance))))


(defn first-read-order [points]
  (first (sort-positions points)))

(defn reachable-from? [[[d-x d-y :as dest] _] [[s-x s-y :as start-p] _ :as start] field]
  (loop [open-set #{start-p} visited #{}]
    (cond (empty? open-set)
          false
          (some #{dest} open-set)
          true
          :else
          (let [current (first open-set)]
            (recur (apply conj (disj open-set current) (remove visited (set (map first (find-open-adjacent (list [current nil]) field)))))
                   (conj visited current))))))
 
;; (defn move [[p state :as unit] field]
;;   (let [targets (find-targets unit field)]
;;     (if (empty? targets)
;;       (assoc field :finished true)
;;       (let [distances-to-target (list-with-distance unit targets)
;;             adjacent-target (first-read-order (map first (filter #(= 1 (second %)) distances-to-target)))]
;;         (if adjacent-target
;;           field
;;           (let [destinations (filter #(reachable-from? % unit field) (find-open-adjacent targets field))
;;                 destination (find-closest unit destinations field)]
;;             (if destination
;;               (let [available-steps (filter #(reachable-from? % destination field) (find-open-adjacent (list unit) field))
;;                     step (find-closest destination available-steps field)]
;;                 (-> field
;;                     (dissoc p)
;;                     (assoc (first step) state)))
;;               field)))))))

(defn move [[p state :as unit] field]
  (let [targets (find-targets unit field)]
    (if (empty? targets)
      (assoc field :finished true)
      (let [destinations (filter #(reachable-from? % unit field) (find-open-adjacent targets field))]
        ;; (println "Moving" unit "already adjacent?" (some (set (adjacent-points p)) (map first targets)) (set (adjacent-points p)) (map first targets))
        (if (some (set (adjacent-points p)) (map first targets)) ; already adjacent to a target
          field
          (let [destination (find-closest unit destinations field)]
            (if destination
              (let [available-steps (filter #(reachable-from? % destination field) (find-open-adjacent (list unit) field))
                    step (find-closest destination available-steps field)]
                (-> field
                    (dissoc p)
                    (assoc (first step) state)))
              field)))))))

(defn attack [unit field]
  (let [targets (find-targets unit field)
        distances-to-target (list-with-distance unit targets)
        adjacent-targets (map first (filter #(= 1 (second %)) distances-to-target))]
    (if (empty? adjacent-targets)
      field
      (let [adjacent-with-hp (sort-by second (map #(list % (get (second %) :hp)) adjacent-targets))
            lowest-hp (second (first adjacent-with-hp))
            lowest-hp-targets (map first (filter #(= lowest-hp (second %)) adjacent-with-hp))
            adjacent-target (first-read-order lowest-hp-targets)
            attack-power (get (second unit) :power)]
        (if (> (get (second adjacent-target) :hp) attack-power)
          (update field (first adjacent-target) (fn [state] (update state :hp #(- % attack-power))))
          (dissoc field (first adjacent-target)))))))

(defn print-field [field]
  (let [field (dissoc field :finished)
        _ (if (empty? (map first (map first field))) (println "EMPTY FIELD" field))
        maxx (apply max (map first (map first field)))
        maxy (apply max (map second (map first field)))]
    (str/join "\n"
              (for [j (range (inc maxy))]
                (str/join
                 (concat (for [i (range (inc maxx))]
                           (let [{:keys [type] :as v} (get field [i j])]
                             (cond (nil? v)
                                   "."
                                   (= :wall type)
                                   "#"
                                   (= :elf type)
                                   "E"
                                   (= :goblin type)
                                   "G")))
                         '("   ")
                         (for [i (range (inc maxx))
                               :let [{:keys [type hp]} (get field [i j])]
                               :when (or (= type :elf) (= type :goblin))]
                           (format "%s(%03d), " (if (= type :elf) "E" "G") hp))))))))
(defn get-unit-by-id [id field]
  (first (filter #(= id (get (second %) :id)) field)))

(defn proc-unit [[p {:keys [id]} :as unit] field]
  (let [field (move unit field)]
    (if (get field :finished)
      field
      (let [unit (get-unit-by-id id field)]
        (attack unit field)))))

(defn proc-all [field]
  (let [combatant-ids (map :id (map second (sort-positions (combatants field))))
        result (reduce (fn [acc combatant-id]
                         (let [combatant (get-unit-by-id combatant-id acc)]
                           ;; (println "reduce-fn" combatant-id ":" combatant)
                           (if combatant
                             (let [processed (proc-unit combatant acc)]
                               (if (get processed :finished)
                                 (reduced processed)
                                 processed))
                             acc)))
                       field
                       combatant-ids)]
    (if (nil? result)
      (println "Nil result for field:" field))
    result))

(defn total-hp [field]
  (apply + (map #(get % :hp 0) (vals field))))

(defn run-combat [field]
  (loop [i 0 field field]
    (let [processed (proc-all field)]
      (if (get processed :finished)
         (format "Battle outcome: %d rounds, %d total hp: %d." i (total-hp processed) (* i (total-hp processed)))
        (recur (inc i) processed)))))

;; (time (run-combat (parse-input small-input)))
;; "Elapsed time: 28.935 msecs"
;; "Battle outcome: 47 rounds, 590 total hp: 27730."

;; (time (run-combat (parse-input large-input)))
;; "Elapsed time: 181017.6448 msecs"
;; "Battle outcome: 102 rounds, 2592 total hp: 264384."



;; Part 2
;; What's the minimum attack power the elves need to win without losing anybody?
(defn set-attack-power [field power]
  (reduce-kv (fn [m k v]
               (assoc m k (if (= :elf (get v :type))
                            (assoc v :power power)
                            v)))
             {}
             field))

(defn count-by-type [field type]
  (count (filter #(= type (:type %)) (vals field))))

(defn print-outcome [{:keys [final-field rounds initial-field power]}]
  (let [elves-left (count-by-type final-field :elf)
        goblins-left (count-by-type final-field :goblin)
        winner (if (> elves-left goblins-left) "Elves" "Goblins")
        elves-lost (- (count-by-type initial-field :elf) elves-left)]
    ;; (println (print-field final-field))
    (printf "Combat ends after %d full rounds\n" rounds)
    (printf "%s win with %d total hit points left\n" winner (total-hp final-field))
    (printf "Elf losses: %d\n" elves-lost)
    (printf "Elf attack power: %d\n" power)
    (printf "Outcome: %d * %d = %d\n" rounds (total-hp final-field) (* rounds (total-hp final-field)))))

(defn run-combat-2 [initial-field power & {:keys [debug] :as opts}]
  (loop [i 0 field (set-attack-power initial-field power)]
    (if debug
      (do
       (println "\nAfter" i "rounds:")
       (println (print-field field))))
    (let [processed (proc-all field)]
      (cond
        (get processed :finished)
        (let [elves-left (count-by-type processed :elf)
              goblins-left (count-by-type processed :goblin)
              winner (if (> elves-left goblins-left) "Elves" "Goblins")
              elves-lost (- (count-by-type initial-field :elf) elves-left)]
          {:winner winner :elves-lost elves-lost :rounds i :final-field processed :power power :initial-field initial-field})
        :else
        (recur (inc i) processed)))))

(defn find-optimal-attack-power [field]
  (let [[result max-power prev-power] (loop [power 4 prev-power 4]
                                        (println (java.util.Date.) "Looping up range" prev-power power)
                                        (let [result (run-combat-2 field power)]
                                          (println (-> result
                                                       (assoc :final-field nil)
                                                       (assoc :initial-field nil)))
                                          (if (and (= "Elves" (:winner result))
                                                   (= 0 (:elves-lost result)))
                                            [result power prev-power]
                                            (recur (* 2 power) power))))]
    (loop [max-power max-power min-power prev-power results {max-power result}]
      (println (java.util.Date.) "Binary search range" min-power max-power)
      (if (or (= max-power min-power) (= max-power (inc min-power)))
        (print-outcome (get results max-power))
        (let [power (+ min-power (quot (- max-power min-power) 2))
              result (run-combat-2 field power)]
          (if (and (= "Elves" (:winner result))
                   (= 0 (:elves-lost result)))
            (recur power min-power (assoc results power result))
            (recur max-power power results)))))))

(defn summarize [result]
  (let [rounds (get result :rounds)
        hps (apply + (map :hp (map second (combatants (get result :final-field)))))
        result (if (and rounds hps)
                 (assoc result :outcome (* rounds hps))
                 result)
        result (dissoc result :final-field :initial-field)
        ]
    result))

;; 22932
;; ---> answer <---
;; 67160
;; 73170


;; (summarize (run-combat-2 (parse-input large-input) 3))
;; result hps (119 107 134 113 74 134 191 194 173 41 149 56 68 119 98)
;; {:winner "Goblins", :elves-lost 10, :rounds 150, :power 3, :outcome 265500}
;; The correct answer, from above, is 264384.


;; (time (find-optimal-attack-power (parse-input small-input)))
;; #inst "2023-03-29T12:40:04.372-00:00" Looping up range 4 4
;; {:winner Goblins, :elves-lost 2, :rounds 47, :final-field nil, :power 4, :initial-field nil}
;; #inst "2023-03-29T12:40:04.428-00:00" Looping up range 4 8
;; {:winner Goblins, :elves-lost 2, :rounds 49, :final-field nil, :power 8, :initial-field nil}
;; #inst "2023-03-29T12:40:04.456-00:00" Looping up range 8 16
;; {:winner Elves, :elves-lost 0, :rounds 27, :final-field nil, :power 16, :initial-field nil}
;; #inst "2023-03-29T12:40:04.471-00:00" Binary search range 8 16
;; #inst "2023-03-29T12:40:04.486-00:00" Binary search range 12 16
;; #inst "2023-03-29T12:40:04.500-00:00" Binary search range 14 16
;; #inst "2023-03-29T12:40:04.516-00:00" Binary search range 14 15
;; Combat ends after 29 full rounds
;; Elves win with 172 total hit points left
;; Elf losses: 0
;; Elf attack power: 15
;; Outcome: 29 * 172 = 4988
;; "Elapsed time: 147.6266 msecs"
;; nil

;; (time (find-optimal-attack-power (parse-input large-input)))
;; #inst "2023-03-29T12:40:36.084-00:00" Looping up range 4 4
;; {:winner Goblins, :elves-lost 10, :rounds 100, :final-field nil, :power 4, :initial-field nil}
;; #inst "2023-03-29T12:41:53.850-00:00" Looping up range 4 8
;; {:winner Elves, :elves-lost 8, :rounds 117, :final-field nil, :power 8, :initial-field nil}
;; #inst "2023-03-29T12:42:55.529-00:00" Looping up range 8 16
;; {:winner Elves, :elves-lost 2, :rounds 55, :final-field nil, :power 16, :initial-field nil}
;; #inst "2023-03-29T12:43:57.525-00:00" Looping up range 16 32
;; {:winner Elves, :elves-lost 0, :rounds 41, :final-field nil, :power 32, :initial-field nil}
;; #inst "2023-03-29T12:44:54.497-00:00" Binary search range 16 32
;; #inst "2023-03-29T12:45:59.443-00:00" Binary search range 16 24
;; #inst "2023-03-29T12:47:02.822-00:00" Binary search range 16 20
;; #inst "2023-03-29T12:48:00.298-00:00" Binary search range 18 20
;; #inst "2023-03-29T12:48:55.740-00:00" Binary search range 19 20
;; Combat ends after 46 full rounds
;; Elves win with 1457 total hit points left
;; Elf losses: 0
;; Elf attack power: 20
;; Outcome: 46 * 1457 = 67022
;; "Elapsed time: 499663.788 msecs"
;; nil
