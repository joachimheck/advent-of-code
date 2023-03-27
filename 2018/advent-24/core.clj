(ns advent-24.core)

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

;; Day 24: Immune System Simulator 20XX

;; Part 1
;; How many units are left in the winning army?
(defrecord Group [units hp conditions attack attack-type initiative army id])

(defn parse-subconditions [s]
  (let [c-strings (second (str/split s #" to "))
        conditions (map str/trim (str/split c-strings #","))]
    conditions))

(defn parse-conditions [s]
  (if (or (empty? s) (nil? s))
    {:immune nil :weak nil}
    (let [substrings (str/split (second (re-matches #"\((.+)\) " s)) #"; ")
          {:strs [weak immune] :as grouped} (group-by #(second (re-matches #"(\S+) .+" %)) substrings)]
      {:weak (if (seq weak) (parse-subconditions (first weak)))
       :immune (if (seq immune) (parse-subconditions (first immune)))})))

(deftest test-parse-conditions
  (is (= {:weak '("radiation" "bludgeoning") :immune nil}
         (parse-conditions "(weak to radiation, bludgeoning) ")))
  (is (= {:immune '("fire") :weak nil}
         (parse-conditions "(immune to fire) ")))
  (is (= {:weak '("bludgeoning") :immune '("slashing" "fire")}
         (parse-conditions "(weak to bludgeoning; immune to slashing, fire) ")))
  (is (= {:weak nil :immune nil}
         (parse-conditions nil))))

(defn parse-line [line army id]
  (let [[units hp conditions attack attack-type initiative :as unit]
        (rest (re-matches #"(\d+) units each with (\d+) hit points (\(.+\) )?with an attack that does (\d+) (.+) damage at initiative (\d+)" line))]
    (Group. (parse-long units) (parse-long hp) (parse-conditions conditions) (parse-long attack) attack-type (parse-long initiative) army id)))

(deftest test-parse-line
  (is (= (Group. 17 5390 {:weak '("radiation" "bludgeoning") :immune nil} 4507 "fire" 2 "x" 1)
         (parse-line "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2" "x" 1)))
  (is (= (Group. 116 7940 {:weak nil :immune nil} 638 "fire" 18 "y" 2)
         (parse-line "116 units each with 7940 hit points with an attack that does 638 fire damage at initiative 18" "y" 2))))

(defn parse-input [f]
  (let [[immune-lines _ infection-lines] (map rest (partition-by #{""} (read-lines f)))]
    (concat (map-indexed #(parse-line %2 "Immune System" (inc %1)) immune-lines)
            (map-indexed #(parse-line %2 "Infection" (inc %1)) infection-lines))))

(defn effective-power [group]
  (* (:units group) (:attack group)))

(defn damage [attacker defender]
  (let [base-damage (effective-power attacker)]
    (cond (some #{(:attack-type attacker)} (:immune (:conditions defender))) 0
          (some #{(:attack-type attacker)} (:weak (:conditions defender))) (* 2 base-damage)
          :else base-damage)))

(deftest test-damage
  (is (= 185832 (damage (Group. 801 4706 {:weak '("radiation") :immune nil} 116 "bludgeoning" 1 "Infection" 1)
                        (Group. 17 5390 {:weak '("radiation" "bludgeoning") :immune nil} 4507 "fire" 2 "Immune System" 1)))))

(defn group-name [group]
  (format "%s group %d" (:army group) (:id group)))

(defn comparator-multi [fns]
  (fn [a b]
    (loop [fns fns]
      (if (empty? fns)
        0
        (let [the-fn (first fns)
              comparison (compare (the-fn a) (the-fn b))]
          (if (not= 0 comparison)
            comparison
            (recur (rest fns))))))))

(defn select-best-target [attacker groups]
  (let [defenders (filter #(not= (:army attacker) (:army %)) groups)
        damageable (filter #(> (damage attacker %) 0) defenders)]
    (last (sort-by identity (comparator-multi [(partial damage attacker) effective-power :initiative]) damageable))))

(defn select-targets [groups]
  (loop [attackers (reverse (sort-by identity (comparator-multi [effective-power :initiative]) groups))
         groups-left groups
         target-map {}]
    (if (empty? attackers)
      target-map
      (let [attacker (first attackers)
            target (select-best-target attacker groups-left)]
        (recur (rest attackers)
               (remove #{target} groups-left)
               (assoc target-map (group-name attacker) (group-name target)))))))

(defn process-fight [groups]
  ;; (print-summary groups)
  ;; (newline)
  (let [target-map (select-targets groups)
        ;; _ (newline)
        new-groups (loop [attackers (map group-name (reverse (sort-by :initiative groups)))
                          remaining-groups (into {} (map (fn [g] [(group-name g) g]) groups))]
                     (if (empty? attackers)
                       (vals remaining-groups)
                       (let [attacker-name (first attackers)
                             attacker (get remaining-groups attacker-name)
                             defender-name (get target-map attacker-name)
                             defender (get remaining-groups defender-name)]
                         (if (and attacker defender)
                           (let [damage (damage attacker defender)
                                 units-killed (quot damage (:hp defender))]
                             ;; (printf "%s attacks %s, killing %d units\n" attacker-name defender-name (min units-killed (:units defender)))
                             (recur (rest attackers)
                                    (if (>= units-killed (:units defender))
                                      (dissoc remaining-groups defender-name)
                                      (assoc remaining-groups defender-name (update defender :units #(- % units-killed))))))
                           (recur (rest attackers) remaining-groups)))))]
    ;; (newline)
    new-groups))

(defn print-summary [groups]
  (let [armies (group-by :army groups)]
    (doseq [army-name ["Immune System" "Infection"]]
      (let [army (get armies army-name)]
        (printf "%s:\n" army-name)
        (if (empty? army)
          (println "No groups remain.")
          (doseq [group (sort-by :id army)]
            (println "Group" (:id group) "contains" (:units group) "units")))))))

(defn process-war [groups]
  (loop [groups groups
         i 0]
    (if (< (count (group-by :army groups)) 2)
      (do (println "After" i "rounds:")
          (print-summary groups)
          (apply + (map :units groups)))
      (recur (process-fight groups) (inc i)))))

;; ---> answer <---
;; 21082


;; (time (process-war (parse-input small-input)))
;; After 8 rounds:
;; Immune System:
;; No groups remain.
;; Infection:
;; Group 1 contains 782 units
;; Group 2 contains 4434 units
;; "Elapsed time: 4.3522 msecs"
;; 5216

;; (time (process-war (parse-input large-input)))
;; After 688 rounds:
;; Immune System:
;; No groups remain.
;; Infection:
;; Group 1 contains 2284 units
;; Group 2 contains 818 units
;; Group 3 contains 2476 units
;; Group 4 contains 6643 units
;; Group 5 contains 1497 units
;; Group 6 contains 99 units
;; Group 7 contains 1219 units
;; Group 8 contains 4390 units
;; Group 9 contains 329 units
;; Group 10 contains 1315 units
;; "Elapsed time: 190.1614 msecs"
;; 21070



;; Part 2
;; How many units does the immune system have left after getting the smallest boost it needs to win?
(defn boost [groups amount]
  (map (fn [group]
         (if (= (:army group) "Immune System")
           (update group :attack #(+ % amount))
           group))
       groups))

(defn process-war-2 [groups]
  (loop [groups groups
         i 0]
    (if (< (count (group-by :army groups)) 2)
      {:winner (:army (first groups))
       :units (apply + (map :units groups))}
      (recur (process-fight groups) (inc i)))))

(defn find-minimum-boost [groups]
  (let [max-boost (loop [amount 1]
                    (if (= (:winner (process-war-2 (boost groups amount))) "Immune System")
                      amount
                      (recur (* 2 amount))))]
    (loop [largest-ineffective 0
           smallest-effective max-boost
           i 0]
      (println "Iteration" i [largest-ineffective smallest-effective])
      (if (or (> i 12)
              (= 1 (- smallest-effective largest-ineffective)))
        {:boost smallest-effective
         :results (process-war-2 (boost groups smallest-effective))}
        (let [current (quot (+ largest-ineffective smallest-effective) 2)]
          (if (= (:winner (process-war-2 (boost groups current))) "Immune System")
            (recur largest-ineffective current (inc i))
            (recur current smallest-effective (inc i))))))))

