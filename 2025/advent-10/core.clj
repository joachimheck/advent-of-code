(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

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

(defn parse-wiring-schematic [s]
  (->> s
       (re-seq #"\([\d\,]+\)")
       (map (fn [ns] (re-seq #"\d" ns)))
       (mapv #(map parse-long %))))

(defn parse-indicator-lights [s]
  (vec
   (for [c (re-find #"[.#]+" s)]
     (if (= c \.) 0 1))))

(defn parse-joltage-requirements [s]
  (->> s
       (re-seq #"\d+")
       (mapv parse-long)))

(defn parse-line [line]
  (let [[indicator-lights wiring-schematics joltage-requirements]
        (rest (re-matches #"(\[[.#]+\]) (.+) (\{.+\})" line))]
    {:indicator-lights (parse-indicator-lights indicator-lights)
     :wiring-schematics (parse-wiring-schematic wiring-schematics)
     :joltage-requirements (parse-joltage-requirements joltage-requirements)}))

(defn parse-input [input]
  (->> input
       (read-lines)
       (map parse-line)))


;; Part 1
;; Find the fewest total number of button presses to configure the devices so the
;; indicator lights match.
(defn lights-to-integer [lights]
  (reduce (fn [acc l]
            (if (= 1 l)
              (inc (bit-shift-left acc 1))
              (bit-shift-left acc 1)))
          lights))

(defn flip-lights [lights to-flip]
  (reduce (fn [acc n]
            ;; (println :acc acc :n n :to-flip to-flip :contains (contains? to-flip n))
            (conj acc (if (some #{n} to-flip)
                          (if (= (get lights n) 0) 1 0)
                          (get lights n))))
          []
          (take (count lights) (iterate inc 0))))

  ;; (reduce (fn [acc tf]
  ;;           (if (or (< tf 0) (> tf (dec (count lights))))
  ;;             (throw (Exception. (str ":error-invalid-flip: " to-flip)))
  ;;             (update acc tf (fn [l] (if (= l 0) 1 0)))))
  ;;         lights
  ;;         to-flip)


(defn press-button
  ([{:keys [indicator-lights wiring-schematics] :as device} button]
   {:indicator-lights (press-button indicator-lights wiring-schematics button)
    :wiring-schematics wiring-schematics})
  ([indicator-lights wiring-schematics button]
   (if (or (< button 0) (> button (count wiring-schematics)))
     (throw (Exception. (str ":error-invalid-button: " button)))
     (let [to-flip (get wiring-schematics button)]
       (flip-lights indicator-lights to-flip)))))

(defn shortest-path-to-start [{:keys [indicator-lights wiring-schematics] :as device}]
  (let [initial-lights (vec (repeat (count indicator-lights) 0))
        buttons wiring-schematics
        button-numbers (take (count buttons) (iterate inc 0))]
    (loop [states [[initial-lights []]]
           computed-states {initial-lights []}
           loop-count 0]
      (let [[lights presses :as current] (first states)
            next-states (map (fn [b]
                               (vector (press-button lights wiring-schematics b) (conj presses b)))
                             (remove #(= % (last presses)) button-numbers))
            new-states (remove (fn [[lights buttons]] (get computed-states lights)) next-states)
            new-computed-states (into computed-states new-states)]
        (if (or (get new-computed-states indicator-lights)
                (> loop-count 20000))
          {:device device
           :correct-presses (get new-computed-states indicator-lights)
           :states (count states)
           :loop-count loop-count}
          (recur (doall (concat (rest states) new-states)) new-computed-states (inc loop-count)))))))

(defn get-configurations [devices]
  (map (fn [state]
         [(:indicator-lights (:device state)) (count (:correct-presses state))])
       (for [d devices]
         (let [path (shortest-path-to-start d)
               ;; _ (println (:indicator-lights d) "->" (second (:correct-presses path)))
               ]
           path))))

(defn quickest-configuration [input]
  (->> input
       (parse-input)
       (get-configurations)
       (map second)
       (apply +)
       ))

;; 290 -> too low

;; (time (quickest-configuration small-input))
;; "Elapsed time: 2.1455 msecs"
;; 7
;; (time (quickest-configuration large-input))
;; "Elapsed time: 309.1001 msecs"
;; 432


;; Part 2
;; Find the fewest total number of button presses to configure the devices so the
;; joltage numbers match.
(defn add-joltages [joltages to-add]
  (reduce (fn [acc n]
            ;; (println :acc acc :n n :to-flip to-flip :contains (contains? to-flip n))
            (update acc n inc))
          joltages
          to-add))

(defn press-button-joltage
  ([{:keys [joltage-requirements wiring-schematics] :as device} button]
   {:indicator-lights (press-button-joltage joltage-requirements wiring-schematics button)
    :wiring-schematics wiring-schematics})
  ([joltage-requirements  wiring-schematics button]
   (if (or (< button 0) (> button (count wiring-schematics)))
     (throw (Exception. (str ":error-invalid-button: " button)))
     (let [to-add (get wiring-schematics button)]
       (add-joltages joltage-requirements to-add)))))

(defn over-joltage? [joltages joltage-requirements]
  (seq
   (first
    (filter (fn [[a b]] (> a b))
            (map list joltages joltage-requirements)))))

(defn shortest-path-to-joltage [{:keys [joltage-requirements wiring-schematics] :as device}]
  (let [zero-joltages (vec (repeat (count joltage-requirements) 0))
        buttons wiring-schematics
        button-numbers (take (count buttons) (iterate inc 0))]
    (loop [states [[zero-joltages (into {} (map #(vector % 0) button-numbers))]]
           computed-states {zero-joltages []}
           loop-count 0]
      (let [[joltages presses :as current] (first states)
            next-states (map (fn [b]
                               (vector (press-button-joltage joltages wiring-schematics b) (update presses b inc)))
                             button-numbers)
            new-states (remove (fn [[joltages buttons]] (or (over-joltage? joltages joltage-requirements)
                                                            ;; (get computed-states joltages)
                                                            ))
                               next-states)
            match (first (filter (fn [state] (= (first state) joltage-requirements)) new-states))
            ;; new-computed-states (into computed-states new-states)
            ]
        (if (or ;; (get new-computed-states joltage-requirements)
             match
             (> loop-count 10000))
          {:device device
           :correct-presses match
           :states (count states)
           :latest-state (last new-states)
           :loop-count loop-count
           ;; :computed-states computed-states
           }
          (recur (doall (concat (rest states) new-states)) computed-states (inc loop-count)))))))

(defn get-joltage-configurations [devices]
  (map (fn [state]
         [(:joltage-requirements (:device state)) (count (:correct-presses state))])
       (for [d devices] (shortest-path-to-joltage d))))

(defn quickest-configuration-joltage [input]
  (->> input
       (parse-input)
       (get-joltage-configurations)
       ;; (map second)
       ;; (apply +)
       ))

;; 441 -> too low

;; (time (shortest-path-to-joltage (last (parse-input large-input))))

;; It's very slow. I'm thinking A*.

(defn h-sort [pos goal]
  ;; (println "h-sort pos" pos "goal" goal)
  (->>
   (map list goal pos)
   (map #(- (first %) (second %)))
   (apply +)))

(defn shortest-path-to-joltage-a* [{:keys [joltage-requirements wiring-schematics] :as device} max-iterations sort-fn]
  (let [goal joltage-requirements
        buttons wiring-schematics
        button-numbers (take (count buttons) (iterate inc 0))
        start-buttons (vec (repeat (count buttons) 0))
        start-joltages (vec (repeat (count goal) 0))
        start {:button-presses start-buttons :joltages start-joltages}]
    (loop [open-set #{start}
           froms {}
           f-scores {start-joltages (sort-fn start-joltages goal)}
           g-scores {start-joltages 0}
           iterations 0]
      ;; (println "open-set" open-set "froms" froms "f-scores" f-scores "g-scores" g-scores "iterations" iterations)
      (flush)
      (cond (empty? open-set)
            :failure-no-open-nodes
            (= iterations max-iterations)
            (throw (Exception. (str (list :fail-over-time iterations))))
            :else
            (let [{:keys [button-presses joltages] :as current} (first (first (sort-by second (map #(list % (get f-scores (:joltages %))) open-set))))]
              (if (= joltages goal)
                (assoc {} :device device :button-presses button-presses :iterations iterations)
                (let [open-set (into #{} (remove #{current} open-set))
                      ;; _ (println "button-numbers" button-numbers "button-preses" button-presses "current" current)
                      neighbors (map (fn [b]
                                       {:button-presses (update button-presses b inc)
                                        :joltages (press-button-joltage joltages wiring-schematics b)})
                                     button-numbers)
                      ;; _ (println "neighbors 1" neighbors)
                      neighbors (remove (fn [{:keys [button-presses joltages]}] (over-joltage? joltages joltage-requirements)) neighbors)
                      ;; _ (println "current" current "g-score" (get g-scores (:joltages current)) "g-scores" g-scores "neighbors" neighbors)
                      tentative-g-score (inc (get g-scores (:joltages current)))]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                        (reduce (fn [result neighbor]
                                  (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                    {
                                     :froms (assoc (get result :froms) (:joltages neighbor) current)
                                     :g-scores (assoc (get result :g-scores) (:joltages neighbor) tentative-g-score)
                                     :f-scores (assoc (get result :f-scores) (:joltages neighbor) (+ tentative-g-score (sort-fn (:joltages neighbor) goal)))
                                     :open-set (conj (get result :open-set) neighbor)
                                     }
                                    result))
                                {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                                neighbors)]
                    (recur new-open-set new-froms new-f-scores new-g-scores (inc iterations))))))))))

(defn get-joltage-configurations-a* [devices]
  (map (fn [state]
         [(:joltage-requirements (:device state)) (apply + (:button-presses state))])
       (for [d devices] (shortest-path-to-joltage-a* d 200000 h-sort))))

(defn quickest-configuration-joltage-a* [input]
  (->> input
       (parse-input)
       (get-joltage-configurations-a*)
       (map second)
       (apply +)
       ))


;; 1822 - too low!
