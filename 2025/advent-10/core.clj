(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
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

(defn parse-wiring-schematic [s]
  (->> s
       (re-seq #"\([\d\,]+\)")
       (map (fn [ns] (re-seq #"\d" ns)))
       (mapv #(map parse-long %))))

(defn parse-indicator-lights [s]
  (vec
   (for [c (re-find #"[.#]+" s)]
     (if (= c \.) 0 1))))

(defn parse-line [line]
  (let [[indicator-lights wiring-schematics joltage-requirements]
        (rest (re-matches #"(\[[.#]+\]) (.+) (\{.+\})" line))]
    {:indicator-lights (parse-indicator-lights indicator-lights)
     :wiring-schematics (parse-wiring-schematic wiring-schematics)
     :joltage-requirements joltage-requirements}))

(defn parse-input [input]
  (->> input
       (read-lines)
       (map parse-line)))


;; Part 1
;; Find the fewest total number of button presses to configure the devices so the
;; indicator lights match.
(defn flip-lights [lights to-flip]
  (reduce (fn [acc tf]
            (if (or (< tf 0) (> tf (dec (count lights))))
              (throw (Exception. (str ":error-invalid-flip: " to-flip)))
              (update acc tf (fn [l] (if (= l 0) 1 0)))))
          lights
          to-flip))

(defn press-button [{:keys [indicator-lights wiring-schematics] :as device} button]
  (if (or (< button 0) (> button (count wiring-schematics)))
    (throw (Exception. (str ":error-invalid-button: " button)))
    (let [to-flip (get wiring-schematics button)]
      (assoc device :indicator-lights (flip-lights indicator-lights to-flip)))))

(defn shortest-path-to-start [{:keys [indicator-lights wiring-schematics] :as device}]
  (let [initial-lights (vec (repeat (count indicator-lights) 0))
        initial-device (assoc device :indicator-lights initial-lights)
        buttons wiring-schematics]
    (loop [states [[initial-device []]]
           loop-count 0]
      (let [new-states (mapcat (fn [[device presses :as state]]
                              state
                              (map (fn [b]
                                     (vector (press-button device b) (conj presses b)))
                                   (take (count buttons) (iterate inc 0))))
                            states)
            ;; _ (println "new-states" new-states)
            matches (filter (fn [state] (= indicator-lights (:indicator-lights (first state)))) new-states)]
        (if (or (seq matches)
                (> loop-count 200))
          {:match (first matches)
           :states (count new-states)}
          (recur new-states (inc loop-count)))))))

(defn quickest-configuration [input]
  (let [devices (parse-input input)]
    (apply +
           (map count
                (map (fn [state]
                       (second (:match state)))
                     (for [d devices]
                       (let [path (shortest-path-to-start d)
                             _ (println (:indicator-lights d) "->" (second (:match path)))]
                         path)))))))

;; TODO: fix out of memory
;; (shortest-path-to-start (parse-line "[..#.#..##.] (0,2,3,4,8,9) (1,3,5,6,7,9) (2,5,6,7,9) (0,1,3,5,8,9) (3,5,9) (0,1,4,7,8,9) (0,2,7,9) (4,5,9) (0,4,5,8,9) {57,30,38,67,47,89,27,30,57,112}"))
