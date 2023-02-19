(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 25: The Halting Problem

;; Part 1
;; What is the diagnostic checksum of the Turing tape after the machine has run?
(defn parse-init [[state-s steps-s]]
  {:start-state (first (second (re-find #"Begin in state (.)." state-s)))
   :steps (parse-long (second (re-find #"Perform a diagnostic checksum after (\d+) steps." steps-s)))})

(defn parse-instructions [[current write move continue :as params]]
  {(parse-long (second (re-find #"If the current value is (\d+):" current)))
   {:write (parse-long (second (re-find #"Write the value (\d+)." write)))
    :move (second (re-find #"Move one slot to the ([a-z]+)." move))
    :continue (first (second (re-find #"Continue with state (.)." continue)))}})

(defn parse-state [state]
  (let [name (first (second (re-find #"In state (.)." (first state))))]
    {name (apply merge (map parse-instructions (vec (partition 4 (rest state)))))}))

(defn parse-input [f]
  (let [[init & states] (->> f
              (read-lines)
              (partition-by #(= % ""))
              (remove #(= % '(""))))]
    (apply merge (concat (list (parse-init init)) (map parse-state states)))))

(defn print-tape [tape]
  (str (vals tape)))

(defn run-program [program]
  (loop [step 0 cursor 0 state (:start-state program) tape (sorted-map)]
    (if (= step (:steps program))
      tape
      (let [current (get tape cursor 0)
            {:keys [write move continue]} (get-in program [state current])]
        (recur (inc step)
               (if (= "right" move) (inc cursor) (dec cursor))
               continue
               (assoc tape cursor write))))))

(defn diagnostic-checksum [tape]
  (apply + (vals tape)))

;; (time (diagnostic-checksum (run-program (parse-input small-input))))
;; "Elapsed time: 0.842 msecs"
;; 3

;; (time (diagnostic-checksum (run-program (parse-input large-input))))
;; "Elapsed time: 24064.5997 msecs"
;; 2794
