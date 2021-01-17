(ns day-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

;; Part 1
(def test-input
  '("123 -> x"
    "456 -> y"
    "x AND y -> d"
    "x OR y -> e"
    "x LSHIFT 2 -> f"
    "y RSHIFT 2 -> g"
    "NOT x -> h"
    "NOT y -> i"))

(defn wire-input [s]
  (if (re-matches #"\d" s)
    (Long/parseLong s)
    (symbol s)))

(defn parse-line [line]
  (let [val-match (re-matches #"(\d+) -> (\w+)" line)
        not-match (re-matches #"NOT (\w+) -> (\w+)" line)
        shift-match (re-matches #"(\w+) (\w+) (\d+) -> (\w+)" line)
        gate-match (re-matches #"(\w+) (\w+) (\w+) -> (\w+)" line)
        wire-match (re-matches #"(\w+) -> (\w+)" line)]
    (cond val-match
          (let [[_ v w] val-match]
            (assoc {} :type :value :in (list (Long/parseLong v)) :out (wire-input w)))
          not-match
          (let [[_ w1 w2] not-match]
            (assoc {} :type :not :in (list (wire-input w1)) :out (wire-input w2)))
          shift-match
          (let [[_ w1 dir amt w2] shift-match]
            (assoc {} :type :shift :dir dir :amt (Long/parseLong amt) :in (list (wire-input w1)) :out (wire-input w2)))
          gate-match
          (let [[_ w1 gate w2 w3] gate-match]
            (assoc {} :type :gate :gate gate :in (list (wire-input w1) (wire-input w2)) :out (wire-input w3)))
          wire-match
          (let [[_ w1 w2] wire-match]
            (assoc {} :type :wire :in (list (wire-input w1)) :out (wire-input w2)))
          )))

(defn read-input [f]
  (map parse-line (str/split-lines (slurp f))))

;; initialize signals from :value type instructions
;; compute the signals that we have the input signals for
;; remove the computed instructions from the unprocessed list
;; repeat

(defn get-input [signals input]
  (if (number? input) input
      (get signals input)))

(defn process-instruction [signals instruction]
  (let [[in1 in2 :as ins] (:in instruction)]
    ;; (println instruction in1 "=>" (get signals in1) in2 "=>" (get signals in2))
    (assoc {} (:out instruction)
           (mod
            (case (:type instruction)
              :value (first (:in instruction))
              :gate
              (case (:gate instruction)
                "AND" (bit-and (get-input signals in1) (get-input signals in2))
                "OR" (bit-or (get-input signals in1) (get-input signals in2)))
              :shift
              (case (:dir instruction)
                "LSHIFT" (bit-shift-left (get-input signals in1) (:amt instruction))
                "RSHIFT" (bit-shift-right (get-input signals in1) (:amt instruction)))
              :not (bit-not (get-input signals in1))
              :wire (get-input signals in1))
            65536))))

(defn processable? [signals instruction]
  (let [available (set (keys signals))]
    (or (= :value (:type instruction))
        (every? available (filter symbol? (:in instruction))))))

(defn process-instruction-list
  ([instructions] (process-instruction-list {} instructions))
  ([signals instructions]
   (if (empty? instructions) signals
       (let [processable (filter (partial processable? signals) instructions)
             processed (apply merge (map (partial process-instruction signals) processable))]

         (if (empty? processed) (list signals instructions)
             (process-instruction-list
              (merge signals processed)
              (remove (set processable) instructions)))))))

;; (time (process-instruction-list (map parse-line test-input)))
;; ([d 72] [e 507] [f 492] [g 114] [h 65412] [i 65079] [x 123] [y 456])
;; "Elapsed time: 1.3177 msecs"

;; (time (get (process-instruction-list (read-input "puzzle-input.txt")) 'a))
;; => 3176
;; "Elapsed time: 740.2196 msecs"



;; Part 2
;; Send the previous signal coming from wire a to wire b.

;; (time (get (process-instruction-list (read-input "puzzle-input-2.txt")) 'a))
;; 14710
;; "Elapsed time: 745.4106 msecs"
