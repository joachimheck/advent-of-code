(ns day-12.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Run a program written in assembunny code
(def registers #{"a" "b" "c" "d"})

(defn parse-arg [arg]
  (if (or (nil? arg) (registers arg)) arg (Long/parseLong arg)))

(defn read-input [lines]
 (map (fn [[_ cmd arg1 arg2]]
        (let [parsed1 (parse-arg arg1)
              parsed2 (parse-arg arg2)]
          (remove nil? (list cmd parsed1 parsed2))))
      (map #(re-matches #"(\w+) ([\w\d-]+) ?([\w\d-]+)?" %) lines)))

(def test-input
  '("cpy 41 a"
    "inc a"
    "inc a"
    "dec a"
    "jnz a 2"
    "dec a"))

(defn reg-val [state q] (if (number? q) q (get state q)))

(defn run-program [program initial-state]
  (loop [{ip "ip" a "a" b "b" c "c" d "d" :as state} initial-state]
    (if (>= ip (count program))
      (list :done state)
      (let [[inst x y] (nth program ip)]
;;        (println state)
;;        (when (>= (get state "ip") 16) (read-line))
        (recur
         (let [new-state (case inst
                           "cpy" (assoc state y (reg-val state x))
                           "inc" (update state x #(inc %))
                           "dec" (update state x #(dec %))
                           "jnz" (update state "ip" #(if (not= 0 (reg-val state x)) (+ % y -1) %)))]
           (update new-state "ip" inc))
         )))))

;; (run-program (read-input test-input) )
;; => (:done {"ip" 6, "a" 42, "b" 0, "c" 0, "d" 0})

;; (time (run-program (read-input (str/split-lines (slurp "input-12.txt"))) {"ip" 0 "a" 0 "b" 0 "c" 0 "d" 0}))
;; => (:done {"ip" 23, "a" 318007, "b" 196418, "c" 0, "d" 0})
;; "Elapsed time: 8267.1249 msecs"



;; Part 2
;; Start with a different input.

;; (time (run-program (read-input (str/split-lines (slurp "input-12.txt"))) {"ip" 0 "a" 0 "b" 0 "c" 1 "d" 0}))
;; => (:done { "ip" 23, "a" 9227661, "b" 5702887, "c" 0, "d" 0 })
;; "Elapsed time: 147296.4974 msecs"
