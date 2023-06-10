(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])
(require '[clojure.math.numeric-tower :as math :exclude [abs]])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 25: Clock Signal

;; Part 1
;; 

(def registers #{"a" "b" "c" "d"})

(defn parse-arg [arg]
  (if (or (nil? arg) (registers arg)) arg (Long/parseLong arg)))

(defn read-input [lines]
 (map (fn [[_ cmd arg1 arg2]]
        (let [parsed1 (parse-arg arg1)
              parsed2 (parse-arg arg2)]
          (remove nil? (list cmd parsed1 parsed2))))
      (map #(re-matches #"(\w+) ([\w\d-]+) ?([\w\d-]+)?" %) lines)))

(defn parse-input [f]
  (read-input (read-lines f)))

(defn reg-val [state q] (if (number? q) q (get state q)))

(defn toggle-instruction [[i x y]]
  (if y
    (if (= "jnz" i) (list "cpy" x y) (list "jnz" x y))
    (if (= "inc" i) (list "dec" x) (list "inc" x))))

(defn toggle [program state arg]
;; (println "toggle" program state arg (get state "ip") (reg-val state arg))
  (let [ip (get state "ip")
        toggle-ip (+ ip (reg-val state arg))]
    (if (<= 0 toggle-ip (dec (count program)))
      (let [before (take toggle-ip program)
            after (drop (inc toggle-ip) program)
            inst (nth program toggle-ip)
            toggled (list (toggle-instruction inst))]
        (concat before toggled after))
      program)))

(defn run-program [program initial-state]
  (loop [program program
         {ip "ip" a "a" b "b" c "c" d "d" output "output" :as state} initial-state]
    (cond (>= ip (count program))
          (list :done state)
          (or (>= (count output) 30)
              (not= output (vec (take (count output) (flatten (repeat '(0 1)))))))
          (get state "output")
          :else
          (let [[inst x y] (nth program ip)]
            (if (= "tgl" inst) (println program state))
            ;;        (when (>= (get state "ip") 16) (read-line))
            (recur (if (= "tgl" inst) (toggle program state x) program)
                   (let [new-state (case inst
                                     "cpy" (if (number? y) state (assoc state y (reg-val state x)))
                                     "inc" (if (number? x) state (update state x #(inc %)))
                                     "dec" (if (number? x) state (update state x #(dec %)))
                                     "jnz" (update state "ip" #(if (not= 0 (reg-val state x))
                                                                 (+ % (reg-val state y) -1)
                                                                 %))
                                     "tgl" state
                                     "add" (update state x #(+ % (reg-val state y)))
                                     "mul" (-> state
                                               (assoc "a" (* (reg-val state x) (reg-val state y)))
                                               (assoc "d" 0))
                                     "out" (assoc state "output" (conj output (reg-val state x))))]
                     (update new-state "ip" inc)))))))

(defn generate-signal [program input]
  (run-program program {"ip" 0 "a" input "b" 0 "c" 0 "d" 0 "output" []}))

(defn find-clock-signal [program]
  (loop [i 0]
    (if (= 0 (mod i 100))
      (println i))
    (let [signal (generate-signal (parse-input large-input) i)]
      (if (> (count signal) 3)
        (println i signal))
      (if (= (count signal) 30)
        (list i signal)
        (recur (+ i 2))))))

;; (time (find-clock-signal (parse-input large-input)))
;; 0
;; 4 [0 1 0 1 1]
;; 12 [0 1 0 0]
;; 20 [0 1 0 1 0 0]
;; 28 [0 1 0 0]
;; 36 [0 1 0 1 1]
;; 44 [0 1 0 0]
;; 52 [0 1 0 1 0 1 0 0]
;; 60 [0 1 0 0]
;; 68 [0 1 0 1 1]
;; 76 [0 1 0 0]
;; 84 [0 1 0 1 0 0]
;; 92 [0 1 0 0]
;; 100
;; 100 [0 1 0 1 1]
;; 108 [0 1 0 0]
;; 116 [0 1 0 1 0 1 1]
;; 124 [0 1 0 0]
;; 132 [0 1 0 1 1]
;; 140 [0 1 0 0]
;; 148 [0 1 0 1 0 0]
;; 156 [0 1 0 0]
;; 164 [0 1 0 1 1]
;; 172 [0 1 0 0]
;; 180 [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1]
;; "Elapsed time: 24233.0465 msecs"
;; (180 [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1])
