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
  (loop [program program {ip "ip" a "a" b "b" c "c" d "d" :as state} initial-state]
    (if (>= ip (count program))
      (list :done state)
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
                                     (assoc "d" 0)))]
                 (update new-state "ip" inc)))))))

