(ns day-23.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

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

(def test-input
  '("cpy 2 a"
    "tgl a"
    "tgl a"
    "tgl a"
    "cpy 1 a"
    "dec a"
    "dec a"))

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

;; I hand-optimized the input by combining an inc loop into an add and then combining an add loop into a mul.
(def test-input-2
  '("cpy a b"
    "dec b"
    "cpy a d"
    "cpy 0 a"
    "mul b d"
    ;; a = 0; add b to a d times a = b*d
    ;;    "cpy b c"
    ;;    "add a c"   ;; "inc a" "dec c" "jnz c -"
    ;;    "dec d"
    ;;    "jnz d -3"
    "dec b"
    "cpy b c"
    "cpy c d"
    "dec d"
    "inc c"
    "jnz d -2"
    "tgl c"
    "cpy -11 c"
    "jnz 1 c"
    "cpy 97 c"
    "jnz 79 d"
    "inc a"
    "inc d"
    "jnz d -2"
    "inc c"
    "jnz c -5"))

;; (time (run-program (read-input test-input-2) {"ip" 0 "a" 12 "b" 0 "c" 0 "d" 0}))
;; => (:done {"ip" 21, "a" 479009263, "b" 1, "c" 0, "d" 0})
;; "Elapsed time: 105.4068 msecs"

