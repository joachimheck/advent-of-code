(ns advent-23.core)

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

;; Day 23: Coprocessor Conflagration

;; Part 1
;; How many times is the mul instruction invoked?

;; This code is adapted from day 18.
(defn parse-input [f]
  (->> f
       read-lines
       (map #(re-find #"([a-z]{3}) (.+?) ?(.+)?" %))
       (map rest)
       (map #(remove nil? %))
       (map #(map (fn [s] (if (re-matches #"[-\d]+" s) (parse-long s) s)) %))
       vec))

(defn get-value [registers x]
  (if (integer? x)
    x
    (get registers x 0)))

(defn execute-instruction [instruction registers]
  (let [[op x y] instruction
        current-ip (get registers :ip)
        registers (if (= op "jnz")
                    registers
                    (update registers :ip inc))
        registers (if (= op "mul")
                    (update registers :mul-count inc)
                    registers)]
    ;; (println "executing" current-ip op x y "next ip" (get registers :ip))
    (case op
      "set" (assoc registers x (get-value registers y))
      "add" (update registers x #(+ % (get-value registers y)))
      "sub" (update registers x #(- % (get-value registers y)))
      "mul" (update registers x #(* % (get-value registers y)))
      "jnz" (if (not= (get-value registers x) 0)
              (update registers :ip #(+ % (get-value registers y)))
              registers))))

(defn run-program [program]
  (let [register-names (filter #(not (integer? %)) (distinct (map second program)))
        registers (reduce (fn [acc n] (assoc acc n 0)) {:ip 0 :mul-count 0} register-names)]
    (loop [registers registers]
      (let [ip (get registers :ip)]
        (if (or (< ip 0) (>= ip (count program))
                (get registers :last-recovered))
          registers
          (recur (execute-instruction (get program ip) registers)))))))

;; (run-program (parse-input large-input))
;; {"d" 65, :mul-count 3969, :ip 32, "f" 0, "e" 65, "a" 0, "b" 65, "g" 0, "h" 1, "c" 65}



;; Part 2
;; Fix the bug. With register a starting at 1, what value is left in register h?
(defn execute-instruction-2 [instruction registers]
  (let [[op x y] instruction
        current-ip (get registers :ip)
        registers (update-in registers [:profile current-ip] inc)
        registers (if (= op "jnz")
                    registers
                    (update registers :ip inc))
        registers (if (= op "mul")
                    (update registers :mul-count inc)
                    registers)]
    ;; (println "executing" current-ip op x y "next ip" (get registers :ip))
    (if (= current-ip 15)
      (println "instruction 15" (dissoc registers :profile)))
    (case op
      "set" (assoc registers x (get-value registers y))
      "add" (update registers x #(+ % (get-value registers y)))
      "sub" (update registers x #(- % (get-value registers y)))
      "mul" (update registers x #(* % (get-value registers y)))
      "jnz" (if (not= (get-value registers x) 0)
              (update registers :ip #(+ % (get-value registers y)))
              (update registers :ip inc)))))

(def initial-profile (reduce (fn [acc i] (assoc acc i 0)) (sorted-map) (range 32)))

(defn run-program-2 [program max-steps]
  (let [register-names (filter #(not (integer? %)) (distinct (map second program)))
        registers (reduce (fn [acc n] (assoc acc n 0)) {:ip 0 :mul-count 0 :profile initial-profile} register-names)
        registers (assoc registers "a" 1)]
    (loop [registers registers i 0]
      (let [ip (get registers :ip)
            old-h (get registers "h")]
        (if (or (< ip 0) (>= ip (count program))
                (get registers :last-recovered)
                (= i max-steps))
          registers
          (let [new-registers (execute-instruction-2 (get program ip) registers)
                new-h (get new-registers "h")]
            (if (not= old-h new-h)
              (printf "Register h set to %d on instruction %d" new-h i))
            (recur new-registers (inc i))))))))

;; ---> answer <---
;; 1000
;; 84 is incorrect.

(defn prime? [x]
  (if (and (even? x) (> x 2))
    false
    (let [sqrt (int (inc (Math/sqrt x)))
          factors (for [i (range 2 (inc sqrt))
                        j (range 2 (int (inc (/ x i))))
                        :when (= x (* i j))]
                    [i j])]
      (empty? factors))))

(defn count-primes [start end step]
  (loop [x start
         prime-count 0
         non-prime-count 0]
    (if (> x end)
      {:prime prime-count :non-prime non-prime-count}
      (recur (+ x step)
             (if (prime? x) (inc prime-count) prime-count)
             (if (prime? x) non-prime-count (inc non-prime-count))))))

;; The program is counting non-primes between two numbers.
;; (time (count-primes 106500 123500 17))
;; "Elapsed time: 36972.4098 msecs"
;; {:prime 84, :non-prime 917}
