(ns advent-19.core)

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

;; Day 19: Go With the Flow

;; Part 1
;; What value is in register 0 when the background process halts?
(defn parse-input [f]
  (let [lines (read-lines f)
        ip-register (parse-long (second (re-matches #"#ip (\d+)" (first lines))))
        instructions (->> (rest lines)
                          (map #(re-find #"([a-z]+) (\d+) (\d+) (\d+)" %))
                          (map rest)
                          (map (fn [[i a b c]] [(load-string i) (parse-long a) (parse-long b) (parse-long c)])))]
    {:ip-register ip-register :instructions instructions}))

(defn addr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c registers]
  (assoc registers c (+ (get registers a) (get registers b))))

(defn addi
  "(add immediate) stores into register C the result of adding register A and value B."
  [a b c registers]
  (assoc registers c (+ (get registers a) b)))

(defn mulr
  "(multiply register) stores into register C the result of multiplying register A and register B."
  [a b c registers]
  (assoc registers c (* (get registers a) (get registers b))))

(defn muli
  "(multiply immediate) stores into register C the result of multiplying register A and value B."
  [a b c registers]
  (assoc registers c (* (get registers a) b)))

(defn banr
  "(bitwise AND register) stores into register C the result of the bitwise AND of register A and register B."
  [a b c registers]
  (assoc registers c (bit-and (get registers a) (get registers b))))

(defn bani
  "(bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B."
  [a b c registers]
  (assoc registers c (bit-and (get registers a) b)))

(defn borr
  "(bitwise OR register) stores into register C the result of the bitwise OR of register A and register B."
  [a b c registers]
  (assoc registers c (bit-or (get registers a) (get registers b))))

(defn bori
  "(bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B."
  [a b c registers]
  (assoc registers c (bit-or (get registers a) b)))

(defn setr
  "(set register) copies the contents of register A into register C. (Input B is ignored.)"
  [a b c registers]
  (assoc registers c (get registers a)))

(defn seti
  "(set immediate) stores value A into register C. (Input B is ignored.)"
  [a b c registers]
  (assoc registers c a))

(defn gtir
  "(greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (> a (get registers b)) 1 0)))

(defn gtri
  "(greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (> (get registers a) b) 1 0)))

(defn gtrr
  "(greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (> (get registers a) (get registers b)) 1 0)))

(defn eqir
  "(equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (= a (get registers b)) 1 0)))

(defn eqri
  "(equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (= (get registers a) b) 1 0)))

(defn eqrr
  "(equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0."
  [a b c registers]
  (assoc registers c (if (= (get registers a) (get registers b)) 1 0)))

(defn get-fn-name [fn]
  (second (re-matches #".+\$(.+?)\@.+" (str fn))))

(defn run-program [{:keys [ip-register instructions] :as input} initial-registers]
  (loop [ip 0
         registers initial-registers
         instructions instructions
         i 0]
    (if (< -1 ip (count instructions))
      (let [[op a b c :as instruction] (nth instructions ip)
            ip-updated (assoc registers ip-register ip)
            new-registers (op a b c ip-updated)
            new-ip (get new-registers ip-register)]
        (printf "ip=%d %s %s %d %d %d %s\n" ip ip-updated (get-fn-name op) a b c new-registers)
        (recur (inc new-ip) new-registers instructions (inc i)))
      registers)))

;; (time (run-program (parse-input small-input) [0 0 0 0 0 0]))
;; "Elapsed time: 0.8141 msecs"
;; [6 5 6 0 0 9]

;; (time (run-program (parse-input large-input) [0 0 0 0 0 0]))
;; "Elapsed time: 17149.4328 msecs"
;; [1872 1 1030 1031 1031 256]



;; Part 2
;; What about if register 0 starts with the value 1?
(defn profile-program [{:keys [ip-register instructions] :as input} initial-registers max-iterations]
  (loop [ip 0
         registers initial-registers
         instructions instructions
         i 0
         ip-record []]
    (if (and (< -1 ip (count instructions)) (< i max-iterations))
      (let [[op a b c :as instruction] (nth instructions ip)
            ip-updated (assoc registers ip-register ip)
            new-registers (op a b c ip-updated)
            new-ip (get new-registers ip-register)]
        ;; (printf "ip=%d %s %s %d %d %d %s\n" ip ip-updated (get-fn-name op) a b c new-registers)
        (recur (inc new-ip) new-registers instructions (inc i) (conj ip-record ip)))
      {:registers registers
       :ip-record ip-record})))


;; 10551452
;; ---> answer <---
;; 19105632 ; sum of factors

(def test-input "test-input.txt")

;; idea 1: this is computing a number in r2 and outputing that number.
;; idea 2: output the sum of the factors of r2 (10551452)
(defn find-factors [n]
  (loop [i 1 factors []]
    (if (> i n)
      factors
      (recur (inc i) (if (= 0 (rem n i)) (conj factors i) factors)))))

;; The idea was right but I had incorrectly simplified the math. With the right number in r2, this works!

;; (time (apply + (find-factors 10551430)))
;; "Elapsed time: 377.3747 msecs"
;; 18992592
