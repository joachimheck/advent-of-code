(ns advent-05.core)

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

;; Day 5: Sunny with a Chance of Asteroids

;; Part 1
;; What diagnostic code does the program produce?

(defn parse-input [f]
  (load-string (str/join (list "[" (first (read-lines f)) "]"))))

(defn pad-array [array]
  (let [remainder (rem (count array) 4)]
    (if (= remainder 0)
      array
      (apply conj array (vec (repeat (- 4 remainder) 0))))))

(defn digits [n]
  (if (< n 10)
    [n]
    (let [quotient (quot n 10)
          remainder (rem n 10)]
      (conj (digits quotient) remainder))))

(defn parse-operation [n]
  (let [n-digits (digits n)
        opcode (if (= (count n-digits) 1)
                 n
                 (+ (* 10 (nth n-digits (- (count n-digits) 2))) (last n-digits)))
        p-modes (vec (reverse (drop-last 2 n-digits)))]
    {:opcode opcode
     :p-modes p-modes}))

(defn get-value [n memory p-mode]
  (if (= p-mode 0)
    (get memory n 0)
    n))

(defn op-add [memory a b c p-modes]
  (assoc memory c (+ (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))))

(defn op-mul [memory a b c p-modes]
  (assoc memory c (* (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))))

(defn run-program [program inputs]
  (loop [ip 0
         memory (pad-array program)
         inputs inputs
         outputs []]
    (let [[operation a b c] (subvec memory ip (+ ip 4))
          {:keys [opcode p-modes]} (parse-operation operation)]
      (case opcode
        1 (recur (+ ip 4) (op-add memory a b c p-modes) inputs outputs)
        2 (recur (+ ip 4) (op-mul memory a b c p-modes) inputs outputs)
        3 (recur (+ ip 2) (assoc memory a (first inputs)) (rest inputs) outputs)
        4 (recur (+ ip 2) memory inputs (conj outputs (get-value a memory (get p-modes 0 0))))
        99 outputs
        (list :error-unknown-opcode operation)))))

;; (run-program (parse-input large-input) '(1))
;; [0 0 0 0 0 0 0 0 0 9025675]



;; Part 2
;; After extending the thermal radiators, what is the diagnostic code for system ID 5?

(defn op-jit [ip memory a b p-modes]
  (if (not= 0 (get-value a memory (get p-modes 0 0)))
    (get-value b memory (get p-modes 1 0))
    (+ ip 3)))

(defn op-jif [ip memory a b p-modes]
  (if (= 0 (get-value a memory (get p-modes 0 0)))
    (get-value b memory (get p-modes 1 0))
    (+ ip 3)))

(defn op-let [memory a b c p-modes]
  (assoc memory c (if (< (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))
                    1
                    0)))

(defn op-eql [memory a b c p-modes]
  (assoc memory c (if (= (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))
                    1
                    0)))

(defn run-program-2 [program inputs]
  (loop [ip 0
         memory (pad-array program)
         inputs inputs
         outputs []]
    (let [[operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
            {:keys [opcode p-modes]} (parse-operation operation)]
      ;; (println ip ":" operation a b c "opcode" opcode memory)
        (case opcode
          1 (recur (+ ip 4) (op-add memory a b c p-modes) inputs outputs)
          2 (recur (+ ip 4) (op-mul memory a b c p-modes) inputs outputs)
          3 (recur (+ ip 2) (assoc memory a (first inputs)) (rest inputs) outputs)
          4 (recur (+ ip 2) memory inputs (conj outputs (get-value a memory (get p-modes 0 0))))
          5 (recur (op-jit ip memory a b p-modes) memory inputs outputs)
          6 (recur (op-jif ip memory a b p-modes) memory inputs outputs)
          7 (recur (+ ip 4) (op-let memory a b c p-modes) inputs outputs)
          8 (recur (+ ip 4) (op-eql memory a b c p-modes) inputs outputs)
          99 outputs
          (list :error-unknown-opcode operation)))))

;; (run-program-2 (parse-input large-input) '(5))
;; [11981754]
