(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 7: Amplification Circuit

;; Part 1
;; 

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

(defn op-add [a b c p-modes ip memory inputs outputs]
  [(+ ip 4)
   (assoc memory c (+ (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0))))
   inputs outputs])

(defn op-mul [a b c p-modes ip memory inputs outputs]
  [(+ ip 4)
   (assoc memory c (* (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0))))
   inputs outputs])

(defn op-in [a b c p-modes ip memory inputs outputs]
  [(+ ip 2) (assoc memory a (first inputs)) (vec (rest inputs)) outputs])

(defn op-out [a b c p-modes ip memory inputs outputs]
  [(+ ip 2) memory inputs (conj outputs (get-value a memory (get p-modes 0 0)))])

(defn op-jt [a b c p-modes ip memory inputs outputs]
  [(if (not= 0 (get-value a memory (get p-modes 0 0)))
     (get-value b memory (get p-modes 1 0))
     (+ ip 3))
   memory inputs outputs])

(defn op-jf [a b c p-modes ip memory inputs outputs]
  [(if (= 0 (get-value a memory (get p-modes 0 0)))
     (get-value b memory (get p-modes 1 0))
     (+ ip 3))
   memory inputs outputs])

(defn op-lt [a b c p-modes ip memory inputs outputs]
  [(+ ip 4)
   (assoc memory c (if (< (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))
                     1
                     0))
   inputs outputs])

(defn op-eq [a b c p-modes ip memory inputs outputs]
  [(+ ip 4)
   (assoc memory c (if (= (get-value a memory (get p-modes 0 0)) (get-value b memory (get p-modes 1 0)))
                     1
                     0))
   inputs outputs])

(defn op-ex [a b c p-modes ip memory inputs outputs]
  [-1 memory inputs outputs])

(def ops-by-code {1 op-add
                  2 op-mul
                  3 op-in
                  4 op-out
                  5 op-jt
                  6 op-jf
                  7 op-lt
                  8 op-eq
                  99 op-ex
                  })

(defn do-op [opcode a b c p-modes ip memory inputs outputs]
  ((get ops-by-code opcode) a b c p-modes ip memory inputs outputs))

(defn run-program [program inputs]
  (loop [ip 0
         memory (pad-array program)
         inputs inputs
         outputs []]
    ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
    (let [[operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)
          [new-ip new-memory new-inputs new-outputs] (do-op opcode a b c p-modes ip memory inputs outputs)]
      (if (= new-ip -1)
        new-outputs
        (recur new-ip new-memory new-inputs new-outputs)))))

(defn compute-thruster-signal [program phase-settings]
  (loop [phase-settings phase-settings
         last-output-signal 0]
    (if (empty? phase-settings)
      last-output-signal
      (let [output-signal (first (run-program program [(first phase-settings) last-output-signal]))]
        (recur (rest phase-settings) output-signal)))))

(defn permutations [coll]
  (cond (< (count coll) 2) (list coll)
        (= (count coll) 2) (list coll (reverse coll))
        :else
        (apply concat (for [i (range (count coll))]
                        (map #(concat (list (nth coll i)) %) (permutations (concat (take i coll) (drop (inc i) coll))))))))

(defn compute-max-thruster-signal [program]
  (last (sort-by second (map #(list % (compute-thruster-signal program %)) (permutations [0 1 2 3 4])))))

;; (time (compute-max-thruster-signal (parse-input large-input)))
;; "Elapsed time: 39.8667 msecs"
;; ((0 3 4 2 1) 75228)



;; Part 2
;; Loop the amplifiers. What's the highest signal that can be sent to the thrusters?
(defn run-amplifier [{:keys [name ip memory inputs outputs]}]
  ;; (println "run-amplifier" name inputs)
  (loop [ip ip
         memory memory
         inputs inputs
         outputs outputs]
    ;; (println "loop" ip memory inputs)
    (let [[operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)]
      (if (or (= (get ops-by-code opcode) op-ex)
              (and (= (get ops-by-code opcode) op-in) (empty? inputs)))
        {:name name
         :ip ip
         :memory memory
         :inputs []
         :outputs outputs}
        (let [[new-ip new-memory new-inputs new-outputs] (do-op opcode a b c p-modes ip memory inputs outputs)]
          ;; (println "inputs before/after" (get ops-by-code opcode) inputs new-inputs)
          (recur new-ip new-memory new-inputs new-outputs))))))

(defn running? [{:keys [ip memory]}]
  (not= (get memory ip) 99))

(defn loop-amplifiers [program phase-settings]
  (let [phase-settings (vec phase-settings)
        states (vec (for [i (range (count phase-settings))]
                      {:name (get ["A" "B" "C" "D" "E"] i)
                       :ip 0
                       :memory (pad-array program)
                       :inputs (if (= i 0) [(get phase-settings i) 0] [(get phase-settings i)])
                       :outputs []}))]
    (loop [amp 0 states states previous-output nil i 0]
      (if (not (running? (get states amp)))
        previous-output
        (let [new-state (run-amplifier (get states amp))
              outputs (:outputs new-state)
              next-amp (mod (inc amp) 5)
              new-states (-> states
                             (assoc amp (assoc new-state :outputs []))
                             (assoc next-amp (update (get states next-amp) :inputs #(vec (concat % outputs)))))]
          (recur next-amp new-states (first outputs) (inc i)))))))

(defn compute-max-thruster-signal-2 [program]
  (last (sort-by second (map (fn [settings]
                               ;; (println (vec settings))
                               ;; (println (list settings (loop-amplifiers program settings)))
                               (list settings (loop-amplifiers program settings)))
                             (permutations [5 6 7 8 9])))))

;; (loop-amplifiers [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9 8 7 6 5])
;; 139629729

;; (loop-amplifiers [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9 7 8 5 6])
;; 18216

;; (time (compute-max-thruster-signal-2 (parse-input large-input)))
;; "Elapsed time: 135.0536 msecs"
;; ((6 7 9 5 8) 79846026)
