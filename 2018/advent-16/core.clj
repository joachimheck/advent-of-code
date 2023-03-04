(ns advent-16.core)

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

;; Day 16: Chronal Classification

;; Part 1
;; How many samples in the input behave like three or more opcodes?
(defn parse-input-triplet [[before inst after]]
  (let [registers-before (load-string (first (re-find #"(\[\d+, \d+, \d+, \d+\])" before)))
        instruction (load-string (str/join (list "[" inst "]")))
        registers-after (load-string (first (re-find #"(\[\d+, \d+, \d+, \d+\])" after)))]
    {:before registers-before :instruction instruction :after registers-after}))

(defn parse-instruction-effects [lines]
  (->> lines
       (partition-by #(= "" %))
       (remove #(= '("") %))
       (map parse-input-triplet)))

(defn parse-program [lines]
  (->> lines
       (map #(str/join (list "[" % "]")))
       (map load-string)))

(defn parse-input [f]
  (let [lines (read-lines f)
        division-index (java.util.Collections/indexOfSubList lines '("" "" ""))
        program-start (+ 3 division-index)
        effects (take division-index lines)
        program (drop program-start lines)]
    {:effects (parse-instruction-effects effects)
     :program (parse-program program)}))


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

(def operations (list
                 addr
                 addi
                 mulr
                 muli
                 banr
                 bani
                 borr
                 bori
                 setr
                 seti
                 gtir
                 gtri
                 gtrr
                 eqir
                 eqri
                 eqrr))

(defn match-effect [{:keys [before instruction after]}]
  (let [[op-n a b c] instruction]
    (set
     (for [op-f operations
           :let [result (op-f a b c before)]
           :when (= result after)]
       op-f))))

(defn count-triple-opcode-effects [{:keys [effects]}]
  (->> effects
       (map match-effect)
       (map count)
       (filter #(>= % 3))
       count))

;; (count-triple-opcode-effects (parse-input small-input))
;; 1

;; (count-triple-opcode-effects (parse-input large-input))
;; 677



;; Part 2
;; Figure out the opcodes and run the program. What value is in register 0?
(defn determine-opcodes [effects]
  (let [first-matches (loop [opcode-mapping {} effects effects]
                        (if (empty? effects)
                          opcode-mapping
                          (let [effect (first effects)
                                {:keys [before instruction after]} effect
                                [op-n a b c] instruction
                                matches (match-effect effect)]
                            (recur (update opcode-mapping op-n (fn [old-matches] (if old-matches (set/intersection matches old-matches) matches)))
                                   (rest effects)))))]
    (loop [matches first-matches]
      (let [matched-functions (apply set/union (map second (filter (fn [[k v]] (= 1 (count v))) matches)))
            new-matches (reduce-kv (fn [m k v]
                                     (assoc m k (if (= 1 (count v)) v (set (remove matched-functions v)))))
                                   {}
                                   matches)]
        (if (= matches new-matches)
          (into {} (map (fn [[op f-set]] [op (first f-set)]) new-matches))
          (recur new-matches))))))

(defn run-program [{:keys [effects program] :as input}]
  (let [opcodes (determine-opcodes effects)]
    (loop [registers [0 0 0 0] instructions program]
      (if (empty? instructions)
        registers
        (let [[op a b c :as instruction] (first instructions)]
          (if (get opcodes op)
            (recur ((get opcodes op) a b c registers) (rest instructions))
            (println "no opcode matching" instruction opcodes)))))))

;; (time (run-program (parse-input large-input)))
;; "Elapsed time: 6267.044 msecs"
;; [540 2 9 540]
