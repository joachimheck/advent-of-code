(ns advent-8.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "/home/runner/advent-8/small-input.txt")
(def large-input "/home/runner/advent-8/large-input.txt")

(defrecord Instruction [operand argument])

(defn read-code
  "Returns a vecxtor representing the instructions."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (->> (doall (line-seq rdr))
         (map #(->Instruction
                (first (str/split % #" "))
                (Integer/parseInt (second (str/split % #" ")))))
         vec)))

;; Get the value of the accumulator when the code loops.
(defn execute
  "Execute the program until hitting a loop, then return the current value of acc."
  ([code] (execute code 0 0 []))
  ([code acc ip prev]
   (cond (some #{ip} prev) (list "loop" acc)
         (or (< ip 0) (> ip (count code))) (list "exit" acc)
         :else (let [instruction (get code ip)
                     operand (:operand instruction)
                     argument (:argument instruction)
                     new-ip (if (= operand "jmp")
                               (+ ip argument)
                               (inc ip))
                     new-acc (if (= operand "acc")
                               (+ acc argument)
                               acc)
                     new-prev (conj prev ip)]
;                  (println instruction acc)
                  (execute code new-acc new-ip new-prev)))))




;; Replace one NOP with a JMP or vice versa, until the code does not loop - report acc.
(defn get-until
  ([code operands]
   (cond (empty? code) []
         (seq ((set operands) (:operand (first code)))) '()
         :else (flatten (list (vector (first code)) (get-until (rest code) operands)))))
  ([code start-pos operands]
   (let [start-code (subvec code 0 start-pos)
         rest-code (subvec code start-pos)]
     (flatten (list start-code (get-until rest-code operands))))))

(defn swap [inst]
  (cond (= (:operand inst) "jmp") (->Instruction "nop" (:argument inst))
        (= (:operand inst) "nop") (->Instruction "jmp" (:argument inst))
        :else inst))

(defn swap-jmp-nop [code start-pos]
  (let [before-inst (get-until code start-pos '("jmp" "nop"))
        inst (first (subvec code (count before-inst)))
        after (rest (subvec code (count before-inst)))
        result (vec (flatten (list before-inst (swap inst) after)))]
;    (println "code" code)
;    (println "result" result)
    result))

(def ^:dynamic code '())

(defn fix-loop [start-pos]
  (print ".")
;  (println start-pos code)
  (let [insts-before (count (get-until code start-pos '("jmp" "nop")))]
    (if (>= insts-before (count code)) nil
      (let [execute-result (execute (swap-jmp-nop code start-pos))]
        (if (= (first execute-result) "exit")
          (second execute-result)
          (fix-loop (+ 1 insts-before)))))))
            
(defn fix-loop-in [f]
  (def code (read-code f))
  (fix-loop 0))
