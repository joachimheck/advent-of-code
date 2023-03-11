(ns advent-21.core)

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

;; Day 21: Chronal Conversion

;; Part 1
;; What's the lowest initial value for register 1 that causes the program to halt in the fewest instructions?
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

;; (defn unop
;;   "(unknown operation) sets register C to the result of (compute-r3 A B)."
;;   [a b c registers]
;;   (assoc registers c (compute-r3 (get registers a) (get registers b))))

(defn get-fn-name [fn]
  (second (re-matches #".+\$(.+?)\@.+" (str fn))))

(defn run-program [{:keys [ip-register instructions] :as input} initial-registers max-iterations]
  (loop [ip 0
         registers initial-registers
         instructions instructions
         i 0]
    (if (= i max-iterations)
      (get registers 3)
      (if (< -1 ip (count instructions))
        (let [[op a b c :as instruction] (nth instructions ip)
              ip-updated (assoc registers ip-register ip)
              new-registers (op a b c ip-updated)
              new-ip (get new-registers ip-register)]
          ;; (if (= ip 28)
          ;;   (printf "ip=%d %s %s %d %d %d %s\n" ip ip-updated (get-fn-name op) a b c new-registers))
          (recur (inc new-ip) new-registers instructions (if (= ip 28) (inc i) i)))
        registers))))

(defn run-decompiled-program [r0]
  (let [registers [r0 0 0 0 0 0 false]]
    (loop [registers registers]
      (let [registers (bori 3 65536 2 registers)
            registers (seti 1505483 0 3 registers)
            registers (loop [registers registers]
                        (let [registers (bani 2 255 4 registers)
                              registers (addr 3 4 3 registers)
                              registers (bani 3 16777215 3 registers)
                              registers (muli 3 65899 3 registers)
                              registers (bani 3 16777215 3 registers)
                              registers (if (< (get registers 2) 256)
                                          (assoc registers 6 true) ;; (jmp E)
                                          (loop [r4 0 r5 (get registers 5)]
                                            (if (> (get registers 2) r5)
                                              (-> registers
                                                  (assoc 4 r4)
                                                  (assoc 5 r5))
                                              (recur (inc r4) (* r5 256)))))]
                          (if (get registers 6)
                            (assoc registers 6 false)
                            (recur (assoc registers 2 (inc (get registers 4)))))))]
        (if (= (get registers 3) (get registers 0))
          registers
          (recur registers))))))

(defn compute-r3 [r3 r2]
  (bit-and (* (bit-and (+ r3 (bit-and r2 255)) 16777215) 65899) 16777215))

(defn compute-r23 [r2 r3]
  (loop [r2 r2 r3 r3 i 0]
    ;; (println "compute-r23" r2 r3 )
    (if (= i 10)
      :max-iterations
      (let [new-r3 (compute-r3 r3 r2)]
        (if (< r2 256)
          [r2 new-r3]
          (let [new-r2 (inc (quot r2 256))]
            (recur new-r2 new-r3 (inc i))))))))

(defn program [r0]
  (loop [r3 0 i 0]
    (if (= i 10)
      :max-iterations
      (let [r2 (bit-or r3 65536)
            r3 1505483
            [new-r2 new-r3] (compute-r23 r2 r3)]
        (println "comparing" new-r3 "to expected" r0)
        (if (= new-r3 r0)
          new-r3
          (recur new-r3 (inc i)))))))

;; 4662112 ; this is the first r3 my program spits out.
;; ---> answer <---

;; My simulated program doesn't seem to be giving the right answers. I ran the real program,
;; printing out the registers at the end of the big loop, and the first number it printed, 7216956, was the right one.

;; (run-program (parse-input large-input) [0 0 0 0 0 0] 10000)
;; ip=28 [0 28 1 7216956 1 1] eqrr 3 0 4 [0 28 1 7216956 0 1]
;; :max-iterations



;; Part 2
;; What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions?

(defn collect-r3s [{:keys [ip-register instructions] :as input}]
  (loop [ip 0
         registers [0 0 0 0 0 0]
         instructions instructions
         i 0
         r3-values []]
    (if (< -1 ip (count instructions))
      (let [[op a b c :as instruction] (nth instructions ip)
            ip-updated (assoc registers ip-register ip)
            new-registers (op a b c ip-updated)
            new-ip (get new-registers ip-register)]
        (if (= ip 28)
          (let [new-i (inc i)
                new-r3 (get registers 3)]
            (if (= 0 (mod i 16)) (println "i =" i))
            (if (some #{new-r3} r3-values)
              r3-values
              (recur (inc new-ip) new-registers instructions new-i (conj r3-values new-r3))))
          (recur (inc new-ip) new-registers instructions i r3-values)))
      {:state "exited" :registers registers})))

;; ---> answer <---
;; 16747615


;; advent-21.core> (time (collect-r3s (parse-input large-input)))
;; "Elapsed time: 7920207.554 msecs"
;; [7216956
;;  ... (10776 entries) ...
;;  14596916]
