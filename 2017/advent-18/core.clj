(ns advent-18.core)

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

;; Day 18: Duet

;; Part 1
;; What is the value of the frequency the first time the rcv instruction recovers one?
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
        registers (update registers :ip inc)]
    (case op
      "snd" (assoc registers :last-frequency (get-value registers x))
      "set" (assoc registers x (get-value registers y))
      "add" (update registers x #(+ % (get-value registers y)))
      "mul" (update registers x #(* % (get-value registers y)))
      "mod" (update registers x #(mod % (get-value registers y)))
      "rcv" (if (= (get-value registers x) 0)
              registers
              (assoc registers :last-recovered (get registers :last-frequency)))
      "jgz" (if (> (get-value registers x) 0) ; dec because we've already incremented the ip.
              (update registers :ip #(+ % (dec (get-value registers y))))
              registers))))

(defn run-program [program]
  (let [register-names (filter #(not (integer? %)) (distinct (map second program)))
        registers (reduce (fn [acc n] (assoc acc n 0)) {:ip 0} register-names)]
    (loop [registers registers]
      (let [ip (get registers :ip)]
        (if (or (< ip 0) (>= ip (count program))
                (get registers :last-recovered))
          registers
          (recur (execute-instruction (get program ip) registers)))))))

;; (time (run-program (parse-input large-input)))
;; "Elapsed time: 6.4558 msecs"
;; {:ip 26, "i" 126, "a" 2147483647, "p" 1770024601, "b" 4601, "f" 0, :last-frequency 4601, :last-recovered 4601}



;; Part 2
;; Run two processes, communicating with snd and rcv.
(def small-input-2 "small-input-2.txt")

(defrecord DuetState [registers queue other-queue])

(defn execute-instruction-2 [instruction state]
  (let [[op x y] instruction
        registers (:registers state)
        queue (:queue state)
        state (if (or (= op "jgz")
                      (and (= op "rcv") (empty? queue)))
                state
                (update-in state [:registers :ip] inc))
        state (assoc-in state [:registers :waiting] false)
        ]
    (case op
      "set" (assoc-in state [:registers x] (get-value registers y))
      "add" (update-in state [:registers x] #(+ % (get-value registers y)))
      "mul" (update-in state [:registers x] #(* % (get-value registers y)))
      "mod" (update-in state [:registers x] #(mod % (get-value registers y)))
      "jgz" (if (> (get-value registers x) 0)
              (update-in state [:registers :ip] #(+ % (get-value registers y)))
              (update-in state [:registers :ip] inc))
      "snd" (-> state
                (update :other-queue #(conj % (get-value registers x)))
                (update-in [:registers :sent-values] inc))
      "rcv" (if (empty? queue)
              (assoc-in state [:registers :waiting] true)
              (-> state
                  (assoc-in [:registers x] (first queue))
                  (assoc-in [:registers :waiting] false)
                  (update :queue #(vec (rest %)))
                  (update-in [:registers :received-values] inc))))))

(defn run-program-2 [program]
  (let [register-names (filter #(not (integer? %)) (distinct (map second program)))
        register-init {:ip 0 :waiting false :sent-values 0 :received-values 0}
        registers (reduce (fn [acc n] (assoc acc n 0)) register-init register-names)]
    (loop [registers-0 registers
           registers-1 (assoc registers "p" 1)
           queue-0 []
           queue-1 []
           i 0
           last-time (System/currentTimeMillis)]
      (if (= 0 i)
        (println "initial registers" (list registers-0 registers-1)))
      (if (and (> i 0) (= 0 (mod i 1000000)))
        (println "(" (:sent-values registers-1) ")" (float (/ 1000000000 (- (System/currentTimeMillis) last-time))) "instructions per second."))
      (let [ip-0 (get registers-0 :ip)
            ip-1 (get registers-1 :ip)
            running-0? (and (<= 0 ip-0 (dec (count program))) (not (:waiting registers-0)))
            running-1? (and (<= 0 ip-1 (dec (count program))) (not (:waiting registers-1)))]
        ;; (if (integer? (/ (get registers-0 :sent-values) 10000))
        ;;   (print "0"))
        ;; (if (integer? (/ (get registers-1 :sent-values) 10000))
        ;;   (print "1"))
        (if (or (and (not running-0?) (not running-1?))
                (>= (:sent-values registers-1) 3781060))
          (list registers-0 registers-1)
          (let [state-0 (execute-instruction-2 (get program ip-0) (->DuetState registers-0 queue-0 queue-1))
                state-1 (execute-instruction-2 (get program ip-1) (->DuetState registers-1 (:other-queue state-0) (:queue state-0)))]
            (recur (:registers state-0) (:registers state-1) (:other-queue state-1) (:queue state-1) (inc i)
                   (if (and (> i 0) (= 0 (mod i 1000000))) (System/currentTimeMillis) last-time))))))))

;; 254
;; ---> answer <---
;; 3781060

;; Does not finish.
;; (time (run-program-2 (parse-input large-input)))

;; Running all instructions:
;; Processing 132292.62 instructions per second.
;; Skipping rcv with empty queue:
;; Processing 130753.14 instructions per second.
;; About the same.

;; I was changing the queue from a vector to a list by calling rest on it!

;; (time (run-program-2 (parse-input large-input)))
;; initial registers ({:ip 0, f 0, :received-values 0, p 0, :sent-values 0, :waiting false, a 0, i 0, b 0} {:ip 0, f 0, :received-values 0, p 1, :sent-values 0, :waiting false, a 0, i 0, b 0})
;; "Elapsed time: 603.0804 msecs"
;; ({:ip 21, "f" 0, :received-values 6858, "p" -6, :sent-values 6985, :waiting true, "a" 114, "i" 0, "b" 114}
;;  {:ip 21, "f" 0, :received-values 6985, "p" -6, :sent-values 6858, :waiting true, "a" 114, "i" 0, "b" 114})
;; 6858
