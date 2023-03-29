(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input [1,9,10,3,2,3,11,0,99,30,40,50])
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 2: 1202 Program Alarm

;; Part 1
;; What value is left at position 0 after the intcode program halts?
(defn parse-input [f]
  (load-string (str/join (list "[" (first (read-lines f)) "]"))))

(defn pad-array [array]
  (let [remainder (rem (count array) 4)]
    (if (= remainder 0)
      array
      (apply conj array (vec (repeat (- 4 remainder) 0))))))

(defn run-program [program]
  (loop [ip 0 memory (pad-array program)]
    (let [[op a b c] (subvec memory ip (+ ip 4))
          [at-a at-b] (mapv #(get memory %) [a b c])]
      (case (get memory ip)
        1 (recur (+ ip 4) (assoc memory c (+ at-a at-b)))
        2 (recur (+ ip 4) (assoc memory c (* at-a at-b)))
        99 memory
        :error-unknown-opcode))))

(defn apply-1202-alarm-state [memory]
  (assoc memory 1 12 2 2))

;; 250635
;; ---> answer <---

;; I hadn't applied the 1202 alarm state.
;; (first (run-program (apply-1202-alarm-state (parse-input large-input))))
;; 4570637



;; Part 2
;; What pair of inputs produces the output 19690720?
(defn find-inputs [program expected]
  (first
   (first
    (filter #(= expected (second %))
            (for [n (range 100)
                  v (range 100)]
              [(+ (* 100 n) v) (first (run-program (assoc program 1 n 2 v)))])))))

;; (time (find-inputs (parse-input large-input) 19690720))
;; "Elapsed time: 185.0725 msecs"
;; 5485
