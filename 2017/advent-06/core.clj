(ns advent-06.core)

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

;; Day 6: Memory Reallocation

;; Part 1
;; How many redistribution cycles must be completed before seeing a configuration again?
(defn parse-input [f]
  (let [line (first (read-lines f))]
    (load-string (str/join (list "[" line "]")))))

(defn find-largest [banks]
  (first
   (reduce (fn [[acc-i acc-v] [i v]]
             (cond (> v acc-v) [i v]
                   (= v acc-v) (if (< i acc-i) [i v] [acc-i acc-v])
                   (< v acc-v) [acc-i acc-v]))
           [0 0]
           (map-indexed list banks))))

(defn redistribute [banks amount start-index]
  (loop [banks banks amount amount index start-index]
    (if (= 0 amount)
      banks
      (recur (update banks index inc)
             (dec amount)
             (mod (inc index) (count banks))))))

(defn cycle-banks [banks]
  (let [largest-index (find-largest banks)
        largest-value (get banks largest-index)
        without-largest (assoc banks largest-index 0)]
    (redistribute without-largest largest-value (mod (inc largest-index) (count banks)))))

(defn redistribute-until-repeat [banks]
  (loop [banks banks seen #{} cycles 0]
    (if (some seen (list banks))
      {:cycles cycles}
      (recur (cycle-banks banks)
             (conj seen banks)
             (inc cycles)))))

;; (time (redistribute-until-repeat (parse-input small-input)))
;; "Elapsed time: 1.0474 msecs"
;; {:cycles 5}

;; (time (redistribute-until-repeat (parse-input large-input)))
;; "Elapsed time: 172.948 msecs"
;; {:cycles 7864}



;; Part 2
;; How many cycles are in the infinite loop?
(defn redistribute-get-cycle-size [banks]
  (loop [banks banks seen #{} seen-list '() cycles 0]
    (if (some seen (list banks))
      (let [seen-list (conj seen-list banks)]
        {:cycle-size (abs (apply - (map first (filter (fn [[i b]] (= b banks)) (map-indexed list seen-list)))))})
      (recur (cycle-banks banks)
             (conj seen banks)
             (conj seen-list banks)
             (inc cycles)))))

;; (time (redistribute-get-cycle-size (parse-input small-input)))
;; "Elapsed time: 1.0359 msecs"
;; {:cycle-size 4}

;; (time (redistribute-get-cycle-size (parse-input large-input)))
;; "Elapsed time: 175.5932 msecs"
;; {:cycle-size 1695}
