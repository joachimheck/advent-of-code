(ns advent-17.core)

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

;; Day 17: Spinlock

;; Part 1
;; What will be the value in the circular buffer after the last inserted value?
(def small-steps 3)
(def large-steps 359)

(defn execute-spinlock [step-size step-count]
  (loop [buffer [0] position 0 step 1]
    (if (> step step-count)
      {:buffer buffer :position position}
      (let [new-position (inc (mod (+ position step-size) (count buffer)))]
        (recur (vec (concat (subvec buffer 0 new-position) [step] (subvec buffer new-position)))
               new-position
               (inc step))))))

(defn get-value-after-last [step-size]
  (let [{:keys [buffer position]} (execute-spinlock step-size 2017)]
    (get buffer (inc position))))

;; (time (get-value-after-last small-steps))
;; "Elapsed time: 571.4861 msecs"
;; 638

;; (time (get-value-after-last large-steps))
;; "Elapsed time: 570.0649 msecs"
;; 1506



;; Part 2
;; Get the value after zero, after 50,000,000 iterations.
(defn get-value-after-zero [step-size iterations]
  (loop [buffer-size 1 position 0 value-after-zero nil i 1]
    (if (> i iterations)
      value-after-zero
      (let [new-position (inc (mod (+ position step-size) buffer-size))
            new-value (if (= new-position 1) i value-after-zero)]
        (recur (inc buffer-size) new-position new-value (inc i))))))

;; (time (get-value-after-zero large-steps 50000000))
;; "Elapsed time: 9524.629 msecs"
;; 39479736
