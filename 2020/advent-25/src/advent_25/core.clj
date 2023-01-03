(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

;; Card public key / door public key.
(def small-input '(5764801 17807724))
(def large-input '(15628416 11161639))
(def divisor 20201227)

;; Part 1
;; Compute the encryption key for the hotel room door.

(defn transform [subject-number x]
  (mod (* subject-number x) divisor))

(defn find-loop-size [public-key]
  (count (take-while #(not= (first %) public-key) (partition 2 1 (iterate (partial transform 7) 1)))))

(defn find-encryption-key [[card-public-key door-public-key]]
  (nth (iterate (partial transform door-public-key) 1) (find-loop-size card-public-key)))

;; (time (find-encryption-key small-input))
;; => 14897079
;; "Elapsed time: 0.0565 msecs"

(time (find-encryption-key large-input))
;; => 19774660
;; "Elapsed time: 12440.8603 msecs"
