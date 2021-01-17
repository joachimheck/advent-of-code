(ns day-04.core
  (:import java.security.MessageDigest))

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

;; (def puzzle-input (first  (str/split-lines (slurp "puzzle-input.txt"))))
(def test-input-1 "abcdef")
(def test-input-2 "pqrstuv")
(def puzzle-input "iwrupvqb")

;; Part 1
;; Find the lowest number that yields an MD5 hash starting with five zeroes
;; when the input string and the number are concatenated and hashed.

;; From stack overflow.
(defn md5 [^String s]
    (let [algorithm (MessageDigest/getInstance "MD5")
          raw (.digest algorithm (.getBytes s))]
        (format "%032x" (BigInteger. 1 raw))))



(defn find-adventcoin-number [key length]
  (let [prefix (str/join (repeat length "0"))]
    (first
     (keep (fn [[n hash]] (when (str/starts-with? hash prefix) n))
           (map (fn [i] (list i (md5 (str/join (list key (str i))))))
                (iterate inc 0))))))

;; (time (find-adventcoin-number test-input-1 5))
;; => 609043
;; "Elapsed time: 2808.9281 msecs"

;; (time (find-adventcoin-number test-input-2 5))
;; => 1048970
;; "Elapsed time: 4674.4156 msecs"

;; (time (find-adventcoin-number puzzle-input 5))
;; 346386
;; "Elapsed time: 1547.8155 msecs"



;; Part 2
;; Find one with six leading zeroes.

;; (time (find-adventcoin-number test-input-1 6))
;; => 6742839
;; "Elapsed time: 29266.1157 msecs"

;; (time (find-adventcoin-number test-input-2 6))
;; => 5714438
;; "Elapsed time: 24979.3184 msecs"

;; (time (find-adventcoin-number puzzle-input 6))
;; => 9958218
;; "Elapsed time: 44493.5807 msecs"

