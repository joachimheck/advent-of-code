(ns day-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-input
  (list
   "\"\""
   "\"abc\""
   "\"aaa\\\"aaa\""
   "\"\\x27\""))

(def puzzle-input (str/split-lines (slurp "puzzle-input.txt")))


;; Part 1
;; # code characters - #memory characters

(defn count-string-chars [input]
  (reduce + (map count input)))

(defn count-memory-chars [input]
  (->> input
       (map #(subs % 1 (dec (count %))))
       (map (fn [s] (str/replace s "\\\\" "\\")))
       (map (fn [s] (str/replace s "\\\"" "\"")))
       (map (fn [s] (str/replace s #"\\x[0-9|a-f]{2}" "•")))
       (map count)
       (reduce +)))

(defn diff-char-counts [input]
  (- (count-string-chars input) (count-memory-chars input)))

;; (time (diff-char-counts puzzle-input))
;; => 1333
;; "Elapsed time: 0.8633 msecs"



;; Part 2
;; Encoding.
(defn count-encoded-chars [input]
  (->> input
       (map (fn [s] (str/replace s "\\" "\\\\")))
       (map (fn [s] (str/replace s "\"" "\\\"")))
       (map (fn [s] (str "\"" s "\"")))
       (map count)
       (reduce +)))

(defn diff-char-counts-2 [input]
  (- (count-encoded-chars input) (count-string-chars input)))

;; (time (diff-char-counts-2 puzzle-input))
;; => 2046
;; "Elapsed time: 1.1487 msecs"
