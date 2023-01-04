(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 8: Seven Segment Search

;; Part 1
;; How many times do digits 1, 4, 7, or 8 appear in the output values?

(def segments-by-digit
  {0 [\a \b \c \e \f \g]
   1 [\c \f]
   2 [\a \c \d \e \g]
   3 [\a \c \d \f \g]
   4 [\b \c \d \f]
   5 [\a \b \d \f \g]
   6 [\a \b \d \e \f \g]
   7 [\a \c \f]
   8 [\a \b \c \d \e \f \g]
   9 [\a \b \c \d \f \g]})

(def segment-count-by-digit (reduce-kv (fn [result k v] (assoc result k (count v))) {} segments-by-digit))

(defn parse-line [line]
  (let [matches (rest (re-find (re-pattern (str/join (concat
                                                      (repeat 10 "([abcdefg]+) ")
                                                      '("\\| ")
                                                      (repeat 4 "([abcdefg]+) ?"))))
                               line))]
    {:signal-patterns (take 10 matches) :output-value (drop 10 matches)}))

(defn parse-input [f]
  (map parse-line (read-lines f)))

(defn count-1478 [f]
  (let [state (parse-input f)
        outputs (map #(get % :output-value) state)]
    (->> outputs
         (apply concat)
         (map count)
         (filter #{2 3 4 7}) ;; number of segments in digits 1,4, 7, and 8.
         count)))

;; (count-1478 small-input)
;; 26

;; (count-1478 large-input)
;; 387



;; Part 2
;; Determine the sum of the output digits
(defn find-1478-inputs [signal-patterns]
  (->> signal-patterns
         (filter #(some #{2 3 4 7} (list (count %))))
         (map (fn [s] {(get (set/map-invert segment-count-by-digit) (count s)) (set s)}))
         (apply merge)))

;; 6 is the only five-segment digit that doesn't contain both of the segments in 1.
(defn find-six [digits identified-digits]
  (set (first (remove (fn [digit]
                        ;; (println (set one) "subset of" (set digit) "?" (set/subset? (set one) (set digit)))
                        (set/subset? (get identified-digits 1) (set digit))) digits))))

;; 9 is the only five-segment digit that contains all the segments of 4.
(defn find-nine [digits identified-digits]
  (set (first (filter (fn [digit]
                        ;; (println (set one) "subset of" (set digit) "?" (set/subset? (set one) (set digit)))
                        (set/subset? (get identified-digits 4) (set digit))) digits))))

;; 0 is not 6 or 9.
(defn find-zero [digits identified-digits]
  (set (first (remove (fn [digit]
                        (or (= (set digit) (get identified-digits 6))
                            (= (set digit) (get identified-digits 9))))
                      digits))))

;; 3 is the only five-segment digit that contains both the segments in 1.
(defn find-three [digits identified-digits]
  (set (first (filter (fn [digit]
                        ;; (println (set one) "subset of" (set digit) "?" (set/subset? (set one) (set digit)))
                        (set/subset? (get identified-digits 1) (set digit))) digits))))

;; The segments in 5 are a subset of those in 6.
(defn find-five [digits identified-digits]
  (set (first (filter (fn [digit]
                        ;; (println (set one) "subset of" (set digit) "?" (set/subset? (set one) (set digit)))
                        (set/subset? (set digit) (get identified-digits 6))) digits))))

;; 2 is neither 3 nor 5.
(defn find-two [digits identified-digits]
  (set (first (remove (fn [digit]
                        (or (= (set digit) (get identified-digits 3))
                            (= (set digit) (get identified-digits 5))))
                      digits))))


(defn identify-digits [signal-patterns]
  (let [identified-digits (find-1478-inputs signal-patterns)
        five-segments (filter #(= 5 (count %)) signal-patterns)
        six-segments (filter #(= 6 (count %)) signal-patterns)
        identified-digits (assoc identified-digits 6 (find-six six-segments identified-digits))
        identified-digits (assoc identified-digits 9 (find-nine six-segments identified-digits))
        identified-digits (assoc identified-digits 0 (find-zero six-segments identified-digits))
        identified-digits (assoc identified-digits 3 (find-three five-segments identified-digits))
        identified-digits (assoc identified-digits 5 (find-five five-segments identified-digits))
        identified-digits (assoc identified-digits 2 (find-two five-segments identified-digits))]
    identified-digits))

(defn identify-outputs [outputs identified-digits]
  (let [patterns (set/map-invert identified-digits)]
    (map #(list % (get patterns (set %))) outputs)))

(defn output-value [{signal-patterns :signal-patterns output-value :output-value :as entry}]
  (let [identified-digits (identify-digits signal-patterns)
        outputs (identify-outputs output-value identified-digits)]
    (->> outputs
         (map second)
         (map str)
         str/join
         parse-long)))

;; (apply + (map output-value (parse-input small-input)))
;; 61229

;; (apply + (map output-value (parse-input large-input)))
;; 986034
