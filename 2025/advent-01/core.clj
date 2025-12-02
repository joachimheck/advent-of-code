(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (->> (read-lines input)
       (map #(re-matches #"([LR])(\d+)" %))
       (map rest)
       (map #(list (first %) (parse-long (second %))))))

;; Part 1
;; How many times does the dial end pointing at 0?
(defn count-zeroes [input]
  (loop [moves (parse-input input)
         zeroes 0
         pos 50]
    (let [[dir num] (first moves)]
      ;; (println "dir" dir "num" num)
      (if (empty? moves)
        zeroes
        (let [newpos (mod (if (= dir "R") (+ pos num) (- pos num)) 100)]
          (recur (rest moves) (if (zero? newpos) (inc zeroes) zeroes) newpos))))))



;; (time (count-zeroes small-input))
;; "Elapsed time: 0.4961 msecs"
;; 3
;; (time (count-zeroes large-input))
;; "Elapsed time: 4.6038 msecs"
;; 1029


;; Part 2
;; Count how many times the dial passes zero.
(defn count-all-zeroes [input]
  (loop [moves (parse-input input)
         zeroes 0
         pos 50]
    (let [[dir num] (first moves)]
      ;; (println "dir" dir "num" num)
      (if (empty? moves)
        zeroes
        (let [newpos (mod (if (= dir "R") (+ pos num) (- pos num)) 100)
              revs (quot num 100)
              remainder (if (> revs 0) (rem num (* 100 revs)) num)
              extra-zero (if (or (and (= dir "R") (not= pos 0) (> (+ pos remainder) 100))
                                 (and (= dir "L") (not= pos 0) (< (- pos remainder) 0))
                                 (= newpos 0))
                           1 0)]
          ;; (println "pos" pos "dir" dir "num" num "revs" revs "remainder" remainder "extra-zero" extra-zero "newpos" newpos "zeroes" zeroes)
          ;; (println "pos" pos dir num "revs" revs "extra-zero" extra-zero "newpos" newpos "zeroes" (+ zeroes revs extra-zero))
          (recur (rest moves) (+ zeroes revs extra-zero) newpos))))))


;; (time (count-all-zeroes small-input))
;; "Elapsed time: 0.6038 msecs"
;; 6
;; (time (count-all-zeroes large-input))
;; "Elapsed time: 8.3655 msecs"
;; 5892
