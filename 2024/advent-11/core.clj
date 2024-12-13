(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn print-disk [disk]
  (str/join
   (map #(if (nil? %) "." (format "%d" %)) disk)))

(defn parse-input [input]
  (->> input
       (read-lines)
       (first)
       (#(str/split % #" "))))

;; Part 1
;; How many stones will there be after 25 blinks?
(defn blink [stones]
  (apply concat
         (for [stone stones]
           (cond (= stone "0")
                 ["1"]
                 (even? (count stone))
                 [(subs stone 0 (quot (count stone) 2))
                  (str (parse-long (subs stone (quot (count stone) 2))))]
                 :else
                 [(str (* 2024 (parse-long stone)))]
                 ))))

(defn blink-multiple-count [input n]
  (count
   (doall
    (loop [stones (parse-input input)
           i n]
      (let [_ (Thread/sleep 1)
            result (blink stones)
            _ (println "blink" (inc (- n i)) (count result))]
       (if (= i 0)
         stones
         (recur result (dec i))))))))


;; (time (blink-multiple-count small-input 25))
;; "Elapsed time: 55.5027 msecs"
;; 55312
;; (time (blink-multiple-count large-input 25))
;; "Elapsed time: 338.2661 msecs"
;; 193899


;; Part 2
;; How many stones will there be after 75 blinks?

;; Try computing each number's evolution separately. Note how 2024 evolves in the example.
