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
(defn blink [stone]
  (cond (= stone "0")
        ["1"]
        (even? (count stone))
        [(subs stone 0 (quot (count stone) 2))
         (str (parse-long (subs stone (quot (count stone) 2))))]
        :else
        [(str (* 2024 (parse-long stone)))]))

(defn blink-multiple [stones]
  (apply concat
         (for [stone stones]
           (blink stone))))f

(defn blink-multiple-count [input n]
  (count
   (doall
    (loop [stones (parse-input input)
           i n]
      (let [_ (Thread/sleep 1)
            result (blink-multiple stones)
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
(defn blink-build-tree [stones n]
  (loop [productions {}
         stones stones
         i 0]
    (if (= i n)
      productions
      (let [keyset (set (keys productions))
            new-stones (get (group-by keyset stones) nil)
            new-prods (map #(vector % (blink %)) new-stones)]
        (recur (into productions new-prods)
               (remove keyset (apply concat (map second new-prods)))
               (inc i))))))

(defn blink-walk-tree [tree stones n]
  (loop [stone-freqs (frequencies stones)
         i 0]
    (let [new-freqs (apply concat (map (fn [[s f]] (map #(vector % f) (get tree s))) stone-freqs))
          condensed-freqs (into {} (map (fn [[k vs]] [k (apply + (map second vs))]) (group-by first new-freqs)))]
      (if (= i n)
        (apply + (map second stone-freqs))
        (recur condensed-freqs
               (inc i))))))

(defn blink-count-with-tree [input n]
  (let [stones (parse-input input)
        tree (blink-build-tree stones 75)]
    (blink-walk-tree tree stones n)))


;; (time (blink-count-with-tree small-input 25))
;; "Elapsed time: 7.494 msecs"
;; 55312
;; (time (blink-count-with-tree large-input 25))
;; "Elapsed time: 68.3795 msecs"
;; 193899
;; (time (blink-count-with-tree small-input 75))
;; "Elapsed time: 14.7395 msecs"
;; 65601038650482
;; (time (blink-count-with-tree large-input 75))
;; "Elapsed time: 436.2177 msecs"
;; 229682160383225
