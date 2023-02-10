(ns advent-13.core)

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

;; Day 13: Packet Scanners

;; Part 1
;; What's the severity of a trip through the firewall starting at time 0?
(defn parse-input [f]
  (->> f
       read-lines
       (map #(re-find #"(\d+): (\d+)" %))
       (map rest)
       (map #(map parse-long %))
       (reduce (fn [acc [k v]] (assoc acc k v)) {})))

(defn position-at-time [scan-range time]
  (if (= scan-range 1)
    1
    (let [path-length (- (* 2 scan-range) 2)
          path-position (mod time path-length)]
      (if (< path-position scan-range)
        path-position
        (- (* 2 scan-range) path-position 2)))))

(defn trip-results [scanner-ranges start-time]
  (let [scanners (sort (keys scanner-ranges))]
    (for [scanner scanners
          :let [range (get scanner-ranges scanner)
                pos (position-at-time range (+ start-time scanner))]
          :when (= pos 0)]
      (list scanner (* scanner range)))))

(defn trip-severity [scanner-ranges start-time]
  (reduce (fn [sum [scanner severity]]
            (+ sum severity))
          0
          (trip-results scanner-ranges start-time)))

;; (trip-severity (parse-input small-input) 0)
;; 24

;; (trip-severity (parse-input large-input) 0)
;; 2160



;; Part 2
;; What is the fewest number of picoseconds you can delay the packet to pass through the firewall undetected?
(defn max-iterations [scanner-ranges]
  (apply *
         (reduce (fn [acc n] (if (some #(integer? (/ % n)) acc) acc (conj acc n)))
                 []
                 (reverse (sort (set (vals scanner-ranges)))))))

(defn safe-packet-delay [scanner-ranges]
  (let [max-i (max-iterations scanner-ranges)]
    (loop [i 0]
      (cond (= i max-i) :max-i
            (= 0 (count (trip-results scanner-ranges i))) i
            :else (recur (inc i))))))


;; (time (safe-packet-delay (parse-input small-input)))
;; "Elapsed time: 0.7752 msecs"
;; 10

;; (time (safe-packet-delay (parse-input large-input)))
;; "Elapsed time: 199631.6804 msecs"
;; 3907470
