(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 4: Repose Record

;; Part 1
;; Find the guard who sleeps most. What is that guard's id multiplied by the most-slept minute?
(defn parse-timestamp [timestamp]
  (let [[_ y m d hh mm] (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\]" timestamp)]
    {"y" (parse-long y) "m" (parse-long m) "d" (parse-long d) "hh" (parse-long hh) "mm" (parse-long mm)}))

(defn parse-sleep-wake [sleep-wake]
  (map parse-timestamp sleep-wake))

(defn parse-shift [[init sleeps]]
  (let [guard-id (parse-long (second (re-find #"Guard #(\d+) begins shift" (first init))))]
    {guard-id (map parse-sleep-wake (partition 2 sleeps))}))

(defn get-instant [line]
  (let [timestamp (str/join "T" (rest (re-find #"(\d+-\d+-\d+) (\d+:\d+)" line)))]
    (inst-ms (instant/read-instant-timestamp timestamp))))

(defn parse-input [f]
  (->> f
       (read-lines)
       (sort-by get-instant)
       (partition-by #(re-find #"Guard" %))
       (partition 2)
       (map parse-shift)
       (apply merge-with concat)
       ))

(defn sleep-amounts [records]
  (->> records
       (map (fn [[id shifts]] {id (apply + (map (fn [[sleep wake]] (- (get wake "mm") (get sleep "mm"))) shifts))}))
       (apply merge)))

(defn sleepiest-guard [sleep-amounts]
  (first (last (sort-by second sleep-amounts))))

(defn sleep-minute-map [sleep wake]
  (let [start (get sleep "mm")
        end (get wake "mm")]
    (reduce (fn [acc minute] (assoc acc minute 1))
            (sorted-map)
            (range start end))))

(defn sleep-minute-maps [records]
  (->> records
       (map (fn [[id shifts]] {id (apply merge-with + (map (fn [[sleep wake]] (sleep-minute-map sleep wake)) shifts))}))
       (apply merge)))

(defn minute-with-most-sleeps [sleep-map]
  (let [most-sleeps (apply max (vals sleep-map))
        with-most (filter #(= (second %) most-sleeps) sleep-map)]
    (first (first (sort with-most)))))

(defn compute-result [records]
  (let [the-sleep-amounts (sleep-amounts records)
        the-minute-maps (sleep-minute-maps records)
        sleepiest (sleepiest-guard the-sleep-amounts)
        sleepiest-map (get the-minute-maps sleepiest)
        sleepiest-minute (minute-with-most-sleeps sleepiest-map)]
    (printf "Guard #%d slept for %d minutes total, most frequently at minute %d: %d.\n"
            sleepiest (get the-sleep-amounts sleepiest) sleepiest-minute (* sleepiest sleepiest-minute))
    (get the-minute-maps sleepiest)))

;; 56812
;; ---> answer <---
;; 91305
;; 95363




(def test-times '((0 10) (19 22) (40 51) (36 40) (43 47) (57 58) (27 43) (22 55) (11 55) (0 33) (38 49) (13 56) (5 46) (1 34) (42 51) (54 58) (26 29) (15 42) (45 56) (28 58) (53 55) (9 31) (34 46) (55 57) (50 55) (29 40)))

(defn get-minutes [records]
  (apply merge (map (fn [[k vs]] {k (map (fn [[a b]] (list (get a "mm") (get b "mm"))) vs)}) records)))

(defn expand-time [[start end]]
  (range start end))

(defn map-from-expanded [expanded]
  (reduce (fn [acc n] (assoc acc n 1))
          {}
          expanded))


;; TODO: there's a bug somewhere, but reconstructing the code by hand-ish gives me the same result, guard #2029, 425 minutes:
;; (sort-by second (map (fn [[id mins]] (list id (apply + (map (fn [[a b]] (- b a)) mins)))) (get-minutes (parse-input large-input))))
;;    ...
;;  (2029 425))

;; (sort (apply merge-with + (map map-from-expanded (map expand-time (get (get-minutes (parse-input large-input)) 2029)))))
;; several minutes were slept on 11 times, which seems wrong.
