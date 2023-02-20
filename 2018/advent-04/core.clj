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
  (let [guard-id (parse-long (second (re-find #"Guard #(\d+) begins shift" init)))]
    (assoc (sorted-map) guard-id (map parse-sleep-wake (partition 2 sleeps)))))

(defn get-instant [line]
  (let [timestamp (str/join "T" (rest (re-find #"(\d+-\d+-\d+) (\d+:\d+)" line)))]
    (inst-ms (instant/read-instant-timestamp timestamp))))

;; Broken! Doesn't account for shifts in which the guard doesn't sleep!
;; (defn parse-input [f]
;;   (->> f
;;        (read-lines)
;;        (sort-by get-instant)
;;        (partition-by #(re-find #"Guard" %))
;;        (partition 2)
;;        (map parse-shift)
;;        (apply merge-with concat)))

(defn parse-input [f]
  (let [shifts (loop [lines (read-lines f) groups []]
                 (if (empty? lines)
                   groups
                   (let [line (first lines)]
                     (if (re-find #"Guard #(\d+) begins" line)
                       (recur (rest lines) (conj groups (list line [])))
                       (let [[guard changes] (last groups)]
                         (recur (rest lines) (conj (vec (drop-last groups)) (list guard (conj changes line)))))))))]
    (apply merge-with concat (map parse-shift shifts))))

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
    ;;(get the-minute-maps sleepiest)
    ))

;; 56812

;; ---> answer <---
;; 91305

;; also wrong
;; 58841
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

(defn get-minutes-with-dates-and-ids [records]
  (into (sorted-map)
   (group-by #(vector (nth % 0) (nth % 1) (nth % 2))
             (apply concat (map (fn [[k vs]] (map (fn [[a b]] (list k (get a "m") (get a "d") (get a "mm") (get b "mm"))) vs)) records)))))

;; TODO: there's a bug somewhere, but reconstructing the code by hand-ish gives me the same result, guard #2029, 425 minutes:
;; (sort-by second (map (fn [[id mins]] (list id (apply + (map (fn [[a b]] (- b a)) mins)))) (get-minutes (parse-input large-input))))
;;    ...
;;  (2029 425))

;; (sort (apply merge-with + (map map-from-expanded (map expand-time (get (get-minutes (parse-input large-input)) 2029)))))
;; several minutes were slept on 11 times, which seems wrong.

(defn in-time-ranges [t ranges]
  (not
   (empty?
    (for [[t1 t2] ranges
          :when (and (<= t1 t) (< t t2))]
      true))))

(defn draw-shifts [records]
  (println "Date   ID     Minute")
  (println "              000000000011111111112222222222333333333344444444445555555555")
  (println "              012345678901234567890123456789012345678901234567890123456789")
  (doseq [[[id m d mm1 mm2 :as shift] times] (get-minutes-with-dates-and-ids records)]
    (printf "%02d-%02d  #%04d  "m d id)
    (doseq [t (range 0 60)]
      (if (in-time-ranges t (map #(drop 3 %) times))
        (print "#")
        (print ".")))
    (newline)))

(defn match-input-line [line]
  (let [guard-match (second (re-find #"Guard #(\d+) begins" line))]
   (if guard-match
     guard-match
     )))
(defn parse-input-test [f]
  (->> f
       (read-lines)
       (sort-by get-instant)
       (partition-by #(second (re-find #"Guard #(\d+) begins" %)))
       (partition 2)
       ;; (map parse-shift)
       ;; (apply merge-with concat)
))

;;;;;;;;;; START IMPORTED CODE ;;;;;;;;;;
;; From reddit.
;; (defn get-lines []
;;   (with-open [r (clojure.java.io/reader
;;                  (clojure.java.io/resource "large-input.txt"))]
;;     (doall (line-seq r))))

;; (defn loop-through-times [f]
;;   (loop [lines (read-lines f) id nil times [] last-time nil]
;;     (if (nil? (first lines))
;;       times
;;       (let [line    (first lines)
;;             minutes (Integer/parseInt (second (re-find #"\:(\d+)" line)))]
;;         (if (clojure.string/ends-with? line "begins shift")
;;           (recur (rest lines) (first (re-seq #"\#\d+" line)) times nil)
;;           (if (nil? last-time)
;;             (recur (rest lines) id times minutes)
;;             (recur (rest lines) id (conj times [id minutes last-time]) nil)))))))

;; (defn time-range-maps [[id wake-time sleep-time]]
;;   {id (- wake-time sleep-time)})

;; (defn get-sleepiest-guard [time-ranges]
;;   (first (apply max-key second (apply merge-with + time-ranges))))

;; (def sleepiest-guard (get-sleepiest-guard (map time-range-maps (loop-through-times))))

;; (defn all-minutes [[id wake-time sleep-time]]
;;   (map (fn [x] [id x]) (range sleep-time wake-time)))

;; (defn most-common-element [coll]
;;   (first (apply max-key second (frequencies coll))))

;; (defn most-common-minute [times]
;;   (most-common-element
;;    (map second
;;         (filter #(= (first %) sleepiest-guard) times))))

;; (def all-slept-minutes (mapcat all-minutes (loop-through-times)))

;; (println (most-common-minute all-slept-minutes))

;; (println (most-common-element all-slept-minutes))
;;;;;;;;;; END IMPORTED CODE ;;;;;;;;;;


;; Hoookay, I got hung up on a bug in my parser for a long time, and finally had to import somebody else's
;; code to find it. I wasn't handling sleepless shifts, which result in two guard announcements after one
;; another, with no status changes in between.

;; (compute-result (parse-input small-input))
;; Guard #10 slept for 50 minutes total, most frequently at minute 24: 240.
;; nil

;; (compute-result (parse-input large-input))
;; Guard #1987 slept for 497 minutes total, most frequently at minute 34: 67558.
;; nil



;; Part 2
;; Which guard is most frequently asleep on the same minute?
(defn compute-result-2 [records]
  (let [the-sleep-amounts (sleep-amounts records)
        the-minute-maps (sleep-minute-maps records)
        most-slept-minutes (apply merge (map (fn [[k v]] {k (if (empty? v) [0 0] (apply max-key val v))}) the-minute-maps))
        [guard [minute sleeps]] (last (sort-by #(second (second %)) most-slept-minutes))]
    (printf "Guard #%d slept %d times at minute %d: %d.\n"
            guard sleeps minute (* guard minute))))

;; (compute-result-2 (parse-input small-input))
;; Guard #99 slept 3 times at minute 45: 4455.
;; nil

;; (compute-result-2 (parse-input large-input))
;; Guard #2633 slept 17 times at minute 30: 78990.
;; nil

