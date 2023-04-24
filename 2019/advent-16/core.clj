(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Flawed Frequency Transmission

;; Part 1
;; After 100 phases of FFT, what are the first eight digits in the final output list?
(defn parse-line [line]
  (map parse-long (map str (vec line))))

(defn parse-input [f]
  (parse-line (first (read-lines f))))

(def base-pattern [0 1 0 -1])

(def pattern-cache (atom {}))

(defn generate-pattern [length output-digit]
  (let [cached (get @pattern-cache [length output-digit])]
    (if cached
      cached
      (let [new-value (take length (drop 1 (apply concat (map #(repeat output-digit %) (apply concat (repeat base-pattern))))))]
        (swap! pattern-cache assoc [length output-digit] new-value)
        new-value))))

(defn apply-pattern [signal pattern]
  (parse-long (str (last (str (apply + (map * signal pattern)))))))

(defn run-fft-phase [signal]
  (for [i (range (count signal))]
    (apply-pattern signal (generate-pattern (count signal) (inc i)))))

(defn run-fft [signal phases]
  (loop [signal signal
         phase 0]
    (println "phase" phase "signal start" (take 20 signal) "size" (count signal) (new java.util.Date))
    (if (= phase phases)
      signal
      (recur (run-fft-phase signal) (inc phase))))
  ;;(nth (iterate run-fft-phase signal) phases)
  )

(defn first-8-after-fft [signal phases]
  (reset! pattern-cache {})
  (str/join
    (map str
         (take 8
               (run-fft signal phases)))))

(deftest test-first-8-after-fft
  (is (= 1029498 (first-8-after-fft (parse-line "12345678") 4)))
  (is (= 24176176 (first-8-after-fft (parse-line "80871224585914546619083218645595") 100)))
  (is (= 73745418 (first-8-after-fft (parse-line "19617804207202209144916044189917") 100)))
  (is (= 52432133 (first-8-after-fft (parse-line "69317163492948606335995924319873") 100))))

;; (time (first-8-after-fft (parse-line "12345678") 4))
;; "Elapsed time: 0.5514 msecs"
;; "01029498"

;; (time (first-8-after-fft (parse-input large-input) 100))
;; "Elapsed time: 25114.0115 msecs"
;; "94960436"



;; Part 2
;; What is the eight-digit message embedded in the final output list?
(defn find-offset [signal]
  (->> signal
   (take 7)
   (map str)
   (str/join)
   (parse-long)))

(deftest test-find-offset
  (is (= 1234567 (find-offset (parse-line "12345678"))))
  (is (= 303673 (find-offset (parse-line "03036732577212944063491565474664"))))
  (is (= 293510 (find-offset (parse-line "02935109699940807407585447034323"))))
  (is (= 308177 (find-offset (parse-line "03081770884921959731165446850517")))))

(defn decode-signal [initial-signal]
  (let [signal (apply concat (repeat 10000 initial-signal))]
    (first-8-after-fft signal 2)))

;; (deftest test-decode-signal
;;   (is (= 84462026 (find-offset (parse-line "03036732577212944063491565474664"))))
;;   (is (= 78725270 (find-offset (parse-line "02935109699940807407585447034323"))))
;;   (is (= 53553731 (find-offset (parse-line "03081770884921959731165446850517")))))


(defn apply-pattern-groups [signal digit]
  (let [partitioned (map-indexed list (partition-all digit (conj (seq signal) 0)))
        ;; _ (println "partitioned" partitioned)
        sums (reduce (fn [acc [i ns]]
                       (case (mod i 4)
                         1 (update acc :positive #(+ % (apply + ns)))
                         3 (update acc :negative #(+ % (apply + ns)))
                         acc))
                     {:positive 0 :negative 0}
                     partitioned)
        result (- (:positive sums) (:negative sums))
        ;; _ (println "result" result)
        ]
    (parse-long (str (last (str result))))))

(defn power-of-2? [n]
  (and (not= n 0)
       (or (= n 1)
           (and (even? n)
                (power-of-2? (quot n 2))))))

(defn run-fft-phase-groups [signal]
  (for [i (range (count signal))]
    (do
      ;; (if (power-of-2? i)
      ;;   (println "run-fft-phase-groups" i))
      (apply-pattern-groups signal (inc i)))))

(defn run-fft-groups [signal phases]
  (loop [signal signal
         phase 0]
    (println "phase" phase "signal start" (take 20 signal) "size" (count signal) (new java.util.Date))
    (if (= phase phases)
      signal
      (recur (run-fft-phase-groups signal) (inc phase)))))

(defn first-8-after-fft-groups [signal phases]
  (str/join
    (map str
         (take 8
               (run-fft-groups signal phases)))))

(defn decode-signal-groups [initial-signal]
  (let [signal (apply concat (repeat 10000 initial-signal))]
    (first-8-after-fft-groups signal 2)))

;; (time (first-8-after-fft-groups (apply concat (repeat 10000 (parse-input large-input))) 1))
;; Very slow.
;; TODO: maybe the problem is a lot of copying of data. How about if I leave the signal alone
;; and get subvectors out of it at each iteration?
(defn apply-pattern-in-place [signal digit]
  (let [pattern-size (* 4 digit)
        signal-size (count signal)]
    (reduce (fn [acc base]
              (let [[pos-min pos-max] [(+ base digit) (min (+ base digit digit) signal-size)]
                    [neg-min neg-max] [(+ base (* 3 digit)) (min (+ base (* 4 digit)) signal-size)]]
                (if (>= pos-min (count signal))
                  (reduced (mod (abs acc) 10))
                  (let [pos-sum (if (< pos-min signal-size)
                                  (reduce (fn [pos-acc i] (+ pos-acc (get signal i 0))) 0 (range pos-min pos-max))
                                  0)
                        neg-sum (if (< neg-min signal-size)
                                  (reduce (fn [neg-acc i] (- neg-acc (get signal i 0))) 0 (range neg-min neg-max))
                                  0)]
                    (+ acc pos-sum neg-sum)))))
            0
            (map #(- (* % pattern-size) 1) (range)))))


(defn run-fft-phase-in-place [signal]
  (mapv (fn [digit] (apply-pattern-in-place signal (inc digit)))
        (range (count signal))))

(defn run-fft-in-place [signal phases]
  (loop [signal signal
         phase 0]
    ;; (println "phase" phase "signal start" (take 20 signal) "size" (count signal) (new java.util.Date))
    (if (= phase phases)
      signal
      (recur (run-fft-phase-in-place signal) (inc phase)))))

(defn first-8-after-fft-in-place [signal phases]
  (str/join
    (map str
         (take 8
               (run-fft-in-place (vec signal) phases)))))

(deftest test-first-8-after-fft-in-place
  (is (= "01029498" (first-8-after-fft-in-place (parse-line "12345678") 4)))
  (is (= "24176176" (first-8-after-fft-in-place (parse-line "80871224585914546619083218645595") 100)))
  (is (= "73745418" (first-8-after-fft-in-place (parse-line "19617804207202209144916044189917") 100)))
  (is (= "52432133" (first-8-after-fft-in-place (parse-line "69317163492948606335995924319873") 100))))

;; This is good; it's 25 times faster than the original.
;; (time (first-8-after-fft-in-place (parse-input large-input) 100))
;; "Elapsed time: 895.5379 msecs"
;; "94960436"

;; Replacing the string stuff with mod and capping the ranges at the end of the signal cut the time down even more.
;; (time (first-8-after-fft-in-place (parse-input large-input) 100))
;; "Elapsed time: 567.1624 msecs"
;; "94960436"


(defn decode-signal-in-place [initial-signal]
  (let [signal (apply concat (repeat 10000 initial-signal))]
    (first-8-after-fft-in-place signal 2)))

(deftest test-decode-signal-in-place
  (is (= 84462026 (decode-signal-in-place (parse-line "03036732577212944063491565474664"))))
  (is (= 78725270 (decode-signal-in-place (parse-line "02935109699940807407585447034323"))))
  (is (= 53553731 (decode-signal-in-place (parse-line "03081770884921959731165446850517")))))

;; Took a vague hint from reddit.
;; I've got a positive sum and a negative sum. Compared to the sum from the previous digit,
;; each one drops the first number and adds two new numbers. Instead of adding all the digits,
;; I just need to subtract on and add two at each step.
