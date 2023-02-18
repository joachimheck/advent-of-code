(ns advent-24.core)

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

;; Day 24: Electromagnetic Moat

;; Part 1
;; What's the strength of the strongest bridge made by combining the available components?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"(\d+)/(\d+)" %))
       (map rest)
       (map #(map parse-long %))
       (map (fn [[a b]] (if (< a b) [a b] [b a])))))

(defn starting-components [components]
  (filter #(some #{0} %) components))

(defn extensions [bridge components]
  (let [free-port (second (last bridge))
        free-components (remove (set (concat bridge (map vec (map reverse bridge)))) components)
        available (filter #(some #{free-port} %) free-components)
        directional (map (fn [[a b]] (if (= free-port a) [a b] [b a])) available)]
    (map #(conj bridge %) directional)))

(defn valid-bridges [components]
  (loop [i 0 open-set (map vector (starting-components components)) completed '()]
    (let [extensions (apply concat (map #(extensions % components) open-set))]
        (if (empty? extensions)
          (concat completed open-set)
          (recur (inc i) extensions (concat completed open-set))))))

(defn strength [bridge]
  (apply + (flatten bridge)))

(defn find-strongest-bridge [components]
  (last (sort-by first (map #(list (strength %) %) (sort (valid-bridges components))))))


;; (time (find-strongest-bridge (parse-input small-input)))
;; "Elapsed time: 1.4943 msecs"
;; (31 [[0 1] [1 10] [10 9]])
;; (time (find-strongest-bridge (parse-input large-input)))

;; "Elapsed time: 149890.7161 msecs"
;; (1868 [[0 7] [7 30] [30 1] [1 1] [1 50] [50 50] [50 46] [46 26] [26 41] [41 47] [47 38] [38 34] [34 17] [17 43] [43 4] [4 6] [6 28] [28 32] [32 23] [23 2] [2 28] [28 11] [11 21] [21 13] [13 50] [50 39] [39 39] [39 31] [31 13] [13 25] [25 18] [18 17] [17 14] [14 10] [10 16] [16 42] [42 42]])




;; Part 2
;; What's the strength of the longest bridge?
(defn find-longest-bridge [components]
  (let [bridges (map #(list (count %) (strength %) %) (valid-bridges components))
        max-length (apply max (map first bridges))
        all-longest (filter #(= max-length (first %)) bridges)]
    (last (sort-by second all-longest))))

;; (time (find-longest-bridge (parse-input small-input)))
;; "Elapsed time: 1.173 msecs"
;; (4 19 [[0 2] [2 2] [2 3] [3 5]])

;; (time (find-longest-bridge (parse-input large-input)))
;; "Elapsed time: 147674.2118 msecs"
;; (40 1841 [[0 9] [9 28] [28 2] [2 23] [23 32] [32 28] [28 6] [6 4] [4 43] [43 17] [17 14] [14 10] [10 16] [16 7] [7 30] [30 1] [1 1] [1 50] [50 50] [50 39] [39 39] [39 31] [31 13] [13 25] [25 18] [18 17] [17 34] [34 38] [38 47] [47 41] [41 26] [26 46] [46 50] [50 19] [19 11] [11 21] [21 3] [3 24] [24 5] [5 5]])
