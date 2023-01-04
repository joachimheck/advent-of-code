(ns advent-06.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many exponentially reproducing lanternfish will there be after 80 days?
(defn parse-input [f]
  (load-string (str/join (list "[" (first (read-lines f)) "]"))))

(defn fish-dec [n]
  (if (= n 0)
    6
    (dec n)))

(defn model-fish [fishes max-days]
  (loop [fishes fishes
         reproducing (count (filter #{0} fishes))
         day 0]
    ;; (println "After" (format "%2d" day) "days:" fishes)
    (if (= day max-days)
      fishes
      (let [new-fishes (mapv fish-dec fishes)]
       (recur (apply conj new-fishes (vec (repeat reproducing 8)))
              (count (filter #{0} new-fishes))
              (inc day))))))

;; (time (count (model-fish (parse-input small-input) 80)))
;; "Elapsed time: 16.085001 msecs"
;; 5934
;; (time (count (model-fish (parse-input large-input) 80)))
;; "Elapsed time: 608.2645 msecs"
;; 345793

;; Part 2
;; How many would there be after 256 days?
(defn model-fish-2 [fishes max-days]
  (loop [counts (frequencies fishes)
         day 0]
    ;; (println "After" (format "%2d" day) "days:" (sort counts))
    (if (= day max-days)
      counts
      (recur (reduce (fn [result i]
                       (if (= i 0)
                         (-> result
                             (update 6 #(+ % (get counts 0 0)))
                             (assoc 8 (get counts 0 0)))
                         (assoc result (dec i) (get counts i 0))))
                     {}
                     (reverse (range 9)))
             (inc day)))))

;; (time (apply + (map second (model-fish-2 (parse-input small-input) 256))))
;; "Elapsed time: 4.711501 msecs"
;; 26984457539
;; (time (apply + (map second (model-fish-2 (parse-input large-input) 256))))
;; "Elapsed time: 6.5486 msecs"
;; 1572643095893
