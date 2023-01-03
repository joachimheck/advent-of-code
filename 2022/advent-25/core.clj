(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; Sum up some SNAFU numbers.
(defn pow [x n]
  (apply * (repeat n x)))

(def snafu-to-decimal {
                       \2 2
                       \1 1
                       \0 0
                       \- -1
                       \= -2
                       })

(defn snafu-to-base-10 [snafu]
  (->> snafu
       vec
       (map snafu-to-decimal)
       reverse
       (keep-indexed list)
       (map (fn [[n d]] (* d (pow 5 n))))
       (apply +)))

;; From internet, and day 8.
(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))


(defn range-for-column [col]
  (map (fn [d] (* d (pow 5 col))) [-3 -2 -1 0 1 2 3]))

(defn base-10-to-snafu [decimal-number]
  ;; (println "base-10-to-snafu" decimal-number)
  (let [snafu-columns (reverse (take-until (fn [n] (<= (snafu-to-base-10 (repeat n \=)) decimal-number (snafu-to-base-10 (repeat n \2)))) (range)))
        ranges (map range-for-column snafu-columns)]
    (str/join
     (drop-while #(= \0 %)
                 (map (set/map-invert snafu-to-decimal)
                      (second (reduce
                               (fn [[reduced-n results] r]
                                 ;; (println "reduce" [reduced-n results] r)
                                 (let [range-match (first (filter (fn [[lower upper]] (<= lower reduced-n upper)) (partition 2 1 r)))
                                       [precise-lower precise-upper] range-match
                                       ;; _ (println "precise" [precise-lower precise-upper])
                                       closest (if (< (abs (- precise-lower reduced-n)) (abs (- precise-upper reduced-n)))
                                                 precise-lower
                                                 precise-upper)
                                       closest-digit (- (.indexOf r closest) 3)]
                                   [(- reduced-n closest) (conj results closest-digit)]))
                               (list decimal-number [])
                               ranges)))))))

;; (base-10-to-snafu (apply + (map snafu-to-base-10 (read-lines small-input))))
;; "2=-1=0"
;; (base-10-to-snafu (apply + (map snafu-to-base-10 (read-lines large-input))))
;; "20-1-0=-2=-2220=0011"



;; Part 2
;; 
