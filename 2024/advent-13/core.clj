(ns advent-13.core)

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
       (partition-by #{""})
       (remove #{'("")})
       (map str/join)
       (map #(re-find #"Button A: X\+(\d+), Y\+(\d+)Button B: X\+(\d+), Y\+(\d+)Prize: X=(\d+), Y=(\d+)" %))
       (map #(map parse-long %))
       (map (fn [[_ ax ay bx by px py]] {:a [ax ay] :b [bx by] :prize [px py]}))))

;; Part 1
;; What is the fewest tokens you would have to spend to win all possible prizes?
(defn fewest-tokens [{[ax ay] :a [bx by] :b [px py] :prize :as machine}]
  (first
   (for [a (range 101)
         b (range 101)
         :when (and (= px (+ (* a ax) (* b bx)))
                    (= py (+ (* a ay) (* b by))))]
     [a b (+ (* 3 a) b)])))

(defn min-tokens-to-win-winnable-prizes [input]
  (let [machines (parse-input input)]
    (apply + (map last (remove nil? (map fewest-tokens machines))))))

;; (time (min-tokens-to-win-winnable-prizes small-input))
;; "Elapsed time: 2.4968 msecs"
;; 480
;; (time (min-tokens-to-win-winnable-prizes large-input))
;; "Elapsed time: 121.0937 msecs"
;; 29023


;; Part 2
;; The prizes are much further away than previously understood.
(defn parse-input-2 [input]
  (->> input
       (read-lines)
       (partition-by #{""})
       (remove #{'("")})
       (map str/join)
       (map #(re-find #"Button A: X\+(\d+), Y\+(\d+)Button B: X\+(\d+), Y\+(\d+)Prize: X=(\d+), Y=(\d+)" %))
       (map #(map parse-long %))
       (map (fn [[_ ax ay bx by px py]] {:a [ax ay] :b [bx by] :prize [(+ px 10000000000000) (+ py 10000000000000)]}))))

(defn fewest-tokens-2 [{[ax ay] :a [bx by] :b [px py] :prize :as machine} max]
  (first (for [a (range max)
               b (range max)
               :when (not (and (zero? a) (zero? b)))
               :let [xval (+ (* a ax) (* b bx))
                     xrem (rem px xval)
                     yval (+ (* a ay) (* b by))
                     yrem (rem py yval)]
               :when (and (zero? xrem) (zero? yrem))
               :let [a-pushes (* a (/ px xval))
                     b-pushes (* b (/ py yval))]]
           [a-pushes b-pushes])))

;; This method yields no results up to 15,000 pushes of A or B, and it takes 13 seconds.
;; Hopefully there's a more efficient way to do this.

;; (time (fewest-tokens-2 (nth (parse-input-2 small-input) 1) 15000))
;; "Elapsed time: 13786.1263 msecs"
;; nil

(defn prime-factors [n]
  (let [[n factors]
        (loop [n n
               factors []]
          (Thread/sleep 1)
          (cond (= n 1)
                factors
                (= 0 (rem n 2))
                (do
                  (println "factors" (conj factors 2) (quot n 2))
                  (recur (quot n 2) (conj factors 2)))
                :else
                [n factors]))]
    (loop [n n
           d 3
           factors factors
           root (Math/sqrt n)]
      (Thread/sleep 1)
      (cond (= n 1)
            factors
            (> d root)
            (conj factors n)
            (= 0 (rem n d))
            (do
              (println "factors" (conj factors d) (quot n d) (Math/sqrt (quot n d)))
              (recur (quot n d)
                     d
                     (conj factors d)
                     (Math/sqrt (quot n d))))
            :else
            (recur n (+ d 2) factors root)))))

(defn fewest-tokens-x [{[ax ay] :a [bx by] :b [px py] :prize :as machine}]
  (let [[a b :as moves]
        (first
         (for [a (range (inc (max ax ay bx by)))
               b (range (inc (max ax ay bx by)))
               :when (and (not (and (zero? a) (zero? b)))
                          (zero? (rem px (+ (* a ax) (* b bx))))
                          (zero? (rem py (+ (* a ay) (* b by)))))]
           [a b]))
        factor (if (some? moves)
                 (quot px (+ (* ax a) (* bx b))))]
    (if (some? factor)
      (mapv #(* % factor) [a b (+ (* 3 a) b)]))))


;; This seems to disprove my theory that the maximum values of a and b are
;; bounded by the values of ax, ay, bx, and by.

;; (first
;;  (filter (fn [[_ _ _ b _]] (= false b))
;;          (for [[index machine] (map-indexed list (parse-input large-input))
;;                :let [f (fewest-tokens machine)
;;                      f-x (fewest-tokens-x machine)]
;;                :when (or (some? f) (some? f-x))]
;;            (list index f f-x (= f f-x) machine))))
;; (2 [96 71 359] nil false {:a [13 42], :b [71 29], :prize [6289 6091]})
