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
  (loop [n n
         d 2
         factors []
         root (Math/sqrt n)]
    ;; (Thread/sleep 1)
    (Thread/yield)
    (cond (= n 1)
          factors
          (> d root)
          (conj factors n)
          (= 0 (rem n d))
          (let [new-n (quot n d)]
            ;; (println "Found factor for" n (conj factors d))
            (recur new-n
                   d
                   (conj factors d)
                   (Math/sqrt new-n)))
          :else
          (recur n (if (= d 2) 3 (+ d 2)) factors root))))

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
(defn fewest-tokens-x [ax bx px]
  (let [{multiples true factors false} (group-by #(< % (min ax bx)) (prime-factors px))
        _ (println "multiples/factors" [multiples factors])
        multiplier (apply * multiples)
        ;; [a b :as moves]
        moves (for [mini-px factors]
                (first
                 (for [a (range (quot mini-px ax))
                       b (range (quot mini-px bx))
                       :when (and (not (and (zero? a) (zero? b)))
                                  (zero? (rem mini-px (+ (* a ax) (* b bx)))))
                       :let [_ (Thread/yield)]]
                   [a b (quot mini-px (+ (* a ax) (* b bx)))])))
        _ (println "moves" moves)
        final-multiplier (reduce (fn [acc [a b]]
                                   (if (empty? [a b])
                                     acc
                                     (* acc (+ (* a ax) (* b bx)))))
                                 multiplier
                                 (rest moves))
        _ (println "final-multiplier" final-multiplier)
        answer (if (seq moves)
                 [(* final-multiplier (first (first moves))) (* final-multiplier (second (first moves)))])]
    {:answer answer :total (if (seq answer) (+ (* ax (first answer)) (* bx (second answer))))}))

;; (+ (* 3 a) b) - score

;; #1: {:a [26 66], :b [67 21], :prize [10000000012748 10000000012176]}
;; #3: {:a [69 23], :b [27 71], :prize [10000000018641 10000000010279]}

;; (prime-factors 10000000012748) [2 2 789511 3166517]

;; This is close but I'm not decomposing the sum correctly. I need to get the a and b values out,
;; but I have the total correct.

;; (* multiplier
;;    (+ (* a1 ax) (* b1 bx))
;;    (+ (* a2 ax) (* b2 bx)))

(defn two-x-points
  ([{[ax _] :a [bx _] :b [px _] :prize :as machine}]
   (two-x-points ax bx px))
  ([ax bx px]
   (take 2
         (let [amax (quot px ax)]
           (for [n (range (inc amax))
                 :let [a (- amax n)
                       aval (* ax a)
                       arem (- px aval)
                       b (quot arem bx)
                       bval (* bx b)
                       diff (- px (+ aval bval))]
                 :when (zero? diff)]
             ;; {:a a :b b :aval aval :bval bval :diff diff :score (+ (* 3 a) b)}
             [a b])))))

(defn x-solutions [[[a1 b1] [a2 b2] :as points]]
  (if (seq points)
    ;; The first point is to the right of the second.
    (let [adiff (- a1 a2)
          bdiff (- b1 b2)]
      (take-while #(and (> (first %) 0) (> (second %) 0))
                  (iterate #(vector (- (first %) adiff) (- (second %) bdiff)) [a1 b1])))))

(defn is-y-solution? [{[_ ay] :a [_ by] :b [_ py] :prize :as machine} [a b]]
  (= (+ (* a ay) (* b by)) py))

(defn fewest-tokens-linear [{[ax ay] :a [bx by] :b [px py] :prize :as machine}]
  (let [x-sols (x-solutions (two-x-points machine))
        solutions (filter #(true? (last %)) (map #(list % (is-y-solution? machine %)) x-sols))
        ;; Assuming for now that only one answer will work so we don't have to check scores.
        ;; _ (println "solutions" solutions)
        [a b] (if (empty? solutions) [0 0] (first (first solutions)))]
    (+ (* 3 a) b)))

(defn min-tokens-to-win-linear [input]
  (let [machines (parse-input input)]
    (apply + (map fewest-tokens-linear machines))))



;; (min-tokens-to-win-linear large-input)
;; Execution error (NullPointerException) at advent-13.core/x-solutions (form-init2682020877504120393.clj:191).
;; Cannot invoke "Object.getClass()" because "x" is null
;; advent-13.core> 
