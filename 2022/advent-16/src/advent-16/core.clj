(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; What's the most pressure that can be released?
(defn parse-input [f]
  (reduce (fn [valves line]
              (let [first-match (rest (re-matches #".+([A-Z]{2}).+?(\d+).+" line))]
                (assoc valves
                       (first first-match)
                       {:flow-rate (Long/parseLong (second first-match))
                        :neighbors (mapv last (rest (re-seq #"(([A-Z]{2}),? ?)" line)))})))
          {}
          (read-lines f)))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn get-key-valves [valves]
  (for [[k v] valves :when (> (v :flow-rate) 0)] k))

(defn get-pairs [xs]
  (if (= (count xs) 2)
    (list (set xs))
    (concat (map (fn [x] #{(first xs) x}) (rest xs))
            (get-pairs (rest xs)))))

;; From internet.
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn compute-froms [valves current froms]
  (reduce (fn [froms neighbor] (merge {neighbor current} froms))
          froms
          (get (get valves current) :neighbors)))

(defn extract-path [start end froms]
  (if (= start end) [start]
      (apply conj (extract-path start (get froms end) froms) [end])))

(defn find-path [valves start goal]
  (loop [open-set #{start} froms {start nil}]
    (cond (empty? open-set) :path-not-found
          (contains? froms goal) (extract-path start goal froms)
          :else
          (let [open-valves (map #(get valves %) open-set)
                neighbors (set (mapcat #(get % :neighbors) open-valves))]
            (recur neighbors
             (reduce (fn [froms current] (compute-froms valves current froms))
                     froms
                     open-set))))))

(defn compute-distances [valves-to-compute valves]
  (reduce (fn [distance-map s]
            (let [a (first s) b (second s)]
              (assoc distance-map #{a b} (dec (count (find-path valves a b))))))
          {}
          (get-pairs valves-to-compute)))

(defn compute-pressure-relieved [valves distances visit-order time-left]
  (loop [result {:total 0 :flow-rate 0 :minutes 0} visit-pairs (partition 2 1 visit-order)]
      (cond (> (:minutes result) time-left) (list :over-time result)
            (= (:minutes result) time-left) (:total result)
            (empty? visit-pairs) (+ (:total result) (* (:flow-rate result) (- time-left (:minutes result))))
            :else
            (let [[a b :as pair] (first visit-pairs)
                  travel-time (inc (distances (set pair)))
                  valve (get valves b)
                  flow-rate (get valve :flow-rate)]
              (if (> (+ (:minutes result) travel-time) time-left)
                (+ (:total result) (* (:flow-rate result) (- time-left (:minutes result))))
                (recur
                 {:total (+ (:total result) (* (:flow-rate result) travel-time))
                  :flow-rate (+ (:flow-rate result) flow-rate)
                  :minutes (+ (:minutes result) travel-time)}
                 (rest visit-pairs)))))))

(def ^:dynamic *permutation-count* 0)

(def ^:dynamic *percent-done* 0)

(defn compute-max-relief [valves]
  (binding [*percent-done* 0
            *permutation-count* 0]
    (let [key-valves (get-key-valves valves)
          distances (compute-distances (conj key-valves "AA") valves)
          permutations-total (factorial (count key-valves))]
      (reduce (fn [result next]
                (set! *permutation-count* (inc *permutation-count*))
                (let [new-percent (int (* 100 (/ *permutation-count* permutations-total)))]
                  (if (> new-percent *percent-done*)
                    (do
                      (set! *percent-done* new-percent)
;                      (println "Processed" *permutation-count* "of" permutations-total "Percent done:" *percent-done*)
                      )))
                (if (< (first next) 0)
                  (reduced next)
                  (if (> (first next) (first result)) next result)))
              (map (fn [valve-ordering]
                     (let [ordering-plus-start (apply conj ["AA"] valve-ordering)]
                       (list (compute-pressure-relieved valves distances ordering-plus-start 30)
                             ordering-plus-start)))
                   (permutations key-valves))))))

;; Best order: DD BB JJ HH EE CC
;; Including moves: AA DD CC BB AA II JJ II AA DD EE FF GG HH GG FF EE DD CC
;; visit-order ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]
        
;; How about memoizing parts of the answers from compute-pressure-relieved? Surely we're recalculating a lot of path chunks there.

(defn compute-relief [start valve time-left valves]
  (let [key-valves (get-key-valves valves)
        distances (compute-distances (conj key-valves "AA") valves)
        travel-time (inc (distances #{start valve}))]
    (list (* (- time-left travel-time) ((get valves valve) :flow-rate)) valve)))

(defn compute-remaining-possible-relief [valves key-valves visited-valves]
  (let [remaining-valves (remove (set visited-valves) key-valves)
        start (last visited-valves)
        time-left 30]
    (apply + (map first (map #(compute-relief start % time-left valves) remaining-valves)))))

;; didn't work
(defn find-optimal-path [valves visited time-left]
  (let [all-key-valves (get-key-valves valves)
        distances (compute-distances (conj all-key-valves "AA") valves)]
    (loop [visited visited time-left time-left]
      (println visited time-left)
      (let [key-valves (remove (set visited) all-key-valves)]
        (if (empty? key-valves)
          visited
          (let [start (last visited)
                expected-relief (map (fn [valve] (list (+ (first (compute-relief start valve time-left valves))
                                                          (compute-remaining-possible-relief valves key-valves (conj (vec visited) valve)))
                                                       valve
                                                       (- time-left (inc (distances #{start valve})))))
                                     key-valves)
                choice (last (sort-by first expected-relief))]
            (recur (conj visited (second choice)) (last choice))))))))

(defn get-insertions [coll x]
  (for [i (range 1 (inc (count coll)))]
      (apply conj (subvec coll 0 i) x (subvec coll i (count coll)))))

;; didn't work
(defn find-path-insertion [valves]
  (let [key-valves (get-key-valves valves)
        sorted-key-valves (map first (sort-by second (map #(list % (get-in valves [% :flow-rate])) key-valves)))
        distances (compute-distances (conj key-valves "AA") valves)]
    (let [optimal-path (loop [path ["AA"] remaining-valves sorted-key-valves]
             (if (empty? remaining-valves)
               path
               (let [valve (first remaining-valves)
                     possible-paths (map (fn [path] (list path (compute-pressure-relieved valves distances path)))
                                         (get-insertions path valve))]
                 (println possible-paths)
                 (recur (first (last (sort-by second possible-paths)))
                        (remove #(= % valve) remaining-valves)))))]
      (list (compute-pressure-relieved valves distances optimal-path) optimal-path))))

;; didn't work.
(defn find-path-distance [valves]
  (let [key-valves (get-key-valves valves)
        distances (compute-distances (conj key-valves "AA") valves)]
    (let [optimal-path
          (loop [remaining-valves key-valves
                 path ["AA"]]
            ;; (println "loop" remaining-valves path)
            (if (= 1 (count remaining-valves))
              (apply conj path remaining-valves)
              (let [current (last path)
                    max-distance (apply max
                                        (for [valve remaining-valves]
                                          (distances #{current valve})))
                    relief-by-valve (reverse (sort-by second
                                                      (map (fn [valve]
                                                             (list valve (* (get-in valves [valve :flow-rate])
                                                                            (- (inc max-distance) (distances #{current valve})))))
                                                           remaining-valves)))
                    ;; _ (println "relief" relief-by-valve)
                    next-1 (first (first relief-by-valve))
                    next-2 (first (second relief-by-valve))
                    path-1 (conj path next-1 next-2)
                    path-2 (conj path next-2 next-1)
                    next-valve (if (> (compute-pressure-relieved valves distances path-1 30)
                                      (compute-pressure-relieved valves distances path-2 30))
                                 next-1
                                 next-2)]
                ;; (println path-1 (compute-pressure-relieved valves distances path-1 30) path-2 (compute-pressure-relieved valves distances path-2 30))
                (recur (remove #{next-valve} remaining-valves)
                       (conj path next-valve)))))]
      (list (compute-pressure-relieved valves distances optimal-path 30) optimal-path)
      )))

;; visit-order ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]

(defn list-paths [valves distances paths]
  (reverse (sort-by first (map (fn [path]
                                 (list (compute-pressure-relieved valves distances path 30) path))
                               paths))))

(defn extend-paths [path next-valves]
  (map (fn [valve] (conj path valve)) next-valves))

(defn find-path-take-n [valves n]
  (let [key-valves (get-key-valves valves)
        distances (compute-distances (conj key-valves "AA") valves)]
    (loop [paths (list ["AA"])]
      ;; (println paths)
      (if (= (count key-valves) (dec (count (first paths))))
        (first (list-paths valves distances paths))

        (let [paths-with-costs (reverse (sort-by first (mapcat (fn [path]
                                                         (list-paths valves distances (extend-paths path (remove (set path) key-valves))))
                                                       paths)))
              new-paths (map second paths-with-costs)]
          ;; (println paths paths-with-costs)
          (recur (take n new-paths)))))))

;; This finally worked but it's heuristic - if I look far enough ahead I get the right answer but I have to try different
;; values of n to get it to work.

;; (time (find-path-take-n (parse-input small-input) 6))
;; "Elapsed time: 6.3845 msecs"
;; (1651 ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"])

;; (time (find-path-take-n (parse-input large-input) 150))
;; "Elapsed time: 966.1695 msecs"
;; (1857
;;  ["AA"
;;   "MA"
;;   "II"
;;   "AS"
;;   "RU"
;;   "PM"
;;   "KQ"
;;   "ED"
;;   "HR"
;;   "DW"
;;   "MW"
;;   "VI"
;;   "XO"
;;   "FQ"
;;   "LF"
;;   "TV"])



;; Part 2
;; Process two sets of valves at once, in 26 minutes.
;; Best ordering, for 1707 pressure relieved:
;; AA JJ BB CC
;; AA DD HH EE
(defn list-paths-2 [valves distances paths]
  (reverse (sort-by first (map (fn [[path1 path2]]
                                 (list (+ (compute-pressure-relieved valves distances path1 26)
                                          (compute-pressure-relieved valves distances path2 26))
                                       (list path1 path2)))
                               paths))))

(defn get-next-pairs [valves]
  (loop [coll valves result '()]
    (let [pairs (map (fn [x] [(first coll) x]) (rest coll))]
      (if (= 1 (count coll))
        (concat result (map (fn [coll] (reverse coll)) result))
        (recur (rest coll) (concat result pairs))))))

(defn extend-paths-2 [[path1 path2] key-valves]
;;  (println "extend-paths-2" path1 path2)
  (let [remaining-valves (remove (set (apply conj path1 path2)) key-valves)
        next-pairs (get-next-pairs remaining-valves)]
    (if (= (count remaining-valves) 1)
      (list (list (conj path1 (first remaining-valves)) path2)
            (list path1 (conj path2 (first remaining-valves))))
      (map (fn [[a b]] (list (conj path1 a) (conj path2 b))) next-pairs))))

(defn merge-path-lists [x y]
  ;; (println (first (first x)) (first (first y)))
  (cond 
      (empty? x) y
      (empty? y) x
      (> (first (first x)) (first (first y))) 
        (cons (first x) (lazy-seq (merge-path-lists y (rest x))))       
     :else 
      (cons (first y) (lazy-seq (merge-path-lists x (rest y))))))

(defn find-path-take-n-2 [valves n]
  (let [key-valves (get-key-valves valves)
        distances (compute-distances (conj key-valves "AA") valves)]
    (loop [iterations 0 path-pairs (list (list ["AA"] ["AA"]))]
      (println "iteration" iterations)
      ;; (println path-pairs)
      (if (or (> iterations 100)
              (= (count key-valves) (- (apply + (map count (first path-pairs))) 2)))
        (list iterations (first (list-paths-2 valves distances path-pairs)))
        (let [paths-with-costs
              (reduce (fn [paths-with-costs path-pair]
                        (let [new-paths (list-paths-2 valves distances (extend-paths-2 path-pair key-valves))
                              ;; _ (println new-paths)
                              ;; _ (println path-pair "=>" (extend-paths-2 path-pair key-valves))
                              ;; _ (println (concat paths-with-costs new-paths))
]
                          ;; (take n (merge-path-lists paths-with-costs new-paths))
                          (take n (reverse (sort-by first (concat paths-with-costs new-paths))))))
                      '()
                      path-pairs)
              new-paths (map second paths-with-costs)]
          ;; (println paths paths-with-costs)
          (recur (inc iterations) (take n new-paths)))))))


;; (time (find-path-take-n-2 (parse-input small-input) 2))
;; "Elapsed time: 3.6858 msecs"
;; (3 (1707 (["AA" "DD" "HH" "EE"] ["AA" "JJ" "BB" "CC"])))

;; (time (find-path-take-n-2 (parse-input large-input) 10000))
;; "Elapsed time: 411324.0313 msecs"
;; (8
;;  (2536
;;   (["AA" "HR" "DW" "XO" "VI" "MW" "FQ" "LF" "TV"]
;;    ["AA" "MA" "II" "AS" "RU" "PM" "KQ" "ED"])))
