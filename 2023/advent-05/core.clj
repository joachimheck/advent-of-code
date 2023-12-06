(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the lowest location number that corresponds to any of the initial seed numbers?
(defn parse-input [input]
  (let [parts (->> (read-lines input)
                   (filter #(not= "" %))
                   (partition-by #(re-matches #".+map:" %)))
        seeds (map parse-long (re-seq #"\d+" (first (first parts))))
        maps (map (fn [[a b]]
                    (conj (map #(map parse-long (str/split % #" ")) b)
                          (rest (re-matches #"(.+)-to-(.+) map:" (first a)))))
                  (partition 2 (rest parts)))]
    {:seeds seeds
     :maps maps}))

(defn map-number [n maps]
  (let [[d s r :as m] (first (filter (fn [[d s r]] (<= s n (dec (+ s r)))) maps))]
    (if (nil? m)
      n
      (+ d (- n s)))))

(defn find-next [type n maps]
  (let [m (first (filter #(= type (first (first %))) maps))
        next-type (second (first m))
        next-n (map-number n (rest m))]
    [next-type next-n]))

(defn find-location [type n maps]
  (if (= type "location")
    n
    (let [[new-type new-n] (find-next type n maps)]
      (recur new-type new-n maps))))

(defn find-min-location [input]
  (let [{:keys [seeds maps]} (parse-input input)]
    (apply min
     (for [s seeds]
       (find-location "seed" s maps)))))

;; (find-min-location small-input)
;; 35

;; (find-min-location large-input)
;; 251346198



;; Part 2
;; What is the lowest location number that corresponds to any of the initial seed numbers?
(defn convert-map-element [[d s r]]
  (let [in-range [s (dec (+ s r))]
        diff (- d s)]
   {
    :input in-range
    :output (mapv #(+ diff %) in-range)
    :diff diff
    }))

(defn convert-map [n-map]
  (mapv convert-map-element n-map))

(defn parse-input-2 [input]
  (let [parts (->> (read-lines input)
                   (filter #(not= "" %))
                   (partition-by #(re-matches #".+map:" %)))
        seeds (map parse-long (re-seq #"\d+" (first (first parts))))
        maps (mapv (fn [[a b]]
                     (list (rest (re-matches #"(.+)-to-(.+) map:" (first a)))
                           (convert-map (map #(map parse-long (str/split % #" ")) b))))
                   (partition 2 (rest parts)))]
    {:seeds seeds
     :maps maps}))

(defn has-overlap? [[a b] [c d]]
  (or (<= c a d) (<= a c b)))

(defn get-overlap [r1 r2]
  (let [[[a b] [c d]] (sort (list r1 r2))]
    (cond (= r1 r2) (list r1)
          (and (< a c) (< d b)) (list [a (dec c)] [c d] [(inc d) b])
          (= a c) (list [a (min b d)] [(inc (min b d)) (max b d)])
          (= b d) (list [a (dec c)] [c d])
          (= b c) (list [a (dec b)] [b b] [(inc b) d])
          (< c b) (list [a (dec c)] [c b] [(inc b) d]))))

(defn partition-overlaps [r1 r2]
  (reduce (fn [acc r]
            (cond (and (has-overlap? r r1) (has-overlap? r r2)) (update acc :both conj r)
                  (has-overlap? r r1) (update acc :a conj r)
                  (has-overlap? r r2) (update acc :b conj r)
                  :else acc))
          {:a '() :b '() :both '()}
          (get-overlap r1 r2)))

(deftest test-get-overlap
  (is (= '([1 5]) (get-overlap [1 5] [1 5])))
  (is (= '([1 4] [5 5] [6 10]) (get-overlap [1 5] [5 10])))
  (is (= '([1 3] [4 5] [6 10]) (get-overlap [1 5] [4 10])))
  (is (= '([1 2] [3 5]) (get-overlap [1 5] [3 5])))
  (is (= '([1 4] [5 8] [9 10]) (get-overlap [1 10] [5 8])))
  (is (= '([6 8] [9 10]) (get-overlap [6 10] [6 8]))))

(defn any-overlap? [r rs]
  (some true? (map (partial has-overlap? r) rs)))

(defn get-overlaps [ranges]
  (reduce (fn [acc [a b]]
            (if (any-overlap? [a b] acc)
              (mapcat #(if (has-overlap? [a b] %) (get-overlap [a b] %) (list %)) acc)
              (conj acc [a b])))
          [(first (sort ranges))]
          (rest (sort ranges))))

;; seed-soil ranges: [50 97] -> [52 99], [98 99] -> [50 51] 
;; soil-fertilzer ranges: [0 14] -> [39 53], [15 51] -> [0 36], [52 53] -> [37 38]
;; seed-fertilizer ranges: [0 14] -> [39 53], [15 49] -> [0 34], [50 51] -> [37 38], [52 97] -> [54 99], [98 99] -> [35 36]

;; if an output range of map1 overlaps with an input range of map2, combine those ranges

(defn find-diff [r map1 map2]
  (let [in-diff (or (:diff (first (filter #(has-overlap? r (:input %)) map1))) 0)
        out-diff (or (:diff (first (filter #(has-overlap? (map (partial + in-diff) r) (:input %)) map2))) 0)]
    (+ in-diff out-diff)))

(defn build-map [r diff]
  {:input r :output (mapv #(+ diff %) r) :diff diff})

(defn collapse-maps [input]
  (let [state (parse-input-2 input)
        map1 (second (get (:maps state) 0))
        map2 (second (get (:maps state) 1))
        {overlap true no-overlap false} (group-by #(any-overlap? % (map :input map2)) (map :output map1))]
    (for [r-out overlap
          r-in (map :input map2)
          :let [partitions (partition-overlaps r-out r-in)]
          both-p (:both partitions)]
      (let [in (first (filter #(has-overlap? both-p (:output %)) map1))
            out (first (filter #(has-overlap? both-p (:input %)) map2))
            diff-total (+ (:diff in) (:diff out))
            new-out (mapv #(+ diff-total %) both-p)]
        {:input both-p :output new-out :diff diff-total}
))))



;; (let [state (parse-input-2 small-input)
;;                       map1 (second (get (:maps state) 0))
;;                       map2 (second (get (:maps state) 1))
;;                       overlaps (get-overlaps (concat (map :output map1) (map :input map2)))]
;;                   (map #(build-map % (find-diff % map1 map2))
;;                        ))
