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
  {[s (dec (+ s r))] (- d s)})

(defn convert-map [n-map]
  (into {} (mapv convert-map-element n-map)))

(defn parse-input-2 [input]
  (let [parts (->> (read-lines input)
                   (filter #(not= "" %))
                   (partition-by #(re-matches #".+map:" %)))
        seeds (map parse-long (re-seq #"\d+" (first (first parts))))
        maps (map (fn [[a b]]
                    (list (rest (re-matches #"(.+)-to-(.+) map:" (first a)))
                          (convert-map (map #(map parse-long (str/split % #" ")) b))))
                  (partition 2 (rest parts)))]
    {:seeds seeds
     :maps maps}))

(defn get-overlap [r1 r2]
  (let [[[a b] [c d]] (sort (list r1 r2))]
    (cond (= [a b] [c d]) (list [a b])
          (< b c) (list [a b] [c d])
          (= b c) (list [a (dec b)] [b b] [(inc c) d])
          (< c b d) (list [a (dec c)] [c b] [(inc b) d])
          (= b d) (list [a (dec c)] [c d])
          (> b d) (list [a (dec c)] [c d] [(inc d) b]))))

(deftest test-get-overlap
  (is (= '([1 5]) (get-overlap [1 5] [1 5])))
  (is (= '([1 5] [6 10]) (get-overlap [1 5] [6 10])))
  (is (= '([1 4] [5 5] [6 10]) (get-overlap [1 5] [5 10])))
  (is (= '([1 3] [4 5] [6 10]) (get-overlap [1 5] [4 10])))
  (is (= '([1 2] [3 5]) (get-overlap [1 5] [3 5])))
  (is (= '([1 4] [5 8] [9 10]) (get-overlap [1 10] [5 8]))))

(defn get-overlaps [ranges]
  (reduce (fn [acc [a b]]
            ;; (println "acc:" (seq acc))
            ;; (println "new acc:" (map #(get-overlap [a b] %) acc))
            (mapcat #(get-overlap [a b] %) acc))
          [(first (sort ranges))]
          (rest (sort ranges))))

;; TODO: doesn't work right.
;; (sort (get-overlaps '([1 5] [4 8] [6 10])))


(defn collapse-maps [maps]
  )
