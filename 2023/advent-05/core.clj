(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

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
