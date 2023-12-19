(ns advent-19.core)

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

;; Part 1
;; What is the sum of the rating numbers the the accepted parts?
(defn parse-workflow [s]
  (let [[_ name s-rule] (re-matches #"([a-z]+)\{(.+)\}"s)
        rules (str/split s-rule #",")]
    [name
     (map (fn [r] (let [[_ category op value destination :as match] (re-matches #"([a-z])([><])(\d+):(\w+)" r)]
                    (if match
                      {:category category :op op :value (parse-long value) :destination destination}
                      {:destination r})))
          rules)]))

(defn parse-part [s]
  (into {} (map (fn [[_ k v]] [k (parse-long v)]) (re-seq #"(.)=(\d+)" s))))

(defn parse-input [input]
  (let [[s-workflows _ s-parts] (partition-by #{""} (read-lines input))]
    {:workflows (into {} (map parse-workflow s-workflows)) :parts (map parse-part s-parts)}))

(defn apply-rule [rule part]
  (let [{:keys [category op value destination]} rule]
    (if op
      (if (= op ">")
        (if (> (get part category) value) destination)
        (if (< (get part category) value) destination))
      destination)))

(defn sort-part [part workflows]
  (loop [workflow "in"]
    (let [destination (first (remove nil? (map #(apply-rule % part) (get workflows workflow))))]
      (case destination
        nil :failure-no-destination
        "A" :accepted
        "R" :rejected
        (recur destination)))))

(defn sum-ratings [part]
  (+ (get part "x") (get part "m") (get part "a") (get part "s")))

(defn sort-parts [input]
  (let [{:keys [parts workflows]} (parse-input input)]
    (apply +
           (for [part parts
                 :when (= :accepted (sort-part part workflows))]
             (sum-ratings part)))))

;; (sort-parts small-input)
;; 19114

;; (sort-parts large-input)
;; 487623



;; Part 2
;; How many combinations of ratings will be accepted by the workflows?
(defn build-range [op value]
  (if (= op "<")
    [1 value]
    [(inc value) 4001]))

(defn opposite [[lo hi]]
  (if (= lo 1)
    [(inc hi) 4001]
    [1 (dec lo)]))

(defn build-tree [workflows start]
  (if (#{start} ["A" "R"])
    (list start)
    (conj (map (fn [{:keys [category op value destination]}]
                 (let [range (if op (build-range op value) [1 4001])]
                   [range
                   (build-tree workflows destination)]))
               (get workflows start))
          start)))

;; TODO: reduce each workflow to build up the decision tree of ranges.
;; (let [workflows (:workflows (parse-input small-input))]
;;                   (build-tree workflows "in"))
