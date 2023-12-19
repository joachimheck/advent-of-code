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
                      (fn [part] (if ((load-string op) (get part category) (parse-long value)) destination))
                      (fn [part] r))))
          rules)]))

(defn parse-part [s]
  (into {} (map (fn [[_ k v]] [k (parse-long v)]) (re-seq #"(.)=(\d+)" s))))

(defn parse-input [input]
  (let [[s-workflows _ s-parts] (partition-by #{""} (read-lines input))]
    {:workflows (into {} (map parse-workflow s-workflows)) :parts (map parse-part s-parts)}))

(defn sort-part [part workflows]
  (loop [workflow "in"]
    (let [destination (first (remove nil? (map #(% part) (get workflows workflow))))
          ;; _ (println "workflow" workflow
          ;;            "workflows" (get workflows workflow)
          ;;            "destinations" (map #(% part) (get workflows workflow)))
          ]
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
