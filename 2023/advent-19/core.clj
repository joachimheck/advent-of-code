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
  (if (some #{start} ["A" "R"])
    (list start)
    (conj (map (fn [{:keys [category op value destination] :as rule}]
                 (let [range (if op (build-range op value) [1 4001])]
                   [category
                    (if category range)
                    (build-tree workflows destination)]))
               (get workflows start))
          start)))

(def ^:dynamic range-max 4000)

(defn get-minmax [{:keys [category op value] :as constraint}]
  (let [minmax {"x" {:min 1 :max range-max}
                "m" {:min 1 :max range-max}
                "a" {:min 1 :max range-max}
                "s" {:min 1 :max range-max}}
        result (if category
                 (assoc minmax category (case op
                                          "<" {:min 1 :max (dec value)}
                                          ">" {:min (inc value) :max range-max}
                                          nil {}))
                 minmax)]
    (println "get-minmax" constraint result)
    result))

;; TODO: keep track of the values already excluded by previous rules.
(defn count-matching [{:keys [category value] :as rule} constraints]
  (let [ranges (reduce (fn [acc {c-cat :category :as constraint}]                        
                        ;; (println "category" category "c-cat" c-cat "constraint" constraint)
                        (if c-cat
                          (let [old (get acc c-cat)
                                minmax (get-minmax constraint)
                                new (get minmax c-cat)]
                            (println "old" old "minmax" minmax)
                            (assoc acc c-cat {:min (max (:min old) (:min new))
                                              :max (min (:max old) (:max new))}))
                          acc))
                      (get-minmax rule)
                      constraints)]
    (println "count-matching" "rule" rule "constraints" constraints ranges ranges)
    (apply min
           (map (fn [[_ {:keys [min max]}]]
                  (inc (- max min)))
                ranges))))

(deftest test-count-matching
  (binding [range-max 2]
    (is (= 1 (count-matching {:category "s" :op ">" :value 1} [])))
    (is (= 1 (count-matching {:category "s" :op ">" :value 1} [{:category "m", :op "<", :value 2}])))
    (is (= 0 (count-matching {:category "s" :op ">" :value 1} [{:category "s", :op "<", :value 2}])))
    (is (= 0 (count-matching {:destination "R"} [{:category "s", :op ">", :value 1} {:category "s", :op "<", :value 2}]))))
  )

(defn invert-constraint [constraint]
  ;; (println "invert-constraint" constraint)
  (if (:value constraint)
    (-> constraint
        (update :op {">" "<" "<" ">"})
        (assoc :value (inc (- range-max (:value constraint)))))
    {}))

(defn walk-tree
  ([workflows id rule-num] (walk-tree workflows id rule-num []))
  ([workflows id rule-num constraints]
   ;; (println "walk-tree" id rule-num constraints)
   (let [workflow (get workflows id)
         rules (drop rule-num workflow)]
     (if (empty? rules)
       0
       (let [rule (first rules)
             constraint (dissoc rule :destination)
             match-count (count-matching rule constraints)
             no-match-count (- range-max match-count)
             ;; _ (println "walk-tree" rule match-count no-match-count)
             ;; _ (println "inverse of" constraint "is" (invert-constraint constraint))
             _ (if (= "A" (:destination rule)) (println "found path" constraints constraint match-count))
             ]
         (+ (* match-count
               (case (:destination rule)
                 "A" 1
                 "R" 0
                 (walk-tree workflows (:destination rule) 0 (vec (remove empty? (conj constraints constraint))))))
            (* no-match-count (walk-tree workflows id (inc rule-num) (vec (remove empty? (conj constraints (invert-constraint constraint))))))))))))


;; Getting the wrong answer. Here's test-input in a form suitable for https://https://csacademy.com/app/graph_editor/
;; in px s<2
;; in qqz s>1
;; px R a<2
;; px px2 a>1
;; px2 A m>1
;; px2 px3 m<2
;; px3 R
;; qqz R s>1
;; qqz qqz2 s<2
;; qqz2 A m<2
;; qqz2 qqz3 m>1
;; qqz3 R
;; (binding [range-max 2] (walk-tree (:workflows (parse-input test-input)) "in" 0)) ---> 3, should be 1

;; (binding [range-max 2] (walk-tree (:workflows (parse-input test-input)) "in" 0)) ---> 0, should be 1
