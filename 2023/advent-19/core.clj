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
    ;; (println "get-minmax" constraint result)
    result))

;; TODO: keep track of the values already excluded by previous rules.
(defn count-matching [{:keys [category value] :as rule} constraints]
  (let [ranges (reduce (fn [acc {c-cat :category :as constraint}]                        
                         ;; (println "category" category "c-cat" c-cat "constraint" constraint)
                         (if c-cat
                           (let [old (get acc c-cat)
                                 minmax (get-minmax constraint)
                                 new (get minmax c-cat)]
                             ;; (println "old" old "minmax" minmax)
                             (assoc acc c-cat {:min (max (:min old) (:min new))
                                               :max (min (:max old) (:max new))}))
                           acc))
                       (get-minmax rule)
                       constraints)]
    ;; (println "count-matching" "rule" rule "constraints" constraints "ranges" ranges "range-max" range-max)
    (apply *
           (map (fn [[_ {:keys [min max]}]]
                  (clojure.core/max 0 (inc (- max min))))
                ranges))))

(deftest test-count-matching
  (binding [range-max 2]
    (is (= 1 (count-matching {:category "x" :op ">" :value 1} (map (fn [c] {:category c :op ">" :value 1}) ["m" "a" "s"]))))
    (is (= 8 (count-matching {:category "s" :op ">" :value 1} [])))
    (is (= 4 (count-matching {:category "s" :op ">" :value 1} [{:category "m", :op "<", :value 2}])))
    (is (= 0 (count-matching {:category "s" :op ">" :value 1} [{:category "s", :op "<", :value 2}])))
    (is (= 0 (count-matching {:destination "R"} [{:category "s", :op ">", :value 1} {:category "s", :op "<", :value 2}]))))
  (is (= 255936000000000 (count-matching {:category "x" :op ">" :value 1} [])))
  (is (= 255872016000000 (count-matching {:category "x" :op ">" :value 1} [{:category "m" :op ">" :value 1}]))))

(defn invert-constraint [constraint]
  ;; (println "invert-constraint" constraint)
  (if (:value constraint)
    (-> constraint
        (update :op {">" "<" "<" ">"})
        (update :value #(if (= (:op constraint) ">")
                          (inc %)
                          (dec %))))
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
             ;; _ (if (= "A" (:destination rule)) (println "found path" constraints constraint match-count))
             ]
         (+ (* match-count
               (case (:destination rule)
                 "A" (bigint 1)
                 "R" (bigint 0)
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



(defn format-constraint [rule]
  (if (and (:category rule) (:op rule) (:value rule))
    (format "%s%s%d" (:category rule) (:op rule) (:value rule))))

(defn build-constraints
  ([workflows id rule-num] (build-constraints workflows id rule-num []))
  ([workflows id rule-num constraints]
   ;; (println "build-constraints" id rule-num constraints)
   (let [workflow (get workflows id)
         rules (drop rule-num workflow)]
     (if (empty? rules)
       ;; (list :no-rules id rule-num)
       '()
       (let [rule (first rules)
             constraint (assoc (dissoc rule :destination) :id id)
             match-count (count-matching rule constraints)
             no-match-count (- range-max match-count)
             ;; _ (println "build-constraints" rule match-count no-match-count)
             ;; _ (println "inverse of" constraint "is" (invert-constraint constraint))
             ;; _ (if (= "A" (:destination rule)) (println "found path" constraints constraint match-count))
             pos-constraints (vec (remove empty? (conj constraints constraint)))
             neg-constraints (vec (remove empty? (conj constraints (invert-constraint constraint))))
             outside (case (:destination rule)
                       "A" (list pos-constraints)
                       "R" '()
                       (remove empty? (build-constraints workflows (:destination rule) 0 pos-constraints)))
             inside (remove empty? (build-constraints workflows id (inc rule-num) neg-constraints))
             ;; _ (println "build-constraints" "id" id outside inside)
             ]
         (concat outside inside))))))

(defn count-combinations
  ([input] (count-combinations input 4000))
  ([input range-max-in]
   (binding [range-max range-max-in]
     (let [paths (build-constraints (:workflows (parse-input input)) "in" 0)
           ;; counts (doall
           ;;         (map (fn [[end constraints]]
           ;;                (list (map format-constraint constraints)
           ;;                      (count-matching {} constraints)))
           ;;              paths))
           ;; ids (for [path paths] (dedupe (map :id path)))
           path-counts (doall
                        (map (fn [constraints]
                               {:path (dedupe (map :id constraints))
                                :constraints (remove nil? (map format-constraint constraints))
                                :count (count-matching {} constraints)})
                             paths))
           ]
       ;; (list path-counts (apply + (map :count path-counts)))
       (apply + (map :count path-counts))))))

;; This is a bit more than half the correct value of 167409079868000.
;; (count-combinations small-input)
;; 85814301000000

;; 59820152970570
;; ---> answer <---

;; px qkq a<2006
;; px A m>2090
;; px rfg otherwise
;; pv R a>1716
;; pv A otherwise
;; lnx A m>1548
;; lnx A otherwise
;; rfg gd s<537
;; rfg R x>2440
;; rfg A otherwise
;; qs A s>3448
;; qs lnx otherwise
;; qkq A x<1416
;; qkq crn otherwise
;; crn A x>2662
;; crn R otherwise
;; in px s<1351
;; in qqz otherwise
;; qqz qs s>2770
;; qqz hdj m<1801
;; qqz R otherwise
;; gd R a>3333
;; gd R otherwise
;; hdj A m>838
;; hdj pv otherwise

;; (count-combinations small-input)
;; 167409079868000

;; (count-combinations large-input)
;; 113550238315130
