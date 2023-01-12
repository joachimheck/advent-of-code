(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 14: Extended Polymerization

;; Part 1
;; Find the difference in counts of the most and least frequently occurring elements in the polymer after 10 steps.
(defn parse-input [f]
  (let [[[template] _ rule-lines] (partition-by #{""} (read-lines f))]
    {:rules (apply merge
                   (map (fn [s] (let [[_ [a b] [c]] (re-find #"([A-Z]+) -> ([A-Z]+)" s)]
                                  {[a b] (list [a c b])}))
                        rule-lines))
     :template template}))

(defn polymerize
  ([f n]
   (let [{rules :rules template :template} (parse-input f)]
     (polymerize template rules n)))
  ([template rules n]
   (loop [polymer template i 0]
     (if (= i n)
       polymer
       (let [pairs (partition 2 1 polymer)
             expansions (map #(first (rules %)) pairs)]
         (recur (conj (vec (mapcat drop-last expansions)) (last (last expansions))) (inc i)))))))

(defn score-polymer [polymer]
  (let [sorted (sort-by second (frequencies polymer))]
    (- (second (last sorted)) (second (first sorted)))))

(defn expand-and-score-template [f n]
  (let [{rules :rules template :template} (parse-input f)
        polymer (polymerize template rules n)]
    (score-polymer polymer)))

;; (time (expand-and-score-template small-input 10))
;; "Elapsed time: 10.9944 msecs"
;; 1588

;; (time (expand-and-score-template large-input 10))
;; "Elapsed time: 63.902 msecs"
;; 2740



;; Part 2
;; Same thing, but after 40 steps.
(def rules (atom {}))

(def cache-hits (atom 0))

(def cache-misses (atom 0))

(defn parse-input-atom [f]
  (reset! rules {})
  (let [[[template] _ rule-lines] (partition-by #{""} (read-lines f))]
    (reset! rules (apply merge
                         (map (fn [s] (let [[_ [a b] [c]] (re-find #"([A-Z]+) -> ([A-Z]+)" s)]
                                        {[a b] [a c b]}))
                              rule-lines)))
    template))

(defn join-polymers [polymers]
  (conj (mapcat rest polymers) (first (first polymers))))

(defn polymerize-recursive [polymer]
  (if-let [production (get @rules polymer)]
    (do
      (swap! cache-hits #(inc %))
      production)
    (let [size (count polymer)
          half (/ size 2)
          halves (if (even? size)
                   (list (take half polymer) (drop half polymer))
                   (list (take half polymer) (drop (dec half) polymer)))
          r-production (if (odd? size)
                         (join-polymers (map #(polymerize-recursive %) halves))
                         (concat (vec (polymerize-recursive (first halves)))
                                 (vector (second (get @rules [(last (first halves)) (first (second halves))])))
                                 (vec (polymerize-recursive (second halves)))))]
      (if (< size 6)
        (reset! rules (assoc @rules polymer r-production)))
      (swap! cache-misses #(inc %))
      r-production)))

(defn polymerize-repeatedly [template n]
  (loop [polymer template i 0]
    (if (= i n)
      polymer
      (recur (polymerize-recursive polymer) (inc i)))))

(defn expand-and-score-recursive [f n]
  (let [template (parse-input-atom f)]
    {:score (score-polymer (polymerize-repeatedly template n))
     :cache-hit-percentage (int (* 100 (/ @cache-hits (+ @cache-hits @cache-misses))))}))

;; ^-- This doesn't work. --^
;; Splitting each polymer in half and recursing is actually slower than processing by pairs.


;; What about keeping a map of what each pair generates at each iteration? Then look up by the pair and the remaining iterations.

;; Or maybe just pre-compute several levels of production?

(defn pairs [s]
  (apply concat
   (for [c s]
     (map #(list c %) (remove #{c} s)))))

(defn expand-rules [initial-rules depth]
  (reduce 
   (fn [result pair] (assoc result pair (for [i (range (inc depth))] (polymerize pair initial-rules i))))
   {}
   (keys initial-rules)))

(defn polymerize-multi [polymer rules n]
  (let [rules-depth (dec (count (second (first rules))))]
   (if (= n 0)
     polymer
     (let [pairs (partition 2 1 polymer)]
       (if (<= n rules-depth)
         (let [expansions (map #(nth (rules %) n) pairs)]
           (join-polymers expansions))
         (let [expansions (map #(nth (rules %) rules-depth) pairs)]
           (recur (join-polymers expansions) rules (- n rules-depth))))))))

(defn expand-and-score-multi [f n]
  (let [{template :template initial-rules :rules} (parse-input f)
        rules (expand-rules initial-rules 9)]
    (score-polymer (polymerize-multi template rules n))))

;; ^-- This doesn't work --^



;; Try keeping track of the scores, at each level, of productions from pairs.
(defn expand-polymer [polymer rules]
  (mapcat #(rules %) (partition 2 1 polymer)))

(def score-cache (atom {}))

(defn initial-scores [rules]
  (reduce-kv (fn [r k [[v1 v2] [_ v4]]]
               (assoc r k [(frequencies k) (frequencies [v1 v2 v4])]))
             {}
             rules))

(defn parse-input-2 [f]
  (let [[[template] _ rule-lines] (partition-by #{""} (read-lines f))
        rules (apply merge
                     (map (fn [s] (let [[_ [a b] [c]] (re-find #"([A-Z]+) -> ([A-Z]+)" s)]
                                    {[a b] (list [a c] [c b])}))
                          rule-lines))]
    {:rules rules
     :template template}))

(defn overlap [pairs]
  (frequencies (map first (drop 1 pairs))))

(defn add [a b]
  (reduce-kv (fn [r k v] (update r k #((fnil + 0 0) % v))) a b))

(defn subtract [a b]
  (reduce-kv (fn [r k v] (update r k #(- % v))) a b))

(defn add-score [scores pair new-score depth]
  (update scores pair #(assoc % depth new-score)))

(defn score-polymer-2 [polymer depth rules]
  (if (= depth 0) (frequencies polymer)
   (if-let [score (get-in @score-cache [polymer depth])]
     score
     (let [expanded (expand-polymer polymer rules)
           overlap (overlap expanded)
           pair-scores (map (fn [pair] (list pair (score-polymer-2 pair (dec depth) rules))) expanded)
           score (subtract (reduce add (map second pair-scores)) overlap)]
       (reset! score-cache (reduce (fn [r [k v]] (add-score r k v (dec depth)))
                                   @score-cache
                                   pair-scores))
       score))))

(defn simplify-score [score]
  (let [sorted (sort-by second score)]
    (- (second (last sorted)) (second (first sorted)))))

(defn expand-and-score-template-2 [f n]
  (reset! score-cache {})
  (let [{template :template initial-rules :rules} (parse-input-2 f)
        score (score-polymer-2 template n initial-rules)]
    (simplify-score score)))

;; (time (expand-and-score-template-2 small-input 40))
;; "Elapsed time: 11.763 msecs"
;; 2188189693529

;; (time (expand-and-score-template-2 large-input 40))
;; "Elapsed time: 52.2143 msecs"
;; 2959788056211
