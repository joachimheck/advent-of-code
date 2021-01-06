(ns advent-19.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn parse-section [s]
  (let [cleaned (str/replace (str/trim s) "\"" "")]
    (cond (re-find #"[a-z]" cleaned) cleaned
          :else (map #(Integer/parseInt %) (str/split cleaned #" ")))))

(defn parse-rule [acc s]
  (let [[n & xs] (str/split s #":|\|")]
    (assoc acc
           (Integer/parseInt n)
           (map #(parse-section %) xs))))

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (assoc {}
           :rules (doall (reduce parse-rule {} (take-while #(< 0 (count %)) (line-seq rdr))))
           :messages (doall (reduce conj [] (line-seq rdr))))))

;; Algorithm: s matches r0 if s matches r0-1 as m1, s-m1 matches r0-2 as m2, etc.
(defn is-leaf? [r]
  (and (= 1 (count r)) (string? (first r))))

(def test-rules '{0 ((1 2) (2 1)) 1 ("a") 2 ("b")})

;; TODO: rewrite as for comprehension?
(defn match-rule
  "Returns nil for a mismatch, the leftover characters in s otherwise."
  ([rules s]
   (let [match (match-rule rules s 0 0)]
     (and (not (nil? match)) (empty? match))))
  ([rules s rnum lvl]
   (if (nil? s) nil
      (let [r (get rules rnum)]
        (println "match-rule" lvl s rnum r (if (is-leaf? r) "leaf" "not leaf"))
        (if (is-leaf? r)
          (when (str/starts-with? s (first r)) (first r))
          ;; process each rule list in 'r
          (reduce (fn [acc1 rule]
                    (println "acc1" acc1 "rule" rule)
;; TODO - don't return a boolean here, return the remainder of the string
;; problem - what if both match?                    
                    (or acc1
                        ;; process each rule in 'rule
                        (reduce (fn [acc2 subrule]
                                  (println "acc2" acc2 "subrule" subrule)
                                  (let [match (match-rule rules acc2 subrule (inc lvl))]
                                    (when match (subs acc2 1))))
                                s rule)))
                  false r))))))

;;(match-rule (get (read-input small-input) :rules) "ababbb")
