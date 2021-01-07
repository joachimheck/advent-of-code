(ns advent-19.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def small-input-2 "resources/small-input-2.txt")
(def large-input "resources/large-input.txt")
(def large-input-2 "resources/large-input-2.txt")

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
(defn rule-type [r]
  (if (= 1 (count r))
    (if (string? (first r))
      :leaf ; ("a")
      :single) ; ((1 2))
    :multiple)) ; ((1 2) (3 4))

(defn match-leaf [s rs]
  (when (str/starts-with? s (first rs)) (subs s 1)))

(defn match-single
  ;; Match each rule list in 'rule, in order
  [rules s rule lvl]
  ;; (println (repeat lvl "  ") "match-seq-rs" s rule)
  (reduce (fn [acc2 subrule]
            ;; (println (repeat lvl "  ") "acc2" acc2 "subrule" subrule)
            (let [match (match-rule rules acc2 subrule (inc lvl))]
              ;; (println (repeat lvl "  ") "match?" match "new-acc" match)
              ;; (when match (subs acc2 (count match)))))
              (when match match)))
          s rule))

(defn match-multiple
  [rules s rs lvl]
  ;; process each rule list in 'rs
  (let [x (doall (map #(match-single rules s % lvl) rs))]
    ;; (println (repeat lvl "  ") "match-multiple" "x" x)
    ;; (if (> (count (keep identity x)) 1) (println "Multiple matches found!" x))
    ;; TODO: somehow return all the matches.
    (first (keep identity x))))

(defn match-rule
  "Returns nil for a mismatch, the leftover characters in s otherwise."
  ([rules s] (= "" (match-rule rules s 0 0)))
  ([rules s rnum lvl]
   (if (nil? s) nil
       (let [rs (get rules rnum)]
         ;; (println (repeat lvl "  ") "match-rule" s rnum "=" rs "type" (rule-type rs)))
         (case (rule-type rs)
           :leaf (match-leaf s rs)
           :single (match-single rules s (first rs) lvl)
           :multiple (match-multiple rules s rs lvl)
           )))))

(def test-rules '{0 ((1 3)) 1 ((2 2) (2 3)) 2 ("a") 3 ("b")})

;; (let [input (read-input small-input)] (map #(list % (match-rule (get input :rules) %)) (get input :messages)))
;; (("ababbb" true)
;;  ("bababa" false)
;;  ("abbbab" true)
;;  ("aaabbb" false)
;;  ("aaaabbb" false))
;; (let [input (read-input small-input)] (reduce #(if (match-rule (get input :rules) %2) (inc %1) %1) 0 (get input :messages)))
;; 2

;; (let [input (read-input large-input)] (reduce #(if (match-rule (get input :rules) %2) (inc %1) %1) 0 (get input :messages)))
;; 180
