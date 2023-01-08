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
(def rules (atom {}))

(defn parse-input [f]
  (reset! rules {})
  (let [[[template] _ rule-lines] (partition-by #{""} (read-lines f))]
    (reset! rules (apply merge
                         (map (fn [s] (let [[_ [a b] [c]] (re-find #"([A-Z]+) -> ([A-Z]+)" s)]
                                        {[a b] [a c b]}))
                              rule-lines)))
    template))

(defn polymerize
  ([f n]
   (let [template (parse-input f)]
     (polymerize template rules n)))
  ([template rules n]
   ;; (println "polymerize" template n)
   (loop [polymer template i 0]
     (if (= i n)
       polymer
       (let [elements (seq polymer)
             pairs (partition 2 1 polymer)
             expansions (map #(rules %) pairs)]
         (recur (conj (vec (mapcat drop-last expansions)) (last (last expansions))) (inc i)))))))

(defn score-polymer [polymer]
  (let [sorted (sort-by second (frequencies polymer))]
    (- (second (last sorted)) (second (first sorted)))))

(defn expand-and-score-template [f n]
  (let [template (parse-input f)]
    (score-polymer (polymerize template @rules n))))

;; (time (expand-and-score-template small-input 10))
;; "Elapsed time: 10.9944 msecs"
;; 1588

;; (time (expand-and-score-template large-input 10))
;; "Elapsed time: 63.902 msecs"
;; 2740



;; Part 2
;; Same thing, but after 40 steps.
(defn join-polymers [polymers]
  ;;(println "joining" polymers)
  (conj (vec (mapcat drop-last polymers)) (last (last polymers))))

(defn polymerize-recursive [polymer]
  ;; (println "recurse" (str/join polymer))
  (if (= 2 (count polymer))
    (get @rules polymer)
    (let [size (count polymer)
          half (/ size 2)
          halves (if (even? size)
                   (list (take half polymer) (drop half polymer))
                   (list (take half polymer) (drop (dec half) polymer)))]
      (if (odd? size)
        (join-polymers (map #(polymerize-recursive %) halves))
        (concat (vec (polymerize-recursive (first halves)))
                (vector (second (get @rules [(last (first halves)) (first (second halves))])))
                (vec (polymerize-recursive (second halves))))))))

(defn polymerize-repeatedly [template n]
  (loop [polymer template i 0]
    (if (= i n)
      polymer
      (recur (polymerize-recursive polymer) (inc i)))))

(defn expand-and-score-recursive [f n]
  (let [template (parse-input f)]
    (score-polymer (polymerize-repeatedly template n))))

; NCNBCH => NBCCNBBBCBH
