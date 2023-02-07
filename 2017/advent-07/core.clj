(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 7: Recursive Circus

;; Part 1
;; What is the name of the bottom program in the tower?
(defn parse-line [line]
  (let [[_ name weight _ above] (re-find #"(.+) \((\d+)\)( -> )?(.+)?" line)
        above (if above (re-seq #"[a-z]+" above))]
    {name {:weight (parse-long weight) :above above}}))

(defn parse-input [f]
  (->> f
       read-lines
       (map parse-line)
       (apply merge)))

(defn find-bottom-program [programs]
  (let [names (keys programs)
        aboves (set (apply concat (map :above (vals programs))))]
    (first (filter #(not (contains? aboves %)) names))))

;; (time (find-bottom-program (parse-input small-input)))
;; "Elapsed time: 0.7225 msecs"
;; "tknk"

;; (time (find-bottom-program (parse-input large-input)))
;; "Elapsed time: 10.3264 msecs"
;; "mkxke"



;; Part 2
;; What should be the weight of the one program whose weight is incorrect?
(defn get-weight [name programs]
  (let [p (get programs name)
        aboves (get p :above)]
    (if aboves
      (+ (:weight p) (apply + (map #(get-weight % programs) aboves)))
      (:weight p))))

(defn balanced? [name programs]
  (let [p (get programs name)
        aboves (get p :above)]
    (or (nil? aboves)
        (apply = (map #(get-weight % programs) aboves)))))

(defn find-last-unbalanced
  ([programs]
   (find-last-unbalanced (find-bottom-program programs) programs))
  ([node-name programs]
   (let [node (get programs node-name)
         aboves (group-by #(balanced? % programs) (:above node))
         unbalanced (first (get aboves false))]
     (if unbalanced
       (find-last-unbalanced unbalanced programs)
       node-name))))

(defn find-required-weight [nodes programs]
  (let [weighted-nodes (group-by #(get-weight % programs) nodes)
        weights (keys weighted-nodes)
        odd-node (first (second (first (filter #(= 1 (count (second %))) weighted-nodes))))
        other-node (first (second (first (filter #(not= 1 (count (second %))) weighted-nodes))))
        odd-node-weight (get-weight odd-node programs)
        other-node-weight (get-weight other-node programs)
        odd-node-local-weight (:weight (get programs odd-node))
        difference (- other-node-weight odd-node-weight)]
    (list odd-node (+ odd-node-local-weight difference))))

;; (let [programs (parse-input small-input)
;;                       nodes (:above (get programs (find-last-unbalanced programs)))]
;;                   (time (find-required-weight nodes programs)))
;; "Elapsed time: 0.0723 msecs"
;; ("ugml" 60)

;; (let [programs (parse-input large-input)
;;                       nodes (:above (get programs (find-last-unbalanced programs)))]
;;                   (time (find-required-weight nodes programs)))
;; "Elapsed time: 0.1799 msecs"
;; ("gexwzw" 268)
