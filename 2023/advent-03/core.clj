(ns advent-03.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Find the sum of the numbers adjacent to a symbol.
;; (defn parse-line [l]
;;   (let [nums (map first (re-seq #"(\d+)" l))
;;         n-idxs (map #(vector % (str/index-of l %)) nums)
;;         syms (map first (re-seq #"([\#\$\%\&\*\+\-\/\=\@])" l))
;;         s-idxs (map #(vector % (str/index-of l %)) syms)]
;;     {:n-idxs n-idxs
;;      :s-idxs s-idxs}))

(defn add-digits [{nums :nums digits :digits :as state}]
  (if (empty? digits)
    state
    (assoc (update state :nums conj digits) :digits [])))

(defn assemble-num [xs]
  (list (str/join (map second xs)) (first (first xs))))

(defn parse-line [l]
  (let [reduced (reduce (fn [acc [i c :as e]]
                          (cond
                            (= \. c) (add-digits acc)
                            (re-matches #"[0-9]" (str c)) (update acc :digits conj e)
                            :else (-> acc
                                      (add-digits)
                                      (update :symbols conj e))))
                        {:nums '() :symbols '() :digits []}
                        (map-indexed list l))
        extracted (add-digits reduced)]
    {:n-idxs (map assemble-num (:nums extracted))
     :s-idxs (map reverse (:symbols extracted))}))

(defn adjacent-points [nx ny nl]
  (set
   (for [y (range (- ny 1) (+ ny 2))
         x (range (- nx 1) (+ nx nl 1))]
     [x y])))

(defn process-numbers [[y {n-idxs :n-idxs}]]
  (map (fn [[k v]] (list (parse-long k) (adjacent-points v y (count k)))) n-idxs))

(defn process-symbols [[y {s-idxs :s-idxs}]]
  (map (fn [[k v]] (list k [v y])) s-idxs))

(defn parse-input [input]
  (let [lines (->> (read-lines input)
                   (map parse-line)
                   (map-indexed list))
        num-ps (mapcat process-numbers lines)
        sym-ps (set (map (comp vec rest) (partition 3 (flatten (map process-symbols lines)))))]
    {:num-ps num-ps :sym-ps sym-ps}))

(defn adjacent? [[n n-ps] sym-ps]
  (seq (set/intersection n-ps sym-ps)))

(defn sum-symbol-adjacent [input]
  (let [state (parse-input input)]
    (->> (:num-ps state)
         (filter #(adjacent? % (:sym-ps state)))
         (map first)
         (apply +))))

;; 303127
;; 304281
;; ---> answer <---
;; I was using index-of to get the x positions, which mapped everything to the first instance.

;; (sum-symbol-adjacent small-input)
;; 4361

;; (sum-symbol-adjacent large-input)
;; 533784



;; Part 2
;; What is the sum of all of the gear ratios in your engine schematic?
(defn parse-input-2 [input]
  (let [lines (->> (read-lines input)
                   (map parse-line)
                   (map-indexed list))
        num-ps (mapcat process-numbers lines)
        sym-ps (set (map (fn [[s x y]] (list s [x y])) (partition 3 (flatten (map process-symbols lines)))))]
    {:num-ps num-ps :sym-ps sym-ps}))

(defn sum-gear-ratios [input]
  (let [state (parse-input-2 input)
        gears (filter #(= \* (first %)) (:sym-ps state))
        gears-and-adjacent (filter #(= 2 (count %))
                                   (for [[_ [x y]] gears]
                                     (map first (filter (fn [[n ps]] (contains? ps [x y])) (:num-ps state)))))]
    (apply + (map #(apply * %) gears-and-adjacent))))

;; (sum-gear-ratios small-input)
;; 467835

;; (sum-gear-ratios large-input)
;; 78826761
