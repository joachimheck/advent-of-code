(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 14: Space Stoichiometry

;; Part 1
;; How much ORE is required to produce exactly 1 FUEL?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-seq #"((\d+) ([A-Z]+),? ?)" %))
       (map (fn [line] (map #(drop 2 %) line)))
       (map (fn [pairs] (map (fn [[quantity chemical]] [(parse-long quantity) chemical]) pairs)))
       (map (fn [pairs] [(last pairs) (drop-last pairs)]))
       (into {})))

(defn find-reaction-producing [chemical reactions]
  (first (filter (fn [[[q c] v]] (= chemical c)) reactions)))

(defn get-ore-successors [reactions]
  (->> reactions
       (filter (fn [[output inputs]]
                 (and (= 1 (count inputs)) (= "ORE" (second (first inputs))))))
       (map first)
       (map second)
       (set)))

(defn smallest-integer-multiple [dividend divisor]
  (+ (quot dividend divisor)
     (if (> (rem dividend divisor) 0)
       1 0)))

(defn get-required-inputs [[needed chemical] reactions]
  (let [[[p-q _] inputs :as producer] (find-reaction-producing chemical reactions)
          multiple (if (> p-q needed)
                     1
                     (+ (quot needed p-q) (if (> (rem needed p-q) 0) 1 0)))]
      (map (fn [[i-q i-chemical]]
             [(* multiple i-q) i-chemical])
           inputs)))

(defn ore-required-for-one-fuel [reactions]
  (let [ore-successors (get-ore-successors reactions)
        needing-ores (loop [open-set '([1 "FUEL"])
                            needing-ores '()]
                       (if (empty? open-set)
                         needing-ores
                         (let 
                             [raw-inputs (apply concat (map #(get-required-inputs % reactions) open-set))
                              grouped (group-by #(nil? (some ore-successors (list (second %)))) raw-inputs)]
                           (recur (get grouped true) (concat needing-ores (get grouped false))))))
        basic-chemicals (apply merge-with + (map (fn [[k v]] {k v}) (map reverse needing-ores)))
        _ (println basic-chemicals)
        ores (apply concat (map (fn [[chemical needed]] (get-required-inputs [needed chemical] reactions)) basic-chemicals))]
    (apply + (map first ores))))

;; ---> answer <---
;; 2067236

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

(deftest test-ore-required-for-one-fuel
  (is (= 31 (ore-required-for-one-fuel (parse-input small-input))))
  (is (= 165 (ore-required-for-one-fuel (parse-input small-input-2))))
  (is (= 13312 (ore-required-for-one-fuel (parse-input small-input-3))))
  (is (= 180697 (ore-required-for-one-fuel (parse-input small-input-4))))
  (is (= 2210736 (ore-required-for-one-fuel (parse-input small-input-5)))))

;; TODO: I think I'm allocating extra input chemicals all through the process. Whenever two reactions
;; share an input chemical, the inputs can be distributed between them, but I'm treating them separately
;; and overallocating.
;; Build a tree with the required numbers of each chemical, then add the numbers for each chemical
;; to find the amounts of the precursor chemicals required. ?
