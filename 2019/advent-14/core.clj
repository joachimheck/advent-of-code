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

(defn get-required-inputs-integer [[needed chemical] reactions]
  ;; (println "get-required-inputs" needed chemical)
  (let [[[p-q _] inputs :as producer] (find-reaction-producing chemical reactions)
          multiple (if (> p-q needed)
                     1
                     (+ (quot needed p-q) (if (> (rem needed p-q) 0) 1 0)))]
      (map (fn [[i-q i-chemical]]
             [(* multiple i-q) i-chemical])
           inputs)))

(defn get-required-inputs [[needed chemical] reactions]
  ;; (println "get-required-inputs" needed chemical)
  (let [[[p-q _] inputs :as producer] (find-reaction-producing chemical reactions)
        multiple (if (> p-q needed)
                   1
                   (/ needed p-q))]
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
;; (build-tree '([1 "FUEL"]) reactions)
(defn build-tree
  [[amount chemical :as parent] reactions]
   ;; (println "build-tree" parent)
   (if (= chemical "ORE")
     '()
     (concat (list parent) (map #(build-tree % reactions) (get-required-inputs parent reactions)))))

(defn get-precursor-counts [tree]
  (let [sequenced (tree-seq #(and (seq %) (not (empty? (rest %)))) rest tree)
        leaves (filter #(and (= (count %) 2) (empty? (second %))) sequenced)
        grouped (group-by second (map first leaves))]
    (map (fn [[chemical pairs]]
           (list (apply + (map first pairs)) chemical))
         grouped)))


;; FUEL
;; STKFG FUEL
;; MNCFX FUEL
;; VJHF FUEL
;; HVMC FUEL
;; CXFTF FUEL
;; GNMV FUEL
;; VPVL STKFG
;; FWMGM STKFG
;; CXFTF STKFG
;; MNCFX STKFG
;; NVRVD VPVL
;; JNWZP VPVL
;; VJHF FWMGM
;; MNCFX FWMGM
;; ORE NVRVD
;; ORE JNWZP
;; MNCFX HVMC
;; RFSQX HVMC
;; FWMGM HVMC
;; VPVL HVMC
;; CXFTF HVMC
;; VJHF GNMV
;; MNCFX GNMV
;; VPVL GNMV
;; CXFTF GNMV
;; ORE MNCFX
;; NVRVD CXFTF
;; VJHF RFSQX
;; MNCFX RFSQX
;; ORE VJHF

(defn get-required-and-leftovers [[needed chemical] leftovers reactions]
  ;; (println "get-required-and-leftovers" needed chemical)
  (let [[[p-q _] inputs :as producer] (find-reaction-producing chemical reactions)
        leftover-chemical (get leftovers chemical 0)
        actually-needed (max (- needed leftover-chemical) 0)
        _ (if (< actually-needed needed) (println "Need" needed chemical "actually need only" actually-needed))
        new-leftover (max (- leftover-chemical actually-needed) 0)
        multiple (if (> p-q actually-needed)
                   1
                   (+ (quot actually-needed p-q) (if (> (rem actually-needed p-q) 0) 1 0)))
        leftover (- (* multiple p-q) actually-needed)]
    {:required (map (fn [[i-q i-chemical]] [(* multiple i-q) i-chemical]) inputs)
     :new-leftovers (merge-with + (assoc leftovers chemical new-leftover) {chemical leftover})}))

(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         leftovers {}]
    (println "loop" open-set leftovers)
    (let [non-ores (remove #(= (second %) "ORE") open-set)]
      (if (empty? non-ores)
        (apply + (map first open-set))
        (let [{:keys [required new-leftovers]} (get-required-and-leftovers (first non-ores) leftovers reactions)]
          (recur (concat (rest open-set) required)
                 new-leftovers))))))


;; advent-14.core> (compute-ore (parse-input small-input))
;; loop ([1 FUEL]) {}
;; loop ([7 A] [1 E]) {FUEL 0}
;; loop ([1 E] [10 ORE]) {FUEL 0, A 3}
;; loop ([10 ORE] [7 A] [1 D]) {FUEL 0, A 3, E 0}
;; Need 7 A actually need only 4

;; Shouldn't the next iteration have ([1 D] [10 ORE] [10 ORE])? We get that one iteration later.

;; loop ([7 A] [1 D] [10 ORE]) {FUEL 0, A 6, E 0}
;; Need 7 A actually need only 1
;; loop ([1 D] [10 ORE] [10 ORE]) {FUEL 0, A 14, E 0}
;; loop ([10 ORE] [10 ORE] [7 A] [1 C]) {FUEL 0, A 14, E 0, D 0}
;; Need 7 A actually need only 0
;; loop ([10 ORE] [7 A] [1 C] [10 ORE]) {FUEL 0, A 24, E 0, D 0}
;; Need 7 A actually need only 0
;; loop ([7 A] [1 C] [10 ORE] [10 ORE]) {FUEL 0, A 34, E 0, D 0}
;; Need 7 A actually need only 0
;; loop ([1 C] [10 ORE] [10 ORE] [10 ORE]) {FUEL 0, A 44, E 0, D 0}
;; loop ([10 ORE] [10 ORE] [10 ORE] [7 A] [1 B]) {FUEL 0, A 44, E 0, D 0, C 0}
;; Need 7 A actually need only 0
;; loop ([10 ORE] [10 ORE] [7 A] [1 B] [10 ORE]) {FUEL 0, A 54, E 0, D 0, C 0}
;; Need 7 A actually need only 0
;; loop ([10 ORE] [7 A] [1 B] [10 ORE] [10 ORE]) {FUEL 0, A 64, E 0, D 0, C 0}
;; Need 7 A actually need only 0
;; loop ([7 A] [1 B] [10 ORE] [10 ORE] [10 ORE]) {FUEL 0, A 74, E 0, D 0, C 0}
;; Need 7 A actually need only 0
;; loop ([1 B] [10 ORE] [10 ORE] [10 ORE] [10 ORE]) {FUEL 0, A 84, E 0, D 0, C 0}
;; loop ([10 ORE] [10 ORE] [10 ORE] [10 ORE] [1 ORE]) {FUEL 0, A 84, E 0, D 0, C 0, B 0}
;; 41
