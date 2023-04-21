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
  ;; (println "find-reaction-producing" chemical "=>" (first (filter (fn [[[q c] v]] (= chemical c)) reactions)))
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

;; 733822
;; ---> answer <---
;; 2067236

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

;; (deftest test-ore-required-for-one-fuel
;;   (is (= 31 (ore-required-for-one-fuel (parse-input small-input))))
;;   (is (= 165 (ore-required-for-one-fuel (parse-input small-input-2))))
;;   (is (= 13312 (ore-required-for-one-fuel (parse-input small-input-3))))
;;   (is (= 180697 (ore-required-for-one-fuel (parse-input small-input-4))))
;;   (is (= 2210736 (ore-required-for-one-fuel (parse-input small-input-5)))))

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

(defn multiple [needed produced]
  (+ (quot needed produced) (if (zero? (rem needed produced)) 0 1)))

(defn get-required-and-available [[amount chemical] available reactions]
  (let [amount-available (get available chemical 0)]
    (if (>= amount-available amount)
      {:required '()
       :new-available (update available chemical #(- % amount))}
      (let [actual-amount (- amount amount-available)
            [[produced-amount _] inputs] (find-reaction-producing chemical reactions)
            m (multiple actual-amount produced-amount)]
        {:required (map (fn [[i-q i-chemical]] [(* m i-q) i-chemical]) inputs)
         :new-available (assoc available chemical 0)}))))

;; First try, works on all but small-input-5 and large-input.
(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         ores '()
         available {}]
    (println "loop" open-set ores available)
    (if (empty? open-set)
      {:ore (apply + (map first ores)) :available available}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
        (recur (concat (rest open-set) non-ores)
               (concat ores new-ores)
               new-available)))))


;; Thought I might need to think about which reactions depend on others. Same result, though further from the
;; right answer for small-input-5.
;; (defn precursors [chemical reactions]
;;   (let [[in out] (first (filter (fn [[[_ k] v]] (= k chemical)) reactions))]
;;     (map second out)))

;; (defn is-precursor? [chemical chemicals reactions]
;;   ;; (println "is-precursor?" chemical chemicals)
;;   (some #{chemical} (apply concat (map #(precursors % reactions) chemicals))))

;; (defn compute-ore [reactions]
;;   (loop [open-set '([1 "FUEL"])
;;          ores '()
;;          available {}]
;;     ;; (println "loop" open-set ores available)
;;     (if (empty? open-set)
;;       (list (apply + (map first ores)) available)
;;       ;; (apply + (map first ores))
;;       (let [
;;             current (first (remove empty? (map (fn [[[n c] ncs]]
;;                                                  (if (is-precursor? c (map second ncs) reactions) '() [n c]))
;;                                                (map #(list % (remove #{%} open-set)) open-set))))
;;             {:keys [required new-available]} (get-required-and-available current available reactions)
;;             {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
;;         ;; There could be multiple instances of current; just remove the first one.
;;         (recur (let [[n m] (split-with (partial not= current) open-set)]
;;                  (concat non-ores n (rest m)))
;;                (concat ores new-ores)
;;                new-available)))))

(defn is-basic? [chemical reactions]
  ;; (println "is-basic?" chemical (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))
  (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))

(defn sum-chemicals [chemicals reactions]
  (let [grouped (group-by second chemicals)]
    (map (fn [[chemical pairs]]
           (list (apply + (map first pairs)) chemical))
         grouped)))

(defn debug [x]
  (println "debug" x)
  x)

(defn compute-ore-from-basics [basics reactions]
  (as-> basics v
      (sum-chemicals v reactions)
      (map #(get-required-and-available % {} reactions) v)
      (map :required v)
      (apply concat v)
      (map first v)
      (apply + v)))

(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         basics '()
         available {}]
    ;; (println "loop" open-set basics available)
    (if (empty? open-set)
      {:basics basics :ore (compute-ore-from-basics basics reactions)}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-basics non-basics]} (group-by #(if (is-basic? (second %) reactions) :new-basics :non-basics) required)]
        (recur (concat (rest open-set) non-basics)
               (concat basics new-basics)
               new-available)))))

(deftest test-compute-ore
  (is (= 31 (:ore (compute-ore (parse-input small-input)))))
  (is (= 165 (:ore (compute-ore (parse-input small-input-2)))))
  (is (= 13312 (:ore (compute-ore (parse-input small-input-3)))))
  (is (= 180697 (:ore (compute-ore (parse-input small-input-4)))))
  (is (= 2210736 (:ore (compute-ore (parse-input small-input-5))))))

;; Suitable for https://csacademy.com/app/graph_editor/
(defn produce-graph-inputs [reactions]
  (apply concat (map (fn [[[_ k] vs]] (map (fn [[_ v]] (str v " " k)) vs)) reactions)))

(def test-input-1 "test-input-1.txt")
(def test-input-2 "test-input-2.txt")
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

;; 733822
;; ---> answer <---
;; 2067236

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

;; (deftest test-ore-required-for-one-fuel
;;   (is (= 31 (ore-required-for-one-fuel (parse-input small-input))))
;;   (is (= 165 (ore-required-for-one-fuel (parse-input small-input-2))))
;;   (is (= 13312 (ore-required-for-one-fuel (parse-input small-input-3))))
;;   (is (= 180697 (ore-required-for-one-fuel (parse-input small-input-4))))
;;   (is (= 2210736 (ore-required-for-one-fuel (parse-input small-input-5)))))

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

(defn multiple [needed produced]
  (+ (quot needed produced) (if (zero? (rem needed produced)) 0 1)))

(defn get-required-and-available [[amount chemical] available reactions]
  (let [amount-available (get available chemical 0)]
    (if (>= amount-available amount)
      {:required '()
       :new-available (update available chemical #(- % amount))}
      (let [actual-amount (- amount amount-available)
            [[produced-amount _] inputs] (find-reaction-producing chemical reactions)
            m (multiple actual-amount produced-amount)]
        {:required (map (fn [[i-q i-chemical]] [(* m i-q) i-chemical]) inputs)
         :new-available (assoc available chemical 0)}))))

;; First try, works on all but small-input-5 and large-input.
(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         ores '()
         available {}]
    (println "loop" open-set ores available)
    (if (empty? open-set)
      {:ore (apply + (map first ores)) :available available}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
        (recur (concat (rest open-set) non-ores)
               (concat ores new-ores)
               new-available)))))


;; Thought I might need to think about which reactions depend on others. Same result, though further from the
;; right answer for small-input-5.
;; (defn precursors [chemical reactions]
;;   (let [[in out] (first (filter (fn [[[_ k] v]] (= k chemical)) reactions))]
;;     (map second out)))

;; (defn is-precursor? [chemical chemicals reactions]
;;   ;; (println "is-precursor?" chemical chemicals)
;;   (some #{chemical} (apply concat (map #(precursors % reactions) chemicals))))

;; (defn compute-ore [reactions]
;;   (loop [open-set '([1 "FUEL"])
;;          ores '()
;;          available {}]
;;     ;; (println "loop" open-set ores available)
;;     (if (empty? open-set)
;;       (list (apply + (map first ores)) available)
;;       ;; (apply + (map first ores))
;;       (let [
;;             current (first (remove empty? (map (fn [[[n c] ncs]]
;;                                                  (if (is-precursor? c (map second ncs) reactions) '() [n c]))
;;                                                (map #(list % (remove #{%} open-set)) open-set))))
;;             {:keys [required new-available]} (get-required-and-available current available reactions)
;;             {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
;;         ;; There could be multiple instances of current; just remove the first one.
;;         (recur (let [[n m] (split-with (partial not= current) open-set)]
;;                  (concat non-ores n (rest m)))
;;                (concat ores new-ores)
;;                new-available)))))

(defn is-basic? [chemical reactions]
  ;; (println "is-basic?" chemical (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))
  (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))

(defn sum-chemicals [chemicals reactions]
  (let [grouped (group-by second chemicals)]
    (map (fn [[chemical pairs]]
           (list (apply + (map first pairs)) chemical))
         grouped)))

(defn debug [x]
  (println "debug" x)
  x)

(defn compute-ore-from-basics [basics reactions]
  (as-> basics v
      (sum-chemicals v reactions)
      (map #(get-required-and-available % {} reactions) v)
      (map :required v)
      (apply concat v)
      (map first v)
      (apply + v)))

(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         basics '()
         available {}]
    ;; (println "loop" open-set basics available)
    (if (empty? open-set)
      {:basics basics :ore (compute-ore-from-basics basics reactions)}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-basics non-basics]} (group-by #(if (is-basic? (second %) reactions) :new-basics :non-basics) required)]
        (recur (concat (rest open-set) non-basics)
               (concat basics new-basics)
               new-available)))))

(deftest test-compute-ore
  (is (= 31 (:ore (compute-ore (parse-input small-input)))))
  (is (= 165 (:ore (compute-ore (parse-input small-input-2)))))
  (is (= 13312 (:ore (compute-ore (parse-input small-input-3)))))
  (is (= 180697 (:ore (compute-ore (parse-input small-input-4)))))
  (is (= 2210736 (:ore (compute-ore (parse-input small-input-5))))))

;; Suitable for https://csacademy.com/app/graph_editor/
(defn produce-graph-inputs [reactions]
  (apply concat (map (fn [[[_ k] vs]] (map (fn [[_ v]] (str v " " k)) vs)) reactions)))

(def test-input-1 "test-input-1.txt")
(def test-input-2 "test-input-2.txt")

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

;; 733822
;; ---> answer <---
;; 2067236

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

;; (deftest test-ore-required-for-one-fuel
;;   (is (= 31 (ore-required-for-one-fuel (parse-input small-input))))
;;   (is (= 165 (ore-required-for-one-fuel (parse-input small-input-2))))
;;   (is (= 13312 (ore-required-for-one-fuel (parse-input small-input-3))))
;;   (is (= 180697 (ore-required-for-one-fuel (parse-input small-input-4))))
;;   (is (= 2210736 (ore-required-for-one-fuel (parse-input small-input-5)))))

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

(defn multiple [needed produced]
  (+ (quot needed produced) (if (zero? (rem needed produced)) 0 1)))

(defn get-required-and-available [[amount chemical] available reactions]
  (let [amount-available (get available chemical 0)]
    (if (>= amount-available amount)
      {:required '()
       :new-available (update available chemical #(- % amount))}
      (let [actual-amount (- amount amount-available)
            [[produced-amount _] inputs] (find-reaction-producing chemical reactions)
            m (multiple actual-amount produced-amount)]
        ;; (if (= "VPVL" chemical)
        ;;   (println [amount chemical] ":" amount-available "available, actual amount:" actual-amount
        ;;            "required:" (map (fn [[i-q i-chemical]] [(* m i-q) i-chemical]) inputs)
        ;;            "leftover:" (- (* m produced-amount) actual-amount)))
        {:required (map (fn [[i-q i-chemical]] [(* m i-q) i-chemical]) inputs)
         :new-available (assoc available chemical (- (* m produced-amount) actual-amount))}))))

;; First try, works on all but small-input-5 and large-input.
;; Update: this no longer works on any input.
(defn compute-ore [reactions]
  (loop [open-set '([1 "FUEL"])
         ores '()
         available {}]
    (println "loop" open-set ores available)
    (if (empty? open-set)
      {:ore (apply + (map first ores)) :available available}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
        (recur (concat (rest open-set) non-ores)
               (concat ores new-ores)
               new-available)))))


;; Thought I might need to think about which reactions depend on others. Same result, though further from the
;; right answer for small-input-5.
;; (defn precursors [chemical reactions]
;;   (let [[in out] (first (filter (fn [[[_ k] v]] (= k chemical)) reactions))]
;;     (map second out)))

;; (defn is-precursor? [chemical chemicals reactions]
;;   ;; (println "is-precursor?" chemical chemicals)
;;   (some #{chemical} (apply concat (map #(precursors % reactions) chemicals))))

;; (defn compute-ore [reactions]
;;   (loop [open-set '([1 "FUEL"])
;;          ores '()
;;          available {}]
;;     ;; (println "loop" open-set ores available)
;;     (if (empty? open-set)
;;       (list (apply + (map first ores)) available)
;;       ;; (apply + (map first ores))
;;       (let [
;;             current (first (remove empty? (map (fn [[[n c] ncs]]
;;                                                  (if (is-precursor? c (map second ncs) reactions) '() [n c]))
;;                                                (map #(list % (remove #{%} open-set)) open-set))))
;;             {:keys [required new-available]} (get-required-and-available current available reactions)
;;             {:keys [new-ores non-ores]} (group-by #(if (= (second %) "ORE") :new-ores :non-ores) required)]
;;         ;; There could be multiple instances of current; just remove the first one.
;;         (recur (let [[n m] (split-with (partial not= current) open-set)]
;;                  (concat non-ores n (rest m)))
;;                (concat ores new-ores)
;;                new-available)))))

(defn is-basic? [chemical reactions]
  ;; (println "is-basic?" chemical (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))
  (seq (filter (fn [[[_ k] [[_ v]]]] (and (= k chemical) (= v "ORE"))) reactions)))

(defn sum-chemicals [chemicals reactions]
  (let [grouped (group-by second chemicals)]
    (map (fn [[chemical pairs]]
           (list (apply + (map first pairs)) chemical))
         grouped)))

(defn debug [x]
  (println "debug" x)
  x)

(defn compute-ore-from-basics [basics reactions]
  (as-> basics v
      (sum-chemicals v reactions)
      (map #(get-required-and-available % {} reactions) v)
      (map :required v)
      (apply concat v)
      (map first v)
      (apply + v)))

(defn compute-ore-basic [reactions]
  (loop [open-set '([1 "FUEL"])
         basics '()
         available {}]
    ;; (println "loop" (sort-by second open-set) (sort-by second basics) available)
    (if (empty? open-set)
      {:basics basics :ore (compute-ore-from-basics basics reactions)}
      (let [current (first open-set)
            {:keys [required new-available]} (get-required-and-available current available reactions)
            {:keys [new-basics non-basics]} (group-by #(if (is-basic? (second %) reactions) :new-basics :non-basics) required)]
        (recur (sum-chemicals (concat (rest open-set) non-basics) reactions)
               (sum-chemicals (concat basics new-basics) reactions)
               new-available)))))

;; (deftest test-compute-ore
;;   (is (= 31 (:ore (compute-ore (parse-input small-input)))))
;;   (is (= 165 (:ore (compute-ore (parse-input small-input-2)))))
;;   (is (= 13312 (:ore (compute-ore (parse-input small-input-3)))))
;;   (is (= 180697 (:ore (compute-ore (parse-input small-input-4)))))
;;   (is (= 2210736 (:ore (compute-ore (parse-input small-input-5))))))

;; Suitable for https://csacademy.com/app/graph_editor/
(defn produce-graph-inputs [reactions]
  (apply concat (map (fn [[[_ k] vs]] (map (fn [[_ v]] (str v " " k)) vs)) reactions)))

(def test-input-1 "test-input-1.txt")
(def test-input-2 "test-input-2.txt")
(def test-input-3 "test-input-3.txt")

(defn reactions-graph [reactions]
  (into {} (map (fn [[[_ k] vs]] [k (map second vs)]) reactions)))

(defn find-preceding [chemical reaction-graph]
  (loop [open-set #{chemical}
           found #{}]
      ;; (println "loop" open-set found)
      (if (empty? open-set) found
          (let [current (first (seq open-set))
                new-found (get reaction-graph current '())]
            (recur (apply conj (disj open-set current) new-found)
                   (apply conj found new-found))))))

(defn first-without-preceding [chemicals reaction-graph]
  (first
   (first
    (filter #(nil? (second %))
            (for [chemical chemicals]
              (let [preceding (apply conj (map #(find-preceding % reaction-graph) (remove #{chemical} chemicals)))]
                (list chemical (some #{chemical} preceding))))))))

(defn next-reaction [reactions reaction-graph]
  (let [chemical (first-without-preceding (map second reactions) reaction-graph)]
    (first (filter #(= chemical (second %)) reactions))))

(defn compute-ore-preceding [reactions]
  (let [reaction-graph (reactions-graph reactions)]
    (loop [open-set '([1 "FUEL"])
           basics '()
           available {}]
      ;; (println "loop" (sort-by second open-set) (sort-by second basics) available)
      (if (empty? open-set)
        {:basics (sum-chemicals basics reactions) :ore (compute-ore-from-basics basics reactions)}
        (let [current (next-reaction open-set reaction-graph)
              ;; _ (println "current" current)
              {:keys [required new-available]} (get-required-and-available current available reactions)
              {:keys [new-basics non-basics]} (group-by #(if (is-basic? (second %) reactions) :new-basics :non-basics) required)]
          (recur (sum-chemicals (concat (let [[n m] (split-with (partial not= current) open-set)]
                                          (concat non-basics n (rest m))))
                                reactions)
                 (sum-chemicals (concat basics new-basics) reactions)
                 new-available))))))

(deftest test-compute-ore-preceding
  (is (= 31 (:ore (compute-ore-preceding (parse-input small-input)))))
  (is (= 165 (:ore (compute-ore-preceding (parse-input small-input-2)))))
  (is (= 13312 (:ore (compute-ore-preceding (parse-input small-input-3)))))
  (is (= 180697 (:ore (compute-ore-preceding (parse-input small-input-4)))))
  (is (= 2210736 (:ore (compute-ore-preceding (parse-input small-input-5))))))



;; OK, I had a bug in get-required-and-available, setting the leftovers to zero. I was thinking that I would
;; use all the leftovers, which is true, but then I make more than necessary so there are new leftovers.
;; With that fixed, compute-ore-basic is good enough.

;; (time (compute-ore-basic (parse-input small-input-5)))
;; "Elapsed time: 2.1613 msecs"
;; {:basics ((3045 "BHXH") (3201 "KTJDG") (32965 "VRPVC") (69552 "CNZTR")), :ore 2210736}

;; (time (compute-ore-preceding (parse-input small-input-5)))
;; "Elapsed time: 4.2294 msecs"
;; {:basics ((3045 "BHXH") (3201 "KTJDG") (32965 "VRPVC") (69552 "CNZTR")), :ore 2210736}

;; (time (compute-ore-preceding (parse-input large-input)))
;; "Elapsed time: 258.1051 msecs"
;; {:basics ((23794 "QLRXZ") (7048 "VWSRH") (27534 "TZWD") (2494 "DLPZ")), :ore 1046184}

;; (time (compute-ore-basic (parse-input large-input)))
;; "Elapsed time: 22.5037 msecs"
;; {:basics ((23794 "QLRXZ") (7048 "VWSRH") (27534 "TZWD") (2494 "DLPZ")), :ore 1046184}



;; Part 2
;; 
