(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 12: Subterranean Sustainability

;; Part 1
;; After 20 generations, what is the sum of the numbers of all pots which contain a plant?
(defn parse-input [f]
  (let [[[header] _ instructions] (partition-by #(= "" %) (read-lines f))
        pots (vec (last (str/split header #" ")))
        inst-map (into {} (map (fn [s] (let [[pattern result] (rest (re-matches #"([\.\#]{5}) => ([\.\#])" s))]
                                         {(vec pattern) (first result)}))
                               instructions))]
    {:pots pots :instructions inst-map :leftmost 0}))

(defn advance-state-once [pots instructions]
  (let [pots (cond (= (get pots 0) \#) (apply conj [\. \.] pots)
                   (= (get pots 1) \#) (apply conj [\.] pots)
                   :else pots)
        pots (cond (= (get pots (- (count pots) 1)) \#) (apply conj pots [\. \.])
                   (= (get pots (- (count pots) 2)) \#) (apply conj pots [\.])
                   :else pots)
        pot-patterns (vec
                      (for [i (range (count pots))]
                        (vec
                         (for [j (range -2 3)]
                           (get pots (+ i j) \.)))))]
    (vec
     (for [i (range (count pot-patterns))]
       (get instructions (get pot-patterns i) \.)))))

(defn advance-state [initial-state generations]
  (let [{:keys [pots instructions leftmost]} initial-state]
    (loop [pots pots
           leftmost leftmost
           gen 0]
      (if (= gen generations)
        {:pots pots :leftmost leftmost}
        (let [extended-pots (cond (= (get pots 0) \#) (apply conj [\. \.] pots)
                                  (= (get pots 1) \#) (apply conj [\.] pots)
                                  :else pots)
              new-leftmost (cond (= (get pots 0) \#) (- leftmost 2)
                                 (= (get pots 1) \#) (- leftmost 1)
                                 :else leftmost)]
          ;; (println (format "%2d: %s%s" gen (str/join (repeat (+ 3 leftmost) ".")) (str/join pots)))
          (recur (advance-state-once extended-pots instructions) new-leftmost (inc gen)))))))

(defn sum-planted-pot-numbers [initial-state generations]
  (let [{:keys [pots leftmost]} (advance-state initial-state generations)]
    (->> pots
         (map-indexed #(list (+ %1 leftmost) %2))
         (filter #(= \# (second %)))
         (map first)
         (apply +))))

;; (time (sum-planted-pot-numbers (parse-input small-input) 20))
;; "Elapsed time: 5.7848 msecs"
;; 325

;; (time (sum-planted-pot-numbers (parse-input large-input) 20))
;; "Elapsed time: 6.1669 msecs"
;; 3248



;; Part 2
;; What about after 50 billion generations?
;; (time (sum-planted-pot-numbers (parse-input large-input) 50))
;; "Elapsed time: 14.2855 msecs"
;; 4658

;; (time (sum-planted-pot-numbers (parse-input large-input) 500))
;; "Elapsed time: 354.2847 msecs"
;; 40000

;; (time (sum-planted-pot-numbers (parse-input large-input) 5000))
;; "Elapsed time: 25855.8781 msecs"
;; 400000

;; 4000000000000? Yes!
