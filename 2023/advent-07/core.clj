(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def large-input-2 "large-input-2.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What are the total winnings?

(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #" "))
       (map (fn [[a b]] [a (parse-long b)]))
       (into {})))

(def card-scores {
                  \A 14
                  \K 13
                  \Q 12
                  \J 11
                  \T 10
                  \9 9
                  \8 8
                  \7 7
                  \6 6
                  \5 5
                  \4 4
                  \3 3
                  \2 2
                  })

(def type-scores {
                  :five-of-a-kind 6
                  :four-of-a-kind 5
                  :full-house 4
                  :three-of-a-kind 3
                  :two-pair 2
                  :one-pair 1
                  :high-card 0
                  })

(defn hand-type [hand]
  (let [counts (reverse (sort (vals (frequencies hand))))]
    (cond (= 5 (first counts)) :five-of-a-kind
          (= 4 (first counts)) :four-of-a-kind
          (= [3 2] counts) :full-house
          (= 3 (first counts)) :three-of-a-kind
          (= [2 2 1] counts) :two-pair
          (= [2 1 1 1] counts) :one-pair
          :else :high-card)))

(defn compare-hands [h1 h2]
  (let [type1 (hand-type h1)
        type2 (hand-type h2)]
    (if (not= type1 type2)
      (- (type1 type-scores) (type2 type-scores))
      (let [diff (drop-while zero? (map (fn [c1 c2] (- (get card-scores c1) (get card-scores c2))) h1 h2))]
        (if (empty? diff)
          0
          (first diff))))))

(defn total-winnings [input]
  (let [hand-map (parse-input input)
        hands (sort-by identity compare-hands (keys hand-map))
        indexed (map-indexed #(list (inc %1) %2) hands)]
    (reduce (fn [acc [i h]] (+ acc (* i (get hand-map h))))
            0
            indexed)))

;; (total-winnings small-input)
;; 6440

;; (total-winnings large-input)
;; 253954294



;; Part 2
;; Js are now jokers, worth 1 but able to mimic whatever card will make the best hand.
(def card-scores-2 {
                    \A 13
                    \K 12
                    \Q 11
                    \T 10
                    \9 9
                    \8 8
                    \7 7
                    \6 6
                    \5 5
                    \4 4
                    \3 3
                    \2 2
                    \J 1
                    })

(defn hand-type-with-joker [hand]
  (let [counts (vec (reverse (sort (vals (frequencies (remove #(= \J %) hand))))))
        joker-count (count (filter #(= \J %) hand))
        counts (if (= 5 joker-count)
                 [5] (assoc counts 0 (+ (get counts 0) joker-count)))]
    (cond (= 5 (first counts)) :five-of-a-kind
          (= 4 (first counts)) :four-of-a-kind
          (= [3 2] counts) :full-house
          (= 3 (first counts)) :three-of-a-kind
          (= [2 2 1] counts) :two-pair
          (= [2 1 1 1] counts) :one-pair
          :else :high-card)))

(defn compare-hands-2 [h1 h2]
  (let [type1 (hand-type-with-joker h1)
        type2 (hand-type-with-joker h2)]
    (if (not= type1 type2)
      (- (type1 type-scores) (type2 type-scores))
      (let [diff (drop-while zero? (map (fn [c1 c2] (- (get card-scores c1) (get card-scores c2))) h1 h2))]
        (if (empty? diff)
          0
          (first diff))))))

(defn total-winnings-2 [input]
  (let [hand-map (parse-input input)
        hands (sort-by identity compare-hands-2 (keys hand-map))
        indexed (map-indexed #(list (inc %1) %2) hands)
        _ (println "top ten" (drop 990 indexed))]
    (reduce (fn [acc [i h]] (+ acc (* i (get hand-map h))))
            0
            indexed)))

;; ---> answer <---
;; 255086951
