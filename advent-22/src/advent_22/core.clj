(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (let [splits (map str/split-lines (str/split (slurp f) #"\R\R"))]
    (map (fn [[p & cards]] (list (str/replace p ":" "")
                                 (vec (map #(Long/parseLong %) cards))))
         splits)))

(read-input small-input)

;; Part 1
;; Play a game of Combat. Report the winning player's score.

(defn play-combat [d1 d2]
  (cond (empty? d1) (list 1 d2)
        (empty? d2) (list 0 d1)
        :else
        (let [c1 (first d1)
              c2 (first d2)]
          (if (> c1 c2)
            (play-combat (conj (subvec d1 1) c1 c2)
                         (subvec d2 1))
            (play-combat (subvec d1 1)
                         (conj (subvec d2 1) c2 c1))))))


(defn score-deck [deck]
  (map-indexed (list %1 %2) (reverse deck)))

(defn play-and-score [input]
  (let [players (map first input)
        winner (apply play-combat (map second input))]
    (str (nth players (first winner))
         " wins with "
         (->> winner
              second
              reverse
              (map-indexed #(* (inc %1) %2))
              (reduce +)))))

;; (play-and-score (read-input small-input))
;; => "Player 2 wins with 306"

;; (play-and-score (read-input large-input))
;; "Player 1 wins with 32598"



;; Part 2
