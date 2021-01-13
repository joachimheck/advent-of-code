(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def small-input-2 "resources/small-input-2.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (let [splits (map str/split-lines (str/split (slurp f) #"\R\R"))]
    (map (fn [[p & cards]] (list (str/replace p ":" "")
                                 (vec (map #(Long/parseLong %) cards))))
         splits)))

(read-input small-input)

;; Part 1
;; Play a game of Combat. Report the winning player's score.

(defn combat [d1 d2]
  (cond (empty? d1) (list 1 d2)
        (empty? d2) (list 0 d1)
        :else
        (let [c1 (first d1)
              c2 (first d2)]
          (if (> c1 c2)
            (combat (conj (subvec d1 1) c1 c2)
                    (subvec d2 1))
            (combat (subvec d1 1)
                    (conj (subvec d2 1) c2 c1))))))


(defn score-deck [deck]
  (map-indexed (list %1 %2) (reverse deck)))

(defn play-and-score [init-state game]
  (let [players (map first init-state)
        winner (apply game (map second init-state))]
    (str (nth players (first winner))
         " wins with "
         (->> winner
              second
              reverse
              (map-indexed #(* (inc %1) %2))
              (reduce +)))))

;; (play-and-score (read-input small-input) combat)
;; => "Player 2 wins with 306"

;; (play-and-score (read-input large-input) combat)
;; "Player 1 wins with 32598"



;; Part 2
(defn recursive-combat
  "Returns a list of the winning player and the winner's deck"
  ([d1 d2] (recursive-combat d1 d2 #{} 0 0))
  ([d1 d2 game] (recursive-combat d1 d2 #{} game 0))
  ([d1 d2 prev game round]
   (println "game" game "round" round)
   (cond (some #{(list d1 d2)} prev) (list 0 d1)
         (empty? d1) (list 1 d2)
         (empty? d2) (list 0 d1)
         :else
         (let [c1 (first d1)
               c2 (first d2)]
           (let [winner
                 (if (and (>= (count (subvec d1 1)) c1)
                          (>= (count (subvec d2 1)) c2))
                   (first (recursive-combat (subvec d1 1) (subvec d2 1) (inc game)))
                   (if (> c1 c2) 0 1))]
             (if (= winner 0)
               (recursive-combat (conj (subvec d1 1) c1 c2)
                                 (subvec d2 1)
                                 (conj prev (list d1 d2))
                                 game (inc round))
               (recursive-combat (subvec d1 1)
                                 (conj (subvec d2 1) c2 c1)
                                 (conj prev (list d1 d2))
                                 game (inc round))))))))


;; (play-and-score (read-input small-input) recursive-combat)
;; => "Player 2 wins with 291"

(play-and-score (read-input small-input-2) recursive-combat)
(play-and-score (read-input large-input) recursive-combat)
