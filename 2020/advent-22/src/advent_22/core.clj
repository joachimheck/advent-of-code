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
  (map-indexed #(list %1 %2) (reverse deck)))

(defn play-and-score [init-state game]
  (let [players (map first init-state)
        result (apply game (map second init-state))]
    (println result)
    (str (nth players (first result))
         " wins with "
         (->> result
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
  ([d1 d2 history game round]
;;   (println "game" game "round" round)
   (cond (empty? d1) (list 1 d2 :card-exhaustion)
         (empty? d2) (list 0 d1 :card-exhaustion)
         (some #{(list d1 d2)} history) (list 0 d1 :infinite-recursion)
         :else
         (let [c1 (first d1)
               c2 (first d2)]
           (let [winner
                 (if (and (> (count d1) c1) (> (count d2) c2))
                   (let [result (recursive-combat (subvec d1 1 (inc c1)) (subvec d2 1 (inc c2)) #{} (inc game) 0)]
                     ;; (println "Game" (inc game) "ended by" (last result))
                     (first result))
                   (if (> c1 c2) 0 1))]
             (if (= winner 0)
               (recur (conj (subvec d1 1) c1 c2)
                      (subvec d2 1)
                      (conj history (list d1 d2))
                      game (inc round))
               (recur (subvec d1 1)
                      (conj (subvec d2 1) c2 c1)
                      (conj history (list d1 d2))
                      game (inc round))))))))


;; (play-and-score (read-input small-input) recursive-combat)
;; => "Player 2 wins with 291"

;(play-and-score (read-input small-input-2) recursive-combat)

;(play-and-score (read-input large-input) recursive-combat)
;;(0 [46 6 42 8 33 20 50 27 40 23 49 21 36 9 43 25 31 19 48 39 45 29 22 16 12 2 1 4 47 38 35 17 44 37 32 3 41 34 30 18 26 10 28 24 15 11 14 7 13 5] :card-exhaustion)
;; "Player 1 wins with 35836"
