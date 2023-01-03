(ns day-13.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Find the happiest seating arrangement


(def test-input
  '(
    "Alice would gain 54 happiness units by sitting next to Bob."
    "Alice would lose 79 happiness units by sitting next to Carol."
    "Alice would lose 2 happiness units by sitting next to David."
    "Bob would gain 83 happiness units by sitting next to Alice."
    "Bob would lose 7 happiness units by sitting next to Carol."
    "Bob would lose 63 happiness units by sitting next to David."
    "Carol would lose 62 happiness units by sitting next to Alice."
    "Carol would gain 60 happiness units by sitting next to Bob."
    "Carol would gain 55 happiness units by sitting next to David."
    "David would gain 46 happiness units by sitting next to Alice."
    "David would lose 7 happiness units by sitting next to Bob."
    "David would gain 41 happiness units by sitting next to Carol."
    ))

(def puzzle-input (str/split-lines (slurp "input-13.txt")))

(defn parse-line [line]
  (let [[_ subject dir happiness object]
        (re-matches
         #"(\w+) would (gain|lose) (\d+) happi.+to (\w+)."
         line)]
    (assoc {} subject (assoc {} object (* (if (= dir "gain") 1 -1) (Long/parseLong happiness))))))

(defn get-rules [lines]
  (apply merge-with conj (map parse-line lines)))

(defn combo [vals]
  (if (empty? vals)
    '(())
    (for [v vals
          more (combo (remove #(= % v) vals))]
      (cons v more))))

(defn neighbors [person people]
  (let [index (.indexOf people person)
        size (count people)]
    (list (nth people (mod (inc index) size))
          (nth people (mod (dec index) size)))))

(defn score-orderings [input rules-f]
  (let [rules (rules-f input)
        names (distinct (map first rules))]
    (->> names
         combo
         (map (fn [people]
                (list people (map (fn [person] (list person (neighbors person people))) people))))
         (map (fn [[people person-and-neighbors]]
                (list people
                      (map (fn [[person [n1 n2]]]
                             (list (get (get rules person) n1) (get (get rules person) n2)))
                           person-and-neighbors))))
         (map (fn [[people scores]]
                (list people (reduce + (flatten scores)))))
         (sort-by second)
         reverse
         )))

;; (time (first (score-orderings test-input get-rules)))
;; => (("David" "Carol" "Bob" "Alice") 330)
;; "Elapsed time: 3.1497 msecs"

;; (time (first (score-orderings puzzle-input get-rules)))
;; => (("Mallory" "George" "David" "Carol" "Bob" "Eric" "Alice" "Frank") 618)
;; "Elapsed time: 2525.7675 msecs"



;; Part 2
;; Include myself, with relationships of zero.

(defn get-rules-plus-me [lines]
  (let [rules (get-rules lines)
        names (distinct (map first rules))
        me-added-to-them (reduce
                          (fn [acc name]
                            (assoc acc name
                                   (merge-with conj (get acc name) {"me" 0})
                                   )
                            
                            )
                          rules
                          names)
        them-added-to-me (assoc me-added-to-them "me"
                                (reduce
                                 (fn [acc name] (assoc acc name 0))
                                 {}
                                 names))

        ]
    them-added-to-me
    ))

;; (time (first (score-orderings test-input get-rules-plus-me)))
;; => (("me" "David" "Carol" "Bob" "Alice") 286)
;; "Elapsed time: 7.2625 msecs"

;; (time (first (score-orderings puzzle-input get-rules-plus-me)))
;; => (("David" "Bob" "Carol" "me" "Eric" "Alice" "Frank" "Mallory" "George") 601)
;; "Elapsed time: 25687.7335 msecs"
