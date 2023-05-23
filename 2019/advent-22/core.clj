(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 22: Slam Shuffle

;; Part 1
;; After shuffling, what is the position of card 2019?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map (fn [s] (re-matches #"(.+?) ?([-0-9]+)?" s)))
       (map rest)
       (map (fn [[op n]] (if (nil? n) [op n] [op (parse-long n)])))))

(defn deal-into-new-stack [deck]
  (vec (reverse deck)))

(defn cut [deck n]
  (if (>= n 0)
    (vec (concat (drop n deck) (take n deck)))
    (vec (concat (take-last (abs n) deck) (drop-last (abs n) deck)))))

(defn deal-with-increment [deck n]
  (let [table (:table (reduce (fn [{:keys [i table deck-size] :as acc} card]
                                (assoc acc :i (inc i) :table (assoc table (mod (* i n) deck-size) card)))
                              {:i 0
                               :table {}
                               :deck-size (count deck)}
                              deck))]
    (reduce (fn [acc i] (conj acc (get table i))) [] (range (count deck)))))

(defn shuffle [instructions initial-deck]
  (reduce (fn [deck [instruction n]]
            (cond (= instruction "deal into new stack")
                  (deal-into-new-stack deck)
                  (= instruction "cut")
                  (cut deck n)
                  (= instruction "deal with increment")
                  (deal-with-increment deck n)))
          initial-deck
          instructions))

(defn get-card-position [deck card]
  (loop [i 0]
    (if (= (get deck i) card)
      i
      (recur (inc i)))))

;; (shuffle (parse-input small-input) [0 1 2 3 4 5 6 7 8 9])
;; [0 3 6 9 2 5 8 1 4 7]
;; (time (shuffle (parse-input small-input) [0 1 2 3 4 5 6 7 8 9]))
;; "Elapsed time: 0.7822 msecs"
;; [0 3 6 9 2 5 8 1 4 7]
;; (time (shuffle (parse-input small-input-2) [0 1 2 3 4 5 6 7 8 9]))
;; "Elapsed time: 0.5439 msecs"
;; [3 0 7 4 1 8 5 2 9 6]
;; (time (shuffle (parse-input small-input-3) [0 1 2 3 4 5 6 7 8 9]))
;; "Elapsed time: 0.6641 msecs"
;; [6 3 0 7 4 1 8 5 2 9]
;; (time (shuffle (parse-input small-input-4) [0 1 2 3 4 5 6 7 8 9]))
;; "Elapsed time: 0.7962 msecs"
;; [9 2 5 8 1 4 7 0 3 6]

;; (time (get-card-position (shuffle (parse-input large-input) (vec (range 10007))) 2019))
;; "Elapsed time: 378.193 msecs"
;; 6978



;; Part 2
;; Shuffle 119315717514047 cards 101741582076661 times. What card is at position 2020?
;; Clearly we have to work backwards with card positions. That will handle the large number
;; of cards. I'm not sure how to handle the large number of shuffles yet. Hopefully there
;; will be some repetition or something.

(defn before-deal-into-new-stack [position card-count]
  (- card-count position 1))

(defn before-cut [position card-count n]
  (if (>= n 0)
    (if (< position (- card-count n))
      (+ position n)
      (- position (- card-count n)))
    (if (< position (abs n))
      (+ position (- card-count (abs n)))
      (- position (abs n)))))

(defn compute-cycle [n m increment]
  (first
   (for [i (range m)
         :when (= (mod (* i increment) m) n)]
     i)))

(defn before-deal-with-increment [position card-count n]
  (let [
        tuple (quot position n)
        position-in-tuple (mod position n)
        advancement (- n (rem card-count n))
        cycle (compute-cycle position-in-tuple n advancement)]
    ;; (list tuple position-in-tuple advancement cycle)
    (+ (+ (* cycle n) tuple) (if (= 0 (mod position n)) 0 1))))

;; I have the (hopefully correct) reverse functions. I still need to chain them together
;; in the reverse order of the given instructions, and figure out how to loop them.
