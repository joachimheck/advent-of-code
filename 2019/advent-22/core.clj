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

(defn mod-for-cycle [cycle card-count increment]
  (mod (- increment (* cycle (rem card-count increment))) increment))

(defn cycle-for-position [p card-count increment]
  (first
   (for [c (range increment)
         :when (= (mod p increment) (mod-for-cycle c card-count increment))]
     c)))

(defn card-count-in-cycle [cycle card-count increment]
  (+ 1 (quot (dec (- card-count (mod-for-cycle cycle card-count increment))) increment)))

(defn first-card-in-cycle [cycle card-count increment]
  (apply + (for [c (range cycle)] (card-count-in-cycle c card-count increment))))

;; (defn cards-in-cycle [cycle card-count increment]
;;   (let [start (apply + (for [c (range cycle)] (card-count-in-cycle c card-count increment)))]
;;     (range start
;;            (+ start (card-count-in-cycle cycle card-count increment)))))

;; (defn cards-in-cycle [cycle card-count increment]
;;   (let [cycle-size (+ 1 (quot (dec (- card-count (mod-for-cycle cycle card-count increment))) increment))
;;         start (apply + (for [c (range cycle)] cycle-size))]
;;     (range start (+ start cycle-size))))

(defn position-in-cycle [p cycle card-count increment]
  (/ (- p (mod-for-cycle cycle card-count increment)) increment))

(defn before-deal-with-increment [p card-count increment]
  (let [cycle (cycle-for-position p card-count increment)
        p-c (position-in-cycle p cycle card-count increment)]
    (+ (first-card-in-cycle cycle card-count increment) p-c)))

;; I have the (hopefully correct) reverse functions. I still need to chain them together
;; in the reverse order of the given instructions, and figure out how to loop them.
(defn reverse-shuffle [instructions position deck-size]
  (reduce (fn [p [instruction n]]
            (cond (= instruction "deal into new stack")
                  (before-deal-into-new-stack p deck-size)
                  (= instruction "cut")
                  (before-cut p deck-size n)
                  (= instruction "deal with increment")
                  (before-deal-with-increment p deck-size n)))
          position
          (reverse instructions)))

;; ---> answer <---
;; 71098221879215

(defn reverse-shuffle-multiple [instructions position deck-size times]
  (let [reversed-instructions (reverse instructions)]
    (let [loop-size (loop [p position
                           i 0]
                      (let [result (reverse-shuffle instructions p deck-size)]
                        (if (= result position)
                          i
                          (recur result (inc i)))))
          remainder (rem times loop-size)]
      (println "Found loop with size" loop-size "remainder" remainder)
      (loop [p position
             i 0]
        (if (= i remainder)
          p
          (recur (reverse-shuffle reversed-instructions p deck-size) (inc i)))))))

;; (time (reverse-shuffle-multiple (parse-input large-input) 6978 10007 50000))
;; "Elapsed time: 6796.226601 msecs"
;; 8316
;; (time (reverse-shuffle-multiple (parse-input large-input) 2020 119315717514047 101741582076661))

;; I think this code is correct and would eventually give the right answer, but it's hopelessly slow.

;; Reddit informs me that I need to use a solution I would never have come up with in a million years.
;; Describe the xth card in the deck as y = (ax + b) % deck-size
;; deal-with-increment increment: a' = deck-size - a
;; cut n: b' = b + n
;; deal-into-new-stack: a' = -a, b' = y(deck-size - 1)

(defn card-at [x a b deck-size]
  (mod (+ b (* a x)) deck-size))

(defn factors-deal-into-new-stack [a b deck-size]
  ;; (println "factors-deal-into-new-stack" a b deck-size)
  [(- a) (card-at (dec deck-size) a b deck-size)])

(defn factors-cut [n a b deck-size]
  [a (card-at n a b deck-size)])

(defn factors-deal-with-increment [n a b deck-size]
  [
   ;; (inc (quot deck-size n))
   ;; find the position of the card with (mod card deck-size) = 1
   ;; 
   (first
    (for [i (range deck-size)
          :let [p (mod (* i n) deck-size)]
          :when (= p 1)]
      (do
        ;; (println "p" p "card" (card-at i a b deck-size))
        (- (card-at i a b deck-size) b))))
   b])

;; 11 cards increment 2
;; (quot 11 2) = 5, (rem 11 2) = 1
;; C(1) = 6, the first card in cycle 1
;; (- 2 (- 5 1)) = (- 2 4) = -2

;; 11 cards increment 5
;; (quot 11 5) = 2 (rem 11 5) = 1
;; C(1) = 9, the first card in cycle 4
;; (- increment (- quotient remainder))?
;; starting position of cycle 1 is (mod (+ (- deck-size remainder) increment) deck-size)

(defn apply-factors [[a b] deck-size]
  (vec
   (for [i (range deck-size)]
     (card-at i a b deck-size))))

(deftest test-factors
  (is (= [9 8 7 6 5 4 3 2 1 0] (apply-factors (factors-deal-into-new-stack 1 0 10) 10)))
  (is (= [3 4 5 6 7 8 9 0 1 2] (apply-factors (factors-cut 3 1 0 10) 10)))
  (is (= [6 7 8 9 0 1 2 3 4 5] (apply-factors (factors-cut -4 1 0 10) 10)))
  (is (= [0 7 4 1 8 5 2 9 6 3] (apply-factors (factors-deal-with-increment 3 1 0 10) 10)))
  (is (= [6 9 2 5 8 1 4 7 0 3] (apply-factors (factors-deal-with-increment 7 1 6 10) 10))))

(defn shuffle-factors [instructions deck-size]
  (let [[a b] (reduce (fn [[a b] [instruction n]]
                        ;; (println "reduce-fn" a b instruction n)
                        (cond (= instruction "deal into new stack")
                              (factors-deal-into-new-stack a b deck-size)
                              (= instruction "cut")
                              (factors-cut n a b deck-size)
                              (= instruction "deal with increment")
                              (factors-deal-with-increment n a b deck-size)))
                      [1 0]
                      instructions)]
    ;; (println "a" a "b" b)
    (apply-factors [a b] deck-size)))

(deftest test-shuffle-factors
  (is (= [0 3 6 9 2 5 8 1 4 7] (shuffle-factors (parse-input small-input) 10)))
  (is (= [3 0 7 4 1 8 5 2 9 6] (shuffle-factors (parse-input small-input-2) 10)))
  (is (= [6 3 0 7 4 1 8 5 2 9] (shuffle-factors (parse-input small-input-3) 10)))
  (is (= [9 2 5 8 1 4 7 0 3 6] (shuffle-factors (parse-input small-input-4) 10))))

(defn factors-before-deal-into-new-stack [a b deck-size]
  [(- a) (card-at (dec deck-size) a b deck-size)])

(defn factors-before-cut [n a b deck-size]
  ;; [a (card-at n a b deck-size)]
  [a (card-at (mod (- deck-size n) deck-size) a b deck-size)])

(defn factors-before-deal-with-increment [n a b deck-size]
  [
   ;; (first
   ;;  (for [i (range deck-size)
   ;;        :let [p (mod (* i n) deck-size)]
   ;;        :when (= p 1)]
   ;;    (- (card-at i a b deck-size) b)))

   ;; (first (for [x (range 1 deck-size)
   ;;              r (range 1 deck-size)
   ;;              :when (= (* n x) (+ 1 (* deck-size r)))]
   ;;          x))
   (let [candidates (filter #(true? (last %))
                            (for [x (range 1 deck-size)
                                  r (range 1 deck-size)
                                  :when (= (* n x) (+ 1 (* deck-size r)))]
                              (list x r (= (* n x) (+ 1 (* deck-size r))))))
         ]
     (println "candidates" candidates)
     (first candidates))

   b])


(deftest test-factors-before
  (is (= [0 1 2 3 4 5 6 7 8 9] (apply-factors (factors-before-deal-into-new-stack -1 9 10) 10)))
  (is (= [0 1 2 3 4 5 6 7 8 9] (apply-factors (factors-before-cut 3 1 3 10) 10)))
  (is (= [0 1 2 3 4 5 6 7 8 9] (apply-factors (factors-before-cut -4 1 6 10) 10)))
  (is (= [0 1 2 3 4 5 6 7 8 9] (apply-factors (factors-before-deal-with-increment 3 7 0 10) 10)))
  (is (= [0 1 2 3 4 5 6 7 8 9] (apply-factors (factors-before-deal-with-increment 7 3 6 10) 10)))
)

;; n=5: (mod (* 5 9) 11) = 1
;; (45 - 44 = 1)
;; (- (* 5 9) (* 4 11)) = 1
;; (* 5 9) = (+ 1 (* 4 11))
;; 9 = (/ (+ 1 (* 4 11)) 5) ; fine, but where does the 4 come from?

;; If 5x mod 11 = 1, then 5x - 1 = 11r for some r. What's the lowest r?
;; http://www-math.ucdenver.edu/~wcherowi/courses/m5410/exeucalg.html
;; If 5x mod 11 = 1, then pX + 11s = 1

;; 45 mod 11 = 1
;; There exists some integer r such that 45 = 11r + 1
;; r = (45 - 1)/11 = 4
;; 5x mod 11 = 1
;; There exists some integers x,r (both < 11) such that 5x = 11r + 1



;; TODO: This doesn't work but my brain is too fried to figure out why:

;; (deal-with-increment [0 1 2 3 4 5 6 7 8 9 10] 5)
;; [0 9 7 5 3 1 10 8 6 4 2]
;; advent-22.core> (apply-factors [9 0] 11)
;; [0 9 7 5 3 1 10 8 6 4 2]
;; advent-22.core> (factors-deal-with-increment 5 1 0 11)
;; [9 0]
;; advent-22.core> (factors-before-deal-with-increment 5 9 0 11)
;; candidates ((9 4 true))
;; [(9 4 true) 0]
;; advent-22.core> 
