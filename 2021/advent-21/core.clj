(ns advent-21.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Dirac Dice

;; Part 1
;; Play a game of Dirac Dice and multiply the losing score by the number of die rolls.
(defn parse-input [f]
  (->> f
       read-lines
       (map #(re-find #": (\d+)" %))
       (map last)
       (mapv parse-long)))

(def deterministic-die (map #(inc (mod % 100)) (range)))

(defn move [space moves]
  (+ 1 (mod (+ moves (dec space)) 10)))

(def PLAYER_1 "Player 1")

(def PLAYER_2 "Player 2")

(defn play-deterministic-dice [[start-1 start-2] die]
  (loop [player PLAYER_1
         state {PLAYER_1 {:space start-1
                          :score 0}
                PLAYER_2 {:space start-2
                          :score 0}}
         die-rolls 0]
    ;; (println "loop" state)
    (let [space (get-in state [player :space])
          score (get-in state [player :score])
          moves (apply + (list (nth die die-rolls) (nth die (+ 1 die-rolls)) (nth die (+ 2 die-rolls))))
          new-space (move space moves)
          new-score (+ score new-space)
          new-die-rolls (+ 3 die-rolls)
          other-player (if (= player PLAYER_1) PLAYER_2 PLAYER_1)]
      (if (>= new-score 1000)
        (* new-die-rolls (get-in state [other-player :score]))
        (recur other-player
               (assoc state player {:space new-space :score new-score})
               new-die-rolls)))))

;; (time (play-deterministic-dice (parse-input small-input) deterministic-die))
;; "Elapsed time: 14.3931 msecs"
;; 739785

;; (time (play-deterministic-dice (parse-input large-input) deterministic-die))
;; "Elapsed time: 14.7537 msecs"
;; 920580



;; Part 2
;; Play with a Dirac die.
(def dirac-die-rolls
  '([1 1 1] [1 1 2] [1 1 3]
    [1 2 1] [1 2 2] [1 2 3]
    [1 3 1] [1 3 2] [1 3 3]
    [2 1 1] [2 1 2] [2 1 3]
    [2 2 1] [2 2 2] [2 2 3]
    [2 3 1] [2 3 2] [2 3 3]
    [3 1 1] [3 1 2] [3 1 3]
    [3 2 1] [3 2 2] [3 2 3]
    [3 3 1] [3 3 2] [3 3 3]))

;; {3 1, 4 3, 5 6, 6 7, 7 6, 8 3, 9 1}
(def dirac-results (let [die-rolls (apply concat (for [i (range 1 4)]
                                                   (apply concat (for [j (range 1 4)]
                                                                   (for [k (range 1 4)]
                                                                     [i j k])))))]
                     (frequencies (map #(apply + %) die-rolls))))
;;(def dirac-results {1 1, 5 1, 11 1})

(defn conj-add [a b]
  (let [[k v] (first b)]
    (if (some #{k} (keys a))
      (update a k #(+ % v))
      (conj a b))))

;; state: {[p1-space p2-space] -> {[p1-score p2-score] universes}}
(defn evolve-state
  "`player` is 0 for player 1, 1 for player 2."
  [state]
  (let [player (:player state)
        other-player (if (= player 0) 1 0)]
    {:state (apply merge-with conj-add
                   (for [[[p1-space p2-space :as spaces] state-scores] (:state state)]
                     (apply merge-with conj
                            (for [[[p1-score p2-score :as scores] universes] state-scores]
                              (if (< (apply max scores) 21)
                                (apply merge-with conj
                                       (for [[die-total die-total-count] dirac-results]
                                         (let [new-spaces (update spaces player #(move % die-total))
                                               new-scores (update scores player #(+ % die-total))]
                                           {new-spaces {new-scores (* universes die-total-count)}})))
                                {spaces {scores universes}})))))
     :player other-player}))

;; (evolve-state {:state {[4 8] {[0 0] 1}} :player 0})
;; {:state {[7 8] {[3 0] 1}, [8 8] {[4 0] 3}, [9 8] {[5 0] 6}, [10 8] {[6 0] 7}, [1 8] {[7 0] 6}, [2 8] {[8 0] 3}, [3 8] {[9 0] 1}}, :player 1}

(defn play-dirac-dice [[start-1 start-2]]
  (let [state {:state {[start-1 start-2] {[0 0] 1}} :player 0}]
    (loop [state state i 0]
      (if (= i 100)
        (assoc state :iterations  i)
        (let [new-state (evolve-state state)]
          (if (= (:state new-state) (:state state))
            (assoc state :iterations i)
            (recur new-state (inc i))))))))

(defn score-state [state]
  (->> state
       :state
       vals
       (apply concat)
       (map (fn [[[score-1 score-2] universes]] (list (if (> score-1 score-2) 0 1) universes)))
       (group-by first)
       vals
       (map #(map second %))
       (map #(apply + %))))

;; state: {[p1-space p2-space p1-score p2-score] -> universes}
(defn evolve-state-flatmap
  "`player` is 0 for player 1, 1 for player 2."
  [state]
  (let [player (:player state)
        other-player (if (= player 0) 1 0)
        state (:state state)]
    {:state (apply merge-with +
                   (apply concat
                          (for [[p1-space p2-space p1-score p2-score :as k] (keys state)]
                            (if (and (< p1-score 21) (< p2-score 21))
                              (let [universes (get state k)]
                                (for [[die-total die-total-count] dirac-results]
                                  (let [new-k [(if (= player 0) (move p1-space die-total) p1-space)
                                               (if (= player 1) (move p2-space die-total) p2-space)
                                               (if (= player 0) (+ p1-score die-total) p1-score)
                                               (if (= player 1) (+ p2-score die-total) p2-score)]]
                                    {new-k (* universes die-total-count)})))
                              (list {k (get state k)})))))
     :player other-player}))

(defn play-dirac-dice-flatmap [[start-1 start-2]]
  (let [state {:state {[start-1 start-2 0 0] 1} :player 0}]
    (loop [state state i 0]
      (if (= i 100)
        (assoc state :iterations  i)
        (let [new-state (evolve-state-flatmap state)]
          (if (= (:state new-state) (:state state))
            (assoc state :iterations i)
            (recur new-state (inc i))))))))

(defn score-state-flatmap [state]
  (->> state
       :state
       (group-by (fn [[[_ _ p1-score p2-score] _]] (if (> p1-score p2-score) 0 1)))
       vals
       (map #(map second %))
       (mapv #(apply + %))))

(def die-value (atom 1))

(defn deterministic-results []
  (println "rolling" @die-value (+ 1 @die-value) (+ 2 @die-value))
  (let [result (+ (* 3 @die-value) 3)]
    (swap! die-value #(inc (mod (dec (+ 3 %)) 100)))
    {result 1}))

;; initial-state {:active-games {[4 8 0 0] 1} :finished-games {} :player 0}
(defn evolve-state-flatmap-reduce
  "`player` is 0 for player 1, 1 for player 2."
  [state]
  (let [player (:player state)
        other-player (if (= player 0) 1 0)
        active-games (:active-games state)
        max-score 21
        ;; max-score 1000
        ]
    (assoc (reduce (fn [acc [[p1-space p2-space p1-score p2-score :as k] universes]]
                     ;; (println "reduce-fn-1" acc k universes)
                     (reduce (fn [acc [die-total die-total-count]]
                               ;; (println "reduce-fn-2" acc [die-total die-total-count])
                               (let [new-p1-space (if (= player 0) (move p1-space die-total) p1-space)
                                     new-p2-space (if (= player 1) (move p2-space die-total) p2-space)
                                     new-p1-score (if (= player 0) (+ p1-score new-p1-space) p1-score)
                                     new-p2-score (if (= player 1) (+ p2-score new-p2-space) p2-score)
                                     new-k [new-p1-space new-p2-space new-p1-score new-p2-score]
                                     new-game {new-k (* universes die-total-count)}
                                     game-key (if (and (< new-p1-score max-score) (< new-p2-score max-score))
                                                :active-games
                                                :finished-games)]
                                 (update acc game-key #(merge-with + % new-game))))
                             acc
                             dirac-results
                             ;;(deterministic-results)
                             ))
                   {:finished-games (:finished-games state)}
                   active-games)
           :player other-player)))

(defn play-dirac-dice-flatmap-reduce [[start-1 start-2]]
  (let [state {:active-games {[start-1 start-2 0 0] 1} :player 0}]
    (loop [state state i 0]
      (if (= i 500)
        (assoc state :iterations  i)
        (let [new-state (evolve-state-flatmap-reduce state)]
          (if (empty? (:active-games state))
            (assoc state :iterations i)
            (recur new-state (inc i))))))))

(defn score-state-flatmap-reduce [state]
  (->> state
       :finished-games
       (group-by (fn [[[_ _ p1-score p2-score] _]] (if (> p1-score p2-score) 0 1)))
       vals
       (map #(map second %))
       (mapv #(apply + %))))


;; (time (apply max (score-state-flatmap-reduce (play-dirac-dice-flatmap-reduce (parse-input small-input)))))
;; "Elapsed time: 1498.3776 msecs"
;; 444356092776315

;; (time (apply max (score-state-flatmap-reduce (play-dirac-dice-flatmap-reduce (parse-input large-input)))))
;; "Elapsed time: 1561.9225 msecs"
;; 647920021341197
