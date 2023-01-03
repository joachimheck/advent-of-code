(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Compute the score of the first winning board.
(defn line-to-vector [line]
  (read-string (str/join ["[" line "]"])))

(defn parse-board [lines]
  {:board (mapv line-to-vector lines)
   :marks []})

(defn parse-input [f]
  (let [lines (remove #{'("")} (partition-by #(= "" %) (read-lines f)))
        numbers (line-to-vector (first (first lines)))
        boards (map parse-board (rest lines))]
    {:numbers numbers :boards boards}))

(defn get-number
  ([board [x y]] (get-number board x y))
  ([board x y] (((board :board) x) y)))

(defn find-position [board num]
  (first (remove #(= nil %)
           (for [i (range 0 5)
                 j (range 0 5)]
             (when (= (get-number board i j) num)
               [i j])))))

(defn apply-number [board num]
  (let [pos (find-position board num)]
    (if pos
      (update board :marks conj pos)
      board)))

(defn contains-all? [coll items]
  (reduce (fn [result item] (boolean (and result (some #{item} coll)))) true items))

(def winning-mark-sets
  [[[0 0] [0 1] [0 2] [0 3] [0 4]]
   [[1 0] [1 1] [1 2] [1 3] [1 4]]
   [[2 0] [2 1] [2 2] [2 3] [2 4]]
   [[3 0] [3 1] [3 2] [3 3] [3 4]]
   [[4 0] [4 1] [4 2] [4 3] [4 4]]
   [[0 0] [1 0] [2 0] [3 0] [4 0]]
   [[0 1] [1 1] [2 1] [3 1] [4 1]]
   [[0 2] [1 2] [2 2] [3 2] [4 2]]
   [[0 3] [1 3] [2 3] [3 3] [4 3]]
   [[0 4] [1 4] [2 4] [3 4] [4 4]]
   ])

(defn bingo? [board]
  (reduce #(or %1 %2) (map #(contains-all? (board :marks) %) winning-mark-sets)))

(defn play-bingo [f]
  (let [parsed (parse-input f)]
    (loop [numbers (parsed :numbers)
           boards (parsed :boards)]
      (if (empty? numbers)
        :no-winners
        (let [updated-boards (map #(apply-number % (first numbers)) boards)
              bingos (map #(list (bingo? %) %) updated-boards)
              ]
          (if (> (count (filter #(= true (first %)) bingos)) 0)
            bingos
            (recur (rest numbers) updated-boards)))))))

(defn get-unmarked-numbers [board]
  (map #(get-number board %)
       (set/difference (set (for [i (range 0 5) j (range 0 5)] [i j]))
                       (set (board :marks)))))

(defn get-bingo-score [f]
  (let [end-state (play-bingo f)
        winner (second (first (filter #(= true (first %)) end-state)))
        last-number (get-number winner (last (winner :marks)))
        unmarked (get-unmarked-numbers winner)]
    (* last-number (apply + unmarked))))


;; (get-bingo-score small-input)
;; 4512
;; (get-bingo-score large-input)
;; 28082



;; Part 2
;; Compute the score of the last winning board.
(defn play-all-boards [f]
  (let [parsed (parse-input f)]
    (loop [numbers (parsed :numbers)
           boards (parsed :boards)]
      (if (empty? numbers)
        :no-winners
        (let [updated-boards (map #(apply-number % (first numbers)) boards)]
          (if (and (= (count updated-boards) 1)
                   (bingo? (first updated-boards)))
            (first updated-boards)
            (recur (rest numbers) (remove #(bingo? %) updated-boards))))))))

(defn get-bingo-score-2 [f]
  (let [winner (play-all-boards f)
        last-number (get-number winner (last (winner :marks)))
        unmarked (get-unmarked-numbers winner)]
    (* last-number (apply + unmarked))))

;; (get-bingo-score-2 small-input)
;; 1924
;; (get-bingo-score-2 large-input)
;; 8224
