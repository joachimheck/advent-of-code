(ns day-15.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(def test-input
  '("Disc #1 has 5 positions; at time=0, it is at position 4."
    "Disc #2 has 2 positions; at time=0, it is at position 1."))

(def real-input (str/split-lines (slurp "input-15.txt")))

(defn parse-input [lines]
  (apply merge
   (map (fn [line]
          (let [[_ disc size init-pos](re-matches #"Disc #(\d+) has (\d+) positions.+at position (\d+)." line)]
            (assoc {} (Long/parseLong disc)
                   (assoc {}
                          :size (Long/parseLong size)
                          :init-pos (Long/parseLong init-pos)))))
        lines)))

(defn positions-at [discs t]
 (let [num-discs (apply max (keys discs))]
   (map (fn [[disc {size :size init-pos :init-pos}]] (mod (+ t disc init-pos) size)) discs)))

(defn valid-state? [state]
  (every? #{0} state))

(defn find-valid-state [discs]
  (loop [i 0]
    (let [state (positions-at discs i)]
      (if (valid-state? state)
        i
        (recur (inc i))))))

;; (find-valid-state (parse-input test-input))
;; => 5

;; (find-valid-state (parse-input real-input))
;; => 16824



;; Part 2
;; Add another disc
(def real-input-2 (concat (str/split-lines (slurp "input-15.txt"))
                          (list "Disc #7 has 11 positions; at time=0, it is at position 0.")))

(find-valid-state (parse-input real-input-2))
;; => 3543984
