(ns day-21.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Scramble up a password
(def test-input
  '("swap position 4 with position 0"
    "swap letter d with letter b"
    "reverse positions 0 through 4"
    "rotate left 1 step"
    "move position 1 to position 4"
    "move position 3 to position 0"
    "rotate based on position of letter b"
    "rotate based on position of letter d"))

(defn parse-line [line]
  (or
   (let [match (re-matches #"swap position (\d+) with position (\d+)" line)]
     (when match [:swap-position (Long/parseLong (nth match 1)) (Long/parseLong (nth match 2))]))
   (let [match (re-matches #"swap letter (\w) with letter (\w)" line)]
     (when match [:swap-letter (nth match 1) (nth match 2)]))
   (let [match (re-matches #"rotate (left|right) (\d+) steps?" line)]
     (when match [:rotate (* (Long/parseLong (nth match 2)) (if (= "left" (nth match 1)) 1 -1))]))
   (let [match (re-matches #"rotate based on position of letter (\w)" line)]
     (when match [:rotate-letter (nth match 1)]))
   (let [match (re-matches #"reverse positions (\d+) through (\d+)" line)]
     (when match [:reverse-range (Long/parseLong (nth match 1)) (Long/parseLong (nth match 2))]))
   (let [match (re-matches #"move position (\d+) to position (\d+)" line)]
     (when match [:move (Long/parseLong (nth match 1)) (Long/parseLong (nth match 2))]))))

(defn swap-position [s in-p1 in-p2]
  (if (= in-p1 in-p2)
    s
    (let [p1 (min in-p1 in-p2) p2 (max in-p1 in-p2)]
      (str/join (list (subs s 0 p1)
                      (subs s p2 (inc p2))
                      (subs s (inc p1) p2)
                      (subs s p1 (inc p1))
                      (subs s (inc p2)))))))

(defn swap-letter [s l1 l2]
  (swap-position s (str/index-of s l1) (str/index-of s l2)))

(defn rotate [s amt]
  (str/join (take (count s) (drop (+ (* 2 (count s)) amt) (cycle s)))))

(defn rotate-letter [s l]
  (let [idx (str/index-of s l)
        amt (- (+ 1 idx (if (>= idx 4) 1 0)))]
    (rotate s amt)))

(defn reverse-range [s in-p1 in-p2]
  (let [p1 (min in-p1 in-p2) p2 (max in-p1 in-p2)
        sub-s (subs s p1 (inc p2))
        reversed (str/reverse sub-s)]
    (str/join (list
               (subs s 0 p1)
               (str/reverse sub-s)
               (subs s (inc p2))))))

(defn move [s p1 p2]
  (let [c (subs s p1 (inc p1))
        removed (str/join (list (subs s 0 p1) (subs s (inc p1))))]
    (str/join (list
               (subs removed 0 p2)
               c
               (subs removed p2)))))

(defn operate [s [op arg1 arg2]]
  (doto
      (case op
        :swap-position (swap-position s arg1 arg2)
        :swap-letter (swap-letter s arg1 arg2)
        :rotate (rotate s arg1)
        :rotate-letter (rotate-letter s arg1)
        :reverse-range (reverse-range s arg1 arg2)
        :move (move s arg1 arg2)
        :unrotate-letter (unrotate-letter s arg1))
    println))

(defn process-string [s ops]
  (reduce operate s ops))

;; (process-string "abcde" (map parse-line test-input))
;; => "decab"

;; (process-string "abcdefgh" (map parse-line (str/split-lines (slurp "input-21.txt"))))
;; => "ghfacdbe"



;; Part 2
;; Unscramble a password

(defn parse-line-unscramble [line]
  (or
   (let [match (re-matches #"swap position (\d+) with position (\d+)" line)]
     (when match [:swap-position (Long/parseLong (nth match 2)) (Long/parseLong (nth match 1))]))
   (let [match (re-matches #"swap letter (\w) with letter (\w)" line)]
     (when match [:swap-letter (nth match 1) (nth match 2)]))
   (let [match (re-matches #"rotate (left|right) (\d+) steps?" line)]
     (when match [:rotate (* (Long/parseLong (nth match 2)) (if (= "right" (nth match 1)) 1 -1))]))
   (let [match (re-matches #"rotate based on position of letter (\w)" line)]
     (when match [:unrotate-letter (nth match 1)]))
   (let [match (re-matches #"reverse positions (\d+) through (\d+)" line)]
     (when match [:reverse-range (Long/parseLong (nth match 1)) (Long/parseLong (nth match 2))]))
   (let [match (re-matches #"move position (\d+) to position (\d+)" line)]
     (when match [:move (Long/parseLong (nth match 2)) (Long/parseLong (nth match 1))]))))

(defn unrotate-letter [s l]
  (let [idx (str/index-of s l)
        amt (- (+ 1 idx (if (>= idx 4) 1 0)))]
    (rotate s amt)))

;; (process-string "decab" (reverse (map parse-line-unscramble test-input)))
;; does not work due to multiple possible rotations.

;; (process-string "fbgdceah" (reverse (map parse-line-unscramble (str/split-lines (slurp "input-21.txt")))))
;; => "fhgcdaeb"

(defn unrotate-letter [s l]
  (let [result (keep
          #(when (= (second %) s) (first %))
          (for [i (range (count s))]
            (list (rotate s i)
                  (rotate-letter (rotate s i) l))))]
    (if (= 1 (count result))
      (first result)
      result)))
