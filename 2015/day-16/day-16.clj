(ns day-16.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Which Aunt Sue got me a gift?

(defn parse-line [line]
  (let [pattern #"(Sue (\d+))|(([a-z]+): ([\d]+))+"
        matches (re-seq pattern line)]
    (assoc {}
           (Long/parseLong (nth (first matches) 2))
           (reduce (partial apply assoc) {} (map #(list (nth % 4) (Long/parseLong (nth % 5))) (rest matches))))))

(defn read-input [f]
  (reduce merge
          {}
          (map parse-line (str/split-lines (slurp f)))))

(def target (assoc {} "children" 3 "cats" 7 "samoyeds" 2 "pomeranians" 3 "akitas" 0 "vizslas" 0 "goldfish" 5 "trees" 3 "cars" 2 "perfumes" 1))

(defn aunt-match? [aunt tgt]
  (reduce
   (fn [acc k] (and acc (= (get tgt k) (get aunt k))))
   true
   (keys aunt)))

(defn get-aunts [f match-f]
  (first
   (first
    (let [aunt-map (read-input f)]
      (filter #(second %)
              (for [i (range 1 501)]
                (list i (match-f (get aunt-map i) target))))))))

;; (get-aunts "input-16.txt" aunt-match?)
;; => 40



;; Part 2
(defn aunt-match-2? [aunt tgt]
  (reduce
   (fn [acc k]
     (and acc
          (case k
            ("cats" "trees") (> (get aunt k) (get tgt k))
            ("pomeranians" "goldfish") (< (get aunt k) (get tgt k))
            (= (get tgt k) (get aunt k)))))
   true
   (keys aunt)))

;; (get-aunts "input-16.txt" aunt-match-2?)
;; => 241
