(ns day-19.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Count distinct variants of single replacements to a molecule.

(def test-input {:molecule "HOH" :transforms {"H" '("HO" "OH") "O" '("HH")}})
(def test-input-2 {:molecule "SieMgH" :transforms {"H" '("HO" "OH") "Si" '("Ti") "e" '("P")}})

(def real-input
 (let [[part1 molecule] (str/split (slurp "input-19.txt") #"\R\R")
       lines (str/split-lines part1)]
   (assoc {}
          :molecule molecule
          :transforms
          (reduce
           (fn [acc line]
             (let [[_ from to] (re-matches #"(\w+) => (\w+)" line)]
               (merge-with concat acc (assoc {} from (list to)))
               ))
           {}
           lines))))

(defn match [alphabet chars]
  (cond (alphabet (str/join chars)) (str/join chars)
        (alphabet (str (first chars))) (str (first chars))
        :else (first chars)))

(split-molecule test-input-2)

(defn split-molecule [{molecule :molecule transforms :transforms}]
  (let [alphabet (set (keys transforms))]
    (vec
     (for [cc (partition 2 1 (str/join (list molecule "0")))
           :let [match (match alphabet cc)]
           :when match]
       match))))

(defn replace [{transforms :transforms :as input}]
  (distinct
   (flatten
    (let [molecule (split-molecule input)]
      (for [i (range (count molecule))]
        (let [pre (subvec molecule 0 i)
              atom (get molecule i)
              post (subvec molecule (inc i))]
          (for [t (get transforms atom)]
            (str/join (concat pre t post))))))
    )))

;; (replace test-input)
;; => ("HOOH" "OHOH" "HHHH" "HOHO")
;; (count (replace test-input))
;; => 4

;; (count (replace real-input))
;; => 189 too low! - I'm assuming one character per atom.

;; (count (replace real-input))
;; => 507 too low, again! I'm dropping atoms that aren't transformable, that's probably it.

;; (count (replace real-input))
;; => 585 too high? Maybe I'm not correctly including the extra characters.
