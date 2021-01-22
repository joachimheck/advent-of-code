(ns day-19.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Count distinct variants of single replacements to a molecule.

(defn parse-input [lines molecule]
  (let [[trxs inv-trxs]
        (reduce
         (fn [[trxs inv-trxs] line]
           (let [[_ from to] (re-matches #"(\w+) => (\w+)" line)]
             (list
              (merge-with concat trxs (assoc {} from (list to)))
              (merge-with concat inv-trxs (assoc {} to (list from))))
             ))
         '({} {})
         lines)]
    (assoc {}
           :molecule molecule
           :transforms trxs
           :inv-trans inv-trxs)))

(def test-input
  (parse-input '("e => H" "e => O" "H => HO" "H => OH" "O => HH") "HOH"))

(def test-input-2
  (parse-input '("H => HO" "H => OH" "Si => Ti" "e => P") "SieMgH"))

(def real-input
  (let [[part1 molecule] (str/split (slurp "input-19.txt") #"\R\R")
        lines (str/split-lines part1)]
    (parse-input lines (str/replace molecule #"\R" ""))))


(defn match [alphabet chars]
  (cond (alphabet (str/join chars)) (str/join chars)
        (alphabet (str (first chars))) (str (first chars))
        :else (first chars)))

(defn split-molecule [{molecule :molecule transforms :transforms}]
;;  (println molecule (type molecule))
  (if (<= (count molecule) 1) (list molecule)
      (vec
       (let [alphabet (set (keys transforms))
             sub1 (subs molecule 0 1)
             sub2 (subs molecule 0 2)
             match1 (alphabet sub1)
             match2 (alphabet sub2)]
         (if match2
           (concat (list sub2) (split-molecule {:molecule (subs molecule 2) :transforms transforms}))
           (concat (list sub1) (split-molecule {:molecule (subs molecule 1) :transforms transforms})))))))

(defn replace-atoms [{transforms :transforms :as input}]
  (distinct
   (flatten
    (let [molecule (split-molecule input)]
      (for [i (range (count molecule))]
        (let [pre (subvec molecule 0 i)
              atom (get molecule i)
              post (subvec molecule (inc i))]
          (for [t (get transforms atom)]
            (str/join (concat pre t post)))))))))

;; (replace-atoms test-input)
;; => ("HOOH" "OHOH" "HHHH" "HOHO")
;; (count (replace-atoms test-input))
;; => 4

;; (count (replace-atoms real-input))
;; => 189 too low! - I'm assuming one character per atom.

;; (count (replace-atoms real-input))
;; => 507 too low, again! I'm dropping atoms that aren't transformable, that's probably it.

;; (count (replace-atoms real-input))
;; => 585 too high? Maybe I'm not correctly including the extra characters.

;; (count (replace-atoms real-input))
;; => 509



;; Part 2
(defn reduce-molecule [{molecule :molecule txs :transforms inv-txs :inv-trans :as input} limit]
  (let [inv-keys (reverse (sort-by count (keys inv-txs)))
        ]
    (loop [i 0
           mol molecule]
      (if (< limit i)
        mol
        (recur (inc i)
               (reduce
                (fn [mol k]
;;                  (println i "key" k mol)
                  (str/replace mol k (first (get inv-txs k))))
                molecule
                inv-keys))
        ))))



(reduce-molecule test-input 100)
;; => "CRnSiRnBCaPMgArCaSiRnTiMgArSiRnMgArCaPRnPMgArSiRnCaCaFYSiRnFYFArCaPMgArPRnFArCaSiRnFYCaCaRnFArArSiRnPMgYCaCaFArSiRnPMgArBCaCaCaCaCaCaCaSiRnFArRnFArCaCaSiRnCaFArMgArThSiRnTiRnFArFArCaCaSiRnMgArPRnFArCaRnPMgAr"
;; => "CRnSiRnBCaPMgArCaSiRnTiMgArSiRnMgArCaPRnPMgArSiRnCaCaFYSiRnFYFArCaPMgArPRnFArCaSiRnFYCaCaRnFArArSiRnPMgYCaCaFArSiRnPMgArBCaCaCaCaCaCaCaSiRnFArRnFArCaCaSiRnCaFArMgArThSiRnTiRnFArFArCaCaSiRnMgArPRnFArCaRnPMgAr"
;; => "CRnSiRnBCaPMgArCaSiRnTiMgArSiRnMgArCaPRnPMgArSiRnCaCaFYSiRnFYFArCaPMgArPRnFArCaSiRnFYCaCaRnFArArSiRnPMgYCaCaFArSiRnPMgArBCaCaCaCaCaCaCaSiRnFArRnFArCaCaSiRnCaFArMgArThSiRnTiRnFArFArCaCaSiRnMgArPRnFArCaRnPMgAr"
