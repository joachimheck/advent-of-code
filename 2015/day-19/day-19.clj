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

(defn split-molecule [mol-str transforms]
  (if (<= (count mol-str) 1) (vector mol-str)
      (vec
       (let [alphabet (set (keys transforms))
             sub1 (subs mol-str 0 1)
             sub2 (subs mol-str 0 2)
             match1 (alphabet sub1)
             match2 (alphabet sub2)]
         (if match2
           (concat (list sub2) (split-molecule (subs mol-str 2) transforms))
           (concat (list sub1) (split-molecule (subs mol-str 1) transforms)))))))

(defn evolve [transforms mol-str]
  (distinct
   (flatten
    (let [molecule (split-molecule mol-str transforms)]
      (for [i (range (count molecule))]
        (let [pre (subvec molecule 0 i)
              atom (get molecule i)
              post (subvec molecule (inc i))]
          (for [t (get transforms atom)]
            (str/join (concat pre t post)))))))))

;; (evolve (get test-input :transforms) (get test-input :molecule))
;; => ("HOOH" "OHOH" "HHHH" "HOHO")
;; (count (evolve test-input))
;; => 4

;; (count (evolve (get real-input :transforms) (get real-input :molecule)))
;; => 189 too low! - I'm assuming one character per atom.

;; (count (evolve (get real-input :transforms) (get real-input :molecule)))
;; => 507 too low, again! I'm dropping atoms that aren't transformable, that's probably it.

;; (count (evolve (get real-input :transforms) (get real-input :molecule)))
;; => 585 too high? Maybe I'm not correctly including the extra characters.

;; (count (evolve (get real-input :transforms) (get real-input :molecule)))
;; => 509



;; Part 2

;; I'll try just doing it forward.
(defn matching-prefix [s1 s2]
  (str/join
   (for [i (range (min (count s1) (count s2)))
         :while (= (subs s1 i (inc i)) (subs s2 i (inc i)))]
     (subs s1 i (inc i)))))

(defn directed-evolve [transforms target mol-str]
  (let [match (matching-prefix target mol-str)]
   (distinct
    (flatten
     (let [molecule (split-molecule (subs mol-str (count match)) transforms)]
       (for [i (range (count molecule))]
         (let [pre (subvec molecule 0 i)
               atom (get molecule i)
               post (subvec molecule (inc i))]
           (for [t (get transforms atom)]
             (str/join (concat pre t post))))))))))

(defn evolve-multiple [transforms target molecules]
  (distinct (mapcat (partial directed-evolve transforms target) molecules)))

(defn evolve-medicine [target transforms start]
  (let [has-target? (set (list target))]
    (loop [molecules (flatten (list start))
           i 1]
      (let [evolved (evolve-multiple transforms target molecules)
            trimmed (remove #(> (count %) (count target)) evolved)]
;; (println "target" target "evolved" evolved "trimmed" trimmed)
(println i (count evolved) (count trimmed))
        (if (or (some has-target? trimmed) (empty? trimmed) (> i 10) (= evolved molecules))
          i
          (recur trimmed (inc i))
          )))))

;; (evolve-medicine "HOH" (get test-input :transforms) "e")
;; => 3

;; (evolve-medicine "HOHOHO" (get test-input :transforms) "e")
;; => 6

;; (time (evolve-medicine (get real-input :molecule) (get real-input :transforms) "e"))
;; Brute force - way too slow. I'll try removing the common prefix with the target string.



;; I'm trying it backward again.


(defn reduce-molecule [molecule txs inv-txs limit]
  (let [atom-pattern #"([A-Z][a-z]|[A-Z])"
        final-atoms (set/difference
                     (set (flatten (map (fn [s] (map second (re-seq atom-pattern s))) (keys inv-txs))))
                     (set (keys txs)))
        inv-keys (reverse (sort-by count
                                   (remove (fn [k] (not (includes-atom? k final-atoms))) (keys inv-txs))))
        ]
(println "final-atoms" final-atoms "removed" (remove (fn [k] (not (includes-atom? k final-atoms))) (keys inv-txs)))
    (loop [i 0
           mol molecule]
      (if (< limit i)
        (list (count mol) mol)
        (let [first-match (first (filter #(str/includes? mol %) inv-keys))]
          (println "replacing" first-match "with" (first (get inv-txs first-match)))
          (if (nil? first-match)
            (list "Molecule reduced in " i "steps" mol)
            (recur (inc i)
                   (str/replace mol first-match (first (get inv-txs first-match)))))
          )))))

(defn ->atoms [molecule]
  (let [atom-pattern #"([A-Z][a-z]|[A-Z])"]
    (map second (re-seq atom-pattern molecule))))

(defn final-atoms [mappable-atoms molecules]
(println "final-atoms" mappable-atoms molecules)
  (let [atom-pattern #"([A-Z][a-z]|[A-Z])"]
    (set/difference
     (set (flatten (map ->atoms molecules)))
     (set mappable-atoms))))

(defn includes-atom? [molecule atoms]
  (some (set atoms) (->atoms molecule)))

(defn mols-to-remove [products final-atoms]
;;(println "mols-to-remove" products final-atoms)
 (reverse (sort-by count (remove
                          (fn [m] (not (includes-atom? m final-atoms)))
                          products))))

(defn reduce-molecule-once [molecule evolvables inv-txs]
  (let [final-atoms (final-atoms evolvables (keys inv-txs))
        remove-mols (mols-to-remove (keys inv-txs) final-atoms)]
    (let [result (reduce (fn [acc k] (str/replace acc k (first (get inv-txs k))))
                         molecule
                         remove-mols)]
      result)))

(count (get real-input :molecule))

(let [molecule (get real-input :molecule)
      evolvables (keys (get real-input :transforms))
      inv-txs (get real-input :inv-trans)
      ]
  (loop [molecule molecule
         evolvables evolvables
         inv-txs inv-txs
         i 0]
    (let [final-atoms (final-atoms evolvables (keys inv-txs))
          remove-mols (mols-to-remove (keys inv-txs) final-atoms)]
;;      (println "final-atoms" final-atoms)
      (println "remove-mols" remove-mols)
      (println "final-atoms" final-atoms)
;;      (println inv-txs)
      (if (< i 3)
        (recur
         (reduce-molecule-once molecule evolvables inv-txs)
         evolvables
         (apply dissoc inv-txs remove-mols)
         (inc i))
        (list (count molecule) molecule)
        )
)))


;; (reduce-molecule (get real-input :molecule) (get real-input :inv-trans) 500)
;; => ("Molecule reduced in " 62 "steps" "CRnSiRnFYCaRnFArArFArAl")
;; Repeatedly replacing the longest string with its precursor doesn't get all the way to the beginning.

;; final-atoms is empty, I think because I need to remove the forward mappings that lead
;; to the products I removed as keys from the inverse mapping map.
