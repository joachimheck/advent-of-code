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


(defn ->atoms [molecule]
  (let [atom-pattern #"([A-Z][a-z]|[A-Z])"]
    (map second (re-seq atom-pattern molecule))))

(defn includes-atom? [molecule atoms]
  (some (set atoms) (->atoms molecule)))

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

(defn final-atoms [evolvables products]
(prn "final-atoms" (set evolvables) (set (flatten (map ->atoms products))))
  (let [atom-pattern #"([A-Z][a-z]|[A-Z])"]
    (set/difference
     (set (flatten (map ->atoms products)))
     (set evolvables))))

(defn mols-to-remove [products final-atoms]
;;(println "mols-to-remove" products final-atoms)
 (reverse (sort-by count (remove
                          (fn [m] (not (includes-atom? m final-atoms)))
                          products))))

(defn reduce-molecule-once [molecule final-atoms inv-txs]
  (let [remove-mols (mols-to-remove (keys inv-txs) final-atoms)]
    (let [result (reduce (fn [acc k] (str/replace acc k (first (get inv-txs k))))
                         molecule
                         remove-mols)]
      result)))


(defn remove-produced [txs final-atoms]
  (reduce-kv
   (fn [acc k v]
     (let [filtered (filter #(not (includes-atom? % final-atoms)) v)]
       (if (empty? filtered)
         (dissoc acc k)
         (assoc acc k filtered))))
   {}
   txs))

;; (let [txs (get real-input :transforms)
;;       final-atoms #{"Y" "C" "Rn" "Ar"}]
;;   (remove-produced txs final-atoms)
;;   )
;; (count (get real-input :molecule))

;; (let [molecule (get real-input :molecule)
;;       evolvables (keys (get real-input :transforms))
;;       txs (get real-input :transforms)
;;       inv-txs (get real-input :inv-trans)
;;       ]
;;   (loop [molecule molecule
;;          txs txs
;;          inv-txs inv-txs
;;          i 0]
;;     (let [evolvables (keys txs)
;;           final-atoms (final-atoms evolvables (keys inv-txs))
;;           remove-mols (mols-to-remove (keys inv-txs) final-atoms)]
;;       (println "molecule" molecule)
;;       (println "txs" txs)
;; ;;      (println "evolvables" evolvables)
;;       (println "final-atoms" final-atoms)
;;       (println "remove-mols" remove-mols)
;; ;;      (println inv-txs)
;;       (println)
;;       (if (< i 3)
;;         (recur
;;          (reduce-molecule-once molecule final-atoms inv-txs)
;;          (remove-produced txs final-atoms)
;;          (apply dissoc inv-txs remove-mols)
;;          (inc i))
;;         (list (count molecule) molecule)
;;         )
;;       )))

;; (reduce-molecule (get real-input :molecule) (get real-input :inv-trans) 500)
;; => ("Molecule reduced in " 62 "steps" "CRnSiRnFYCaRnFArArFArAl")
;; Repeatedly replacing the longest string with its precursor doesn't get all the way to the beginning.

;; final-atoms is empty, I think because I need to remove the forward mappings that lead
;; to the products I removed as keys from the inverse mapping map.


;; I gave up and looked on the web. Apparently I need to do this in the forward direction, with
;; a guided algorithm, basically A*. I'll see if I can do it myself.
;; (defn score [s goal]
;;   (- (count goal) (count s)))

;; (let [input test-input
;;       txs (get test-input :transforms)
;;       start "e"
;;       goal "HOHOHO"
;;       init-fringe (get txs start)]
;;   (loop [fringe init-fringe
;;          i 0]
;;     (cond (> i 20) fringe
;;           (some #{goal} fringe) (list :done i goal fringe)
;;           :else (let [fringe (remove (fn [s] (>= (count s) (count goal))) fringe)
;;                       scored (map (fn [m] (list m (score m goal))) fringe)
;;                       best (first (first (second (first (sort (group-by second scored))))))
;;                       evolved-best (evolve txs best)]
;;                   ;; (println "fringe" fringe)
;;                   ;; (println "best" best)
;;                   (recur (concat (remove
;;                                   (fn [s] (or (= s best) (>= (count s) (count goal))))
;;                                   fringe)
;;                                  evolved-best)
;;                          (inc i))
;;                   ))
;;     ))



;; Reading further I find that I missed the structure of the produced molecules. They contain
;; pairs of Rn/Ar atoms, with the stuff between those divided by Y atoms.

(defn parenthesize [molecule]
  (if (not (and (str/includes? molecule "Rn") (str/includes? molecule "Ar")))
    molecule
    (read-string
     (str/join
      (list "("
            (-> molecule
                (str/replace "Rn" "(Rn")
                (str/replace "Ar" "Ar)")
                (str/replace "Y" " Y "))
            ")")))))
;; #"([A-Z][a-z]?)(Rn)" (fn [[_ g1 g2]] (str/join (list "(" g1 g2)))

(defn desymbolize [coll]
  (if (not (seq? coll))
    (name coll)
    (map desymbolize coll)))

(defn reduceize-molecule-once [molecule inv-txs]
  (reduce (fn [acc k]
;;            (when (str/includes? acc k) (println "Replacing" k "with" (first (get inv-txs k))))
            (str/replace acc k (first (get inv-txs k))))
          molecule
          (reverse (sort-by count (keys inv-txs)))))

(def atom-pattern #"([A-Z][a-z]|[A-Z])")

(defn atom? [x] (and (string? x) (re-matches atom-pattern x)))

(defn reduceize [molecule]
  (if (seq? molecule)
    (reduceize (str/join molecule))
    (if (atom? molecule)
      molecule
      (let [inv-txs (get real-input :inv-trans)
            result (reduceize-molecule-once molecule inv-txs)]
        (if (= result molecule)
          (->atoms molecule)
          (reduceize result))))))

(defn all-atoms? [coll] (every? #(not (seq? %)) coll))

(defn any-atoms? [coll] (some #(not (seq? %)) coll))

(defn atomize-all [input]
  (letfn [(atomize-inner [input]
            (if (seq? input)
              (list (mapcat atomize-inner input))
              (->atoms input)))]
    (first (atomize-inner input))))

;; (defn reduce-all [coll]
;; (println "reduce-all top" coll)
;;   (if (atom? coll)
;;     coll
;;     (if (all-atoms? coll)
;;       (let [in (str/join coll)
;;             out (reduceize coll)]
;;         (println "reduce-all" in "=>" (str/join out))
;;         out)
;;       (if (any-atoms? coll)
;;         (map reduce-all (partition-by seq? coll))
;;         (map reduce-all coll)))))
(defn reduce-bottom [coll]
  (if (atom? coll)
    coll
    (if (all-atoms? coll)
      (let [in (str/join coll)
            out (reduceize coll)]
        (println "reduce-bottom" in "=>" (str/join out))
        out)
      (map reduce-bottom coll))))

(defn combinize [input]
  (get
   (let [partitioned (partition 2 1 (concat input '(())))]
     (reduce
      (fn [acc [a b]]
        (if (get acc :skip)
          (assoc acc :skip false)
          (if (seq? b)
            (let [combinized-b (combinize b)]
              (if (and (atom? a) (= "Rn" (first combinized-b)))
                (-> acc
                    (assoc :skip true)
                    (update :output #(conj % (concat (list a) combinized-b))))
                (-> acc
                    (assoc :skip false)
                    (update :output #(conj % a)))))
            (-> acc
                (assoc :skip false)
                (update :output #(conj % a)))
            )))
      {:output [] :skip false}
      partitioned))
   :output))

(combinize '("C" ("Rn" "Mg" ("Rn" "Si"))))
(reduce-all (combinize '("C" "Ca" "Si")))
(reduce-all '("C" "Ca" "Si"))
(reduce-bottom '("Rn" "B" "Si" ("Rn" "F" "Ar") "Ti" "B" "P" "Ti" "Ti" "B" "F" "Ar"))
(reduce-bottom (combinize '("Rn" "B" "Si" ("Rn" "F" "Ar") "Ti" "B" "P" "Ti" "Ti" "B" "F" "Ar")))

(reduce-all '("Rn" "F" "Ar"))
(reduce-all (reduce-all (combinize '("Rn" "B" "Ca" "Si" ("Rn" "F" "Ar") "Ti" "B" "P" "Ti" "Ti" "B" "F" "Ar"))))

(reduce-all (reduce-all (combinize (reduce-all (atomize-all (desymbolize (parenthesize "CRnFYSiAlYFArSiRnFYFAr")))))))

(let [input (atomize-all (desymbolize (parenthesize (get real-input :molecule))))]
  (->> input
       combinize
       reduce-bottom
       reduce-bottom
       ))


;; CRnSiRnFYFArFYSiThRnFArYPMgAr = CRn((SiRnFYFAr)F)Y(Si(ThRnFAr))Y(PMg)Ar
;; CRnCaFYSiAlYPMgAr = CRn(CaF)Y(SiAl)Y(PMg)Ar
;; CRnFYFYFAr = CRnFYFYFAr
;; H

;; Trace reduceize!
;; At some point we process CRnSiRnFYCaRnFArArFArThSiRnMgArSiRnMgArPRnFArSiRnFYFArCaRnFAr
;; But that should still be broken up into Rn/Ar subsections.

(def test-input-3
  (parse-input
   '("e => HF"
     "H => CRnFYFYFAr"
     "F => SiAl")
   "CRnFYSiAlYFAr"))



(reduceize-molecule-once
 (reduceize-molecule-once
  (reduce-new (desymbolize (parenthesize (get real-input :molecule))))
  (get real-input :inv-trans))
 (get real-input :inv-trans))

(let [inv-txs (get real-input :inv-trans)
      start (reduce-new (desymbolize (parenthesize (get real-input :molecule))))]
  (take 6 (iterate (fn [input] (reduceize-molecule-once input inv-txs)) start)))


;; "CRnNthArRnHCaRnOBArAr"
;; "C (RnNthAr) (RnHCa (RnOBAr) Ar)"

;; Look at each atom
;; If it is Rn, start parsing with the rest of the string
;; If it is Y, recurse with the

;; "CCaYMgAlYSiTh" => (("C" "Ca") ("Y") ("Mg" "Al") ("Y") ("Si" "Th"))
;; "CCaRnMgAlArSiTh" => (("C" "Ca") (("Rn") ("Mg "Al") ("Ar")) ("Si" "Th"))



(defn fx [pre-atoms post-atoms]
  (if (empty? post-atoms)
    pre-atoms
    (let [atom (first post-atoms)
          more (rest post-atoms)]
      (case atom
        "Y" (fx (concat pre-atoms '(("Y")) '(())) more)
        "Rn" 

        (concat pre-atoms (fx '("Rn") more) )
(fx  (list "Rn" (fx)) )


        (fx (concat (butlast pre-atoms) (list (concat (last pre-atoms) (list atom))))
            more)
        ))))

;; (fx '("") (->atoms "CCaYMgAlYSiTh"))

(concat '("abc" "def") '(()))

(defn break-up [atoms]
  (if (some #{"Rn"} atoms)
    (let [beginning (take-while #(not= "Rn" %) atoms)
          end (drop (count beginning) atoms)]
      (list beginning end))
    'blah
    ))

(break-up (->atoms "CRnNthArRnHCaRnOBArAr"))

;; (let [molecule "CRnNthArRnHCaRnOBArAr"
;;       atoms (->atoms molecule)]
;;   (if (= (first atoms) "Rn")
    

;;     )
;; )

(defn multi-reduceize
  ([input]
   (let [inv-txs (get input :inv-trans)
         molecule (get input :molecule)]
     (flatten (list molecule (multi-reduceize molecule inv-txs)))))
  ([molecule inv-txs]
   (let [result (reduceize-molecule-once molecule inv-txs)]
     (if (= result molecule)
       '()
       (list result (multi-reduceize result inv-txs))))))

(multi-reduceize real-input)

 "CRn SiRnFYCaRnFArArCaFArThRnF Ar"
"CRnSiRnFY CaRnFAr ArFArAl"


;; (let [input test-input-3
;;       inv-txs (get input :inv-trans)
;;       molecule (get input :molecule)
;;       parsed-input (desymbolize (parenthesize molecule))]
;;   (loop [input parsed-input n 2]
;;     (if (= n 0)
;;       input
;;       (recur (reduceize inv-txs input) (dec n)))))

  ;; (list parsed-input (reduceize inv-txs parsed-input))
;;   (let [reduceized (reduceize inv-txs parsed-input)]
;;     (list reduceized (reduceize inv-txs reduceized))
;; )

;; (defn reduce-new [coll]
;; (println coll)
;;   (let [inv-txs (get real-input :inv-trans)]
;;     (if (string? coll)
;;       (reduceize coll)
;;       (if (all-atoms? coll)
;;         (reduceize (str/join coll))
;;         (reduce-new (str/join (map reduce-new coll)))))))

;; (defn reduce-new [molecule]
;;   (println "molecule" molecule)
;;   (let [coll (if (string? molecule)
;;                (desymbolize (parenthesize molecule))
;;                molecule)
;;         inv-txs (get real-input :inv-trans)]
;;     (println "coll" coll)
;;     (if (string? coll)
;;       (reduceize coll)
;;       (if (all-atoms? coll)
;;         (reduceize (str/join coll))
;;         (reduce-new (str/join (map reduce-new coll)))))))


;; I gave up. Even having read through other people's solutions
;; I was unable to make this work. The top solution in the solutions
;; megathread gave a mathematical formula; I'll use that.
(let [atoms (->atoms (get real-input :molecule))
      elements (count atoms)
      rn-or-ar (count (filter #{"Rn" "Ar"} atoms))
      y (count (filter #{"Y"} atoms))]
  (- elements rn-or-ar (* 2 y) 1))
