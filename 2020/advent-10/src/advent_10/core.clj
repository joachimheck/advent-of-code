(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "/home/runner/advent-10/small-input.txt")
(def large-input "/home/runner/advent-10/large-input.txt")

(defn- read-numbers
  "Returns a vector containing the numbers."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce conj [] (map #(Long/parseLong %) (line-seq rdr)))))

;; Part 1
;; Compute the joltage differences when you chain all your joltage adapters.
(def small-example '(16 10 15 5 1 11 7 19 6 12 4))

(defn add-ends [coll] (conj (vec coll) 0 (+ 3 (apply max coll))))

(defn joltage-jumps [coll]
  (map #(- (second %) (first %)) (partition 2 1 (sort coll))))

(defn count-joltage-jumps [adapters]
  (let [m (group-by identity (joltage-jumps (sort adapters)))
        ones (count (get m 1))
        twos (count (get m 2))
        threes (count (get m 3))]
    (println "ones" ones "twos" twos "threes" threes)
    (* ones threes)))

;   (count-joltage-jumps (add-ends (read-numbers large-input)))
; ones 68 threes 30
; => 2040



;; Part 2
;; Compute all possible adapter combinations
(defn can-adapt [n coll] (<= (- (first coll) n) 3))

(defn possible-continuations [prefix coll]
;(println prefix coll)
  (if (= 1 (count coll)) (list (flatten (list prefix coll)))
    (let [start (first coll)
          a (when (> (count coll) 1) (rest coll))
          b (when (> (count coll) 2) (rest (rest coll)))
          c (when (> (count coll) 3) (rest (rest (rest coll))))
          continuations (filter seq
                                (filter #(can-adapt start %)
                                        (filter seq (list a b c))))
          ]
;      (println start "=>" prefix "=>" continuations)
      (mapcat #(possible-continuations (list prefix start) %) continuations)
      )))

; '(0 1 4) => '(1 0 1) => 1
; '(0 1 2 5) => '(2 0 1) => 3
; '(0 1 2 3 6) => '(3 0 1) => 5
; '(0 1 2 3 4 7) => '(4 0 1) => 7
; '(0 3 6) => '(0 0 2) => 1
; '(0 3 6 9) => '(0 0 3) => 1 ; no 1-jumps? 1.
; '(0 1 4 7) => '(1 0 2) => 1
; '(0 1 3 6) => '(1 1 1) => 2

; '(0 1) => 1 ; number of jumps = 1
; '(0 2) => 1
; '(0 3) => 1
; '(0 1 2) => 2 ; 2-adapter can skip the middle
; '(0 1 2 3) => 4 ; 1 + 2 2-jumps, 1 3-jump
; '(0 1 2 3 4) => 7 ; 1 + 3 2-jumps, 2 3-jumps, 1 2+2-jump
; '(0 1 2 3 4 5) => 13 ; 1 + 4 2-jumps, 3 3-jumps, 2 2+3-jumps

; 1-jumps give one path, the length of the 1-separated sequence
; 2-jumps give 

(defn paths-from-jumps [coll]
  (println coll)
  (cond (or (nil? coll) (empty? coll)) 0
        (= 1 (count coll)) 1
        (= 2 (count coll))
          (if (or (= '(2 2) coll) (seq (filter #{3} coll)))
              1 2)
        (<= 3 (count coll))
          (let [value (last coll)
                prev-1 (last (butlast coll))
                prev-2 (last (butlast (butlast coll)))
                prev-3 (last (butlast (butlast (butlast coll))))]
            (println "prevs" prev-1 prev-2 prev-3)
;            (cond
;             (= 3 value) (paths-from-jumps (butlast coll))
;             (= 2 value)
;               (if (> prev-1 1)
;                 (paths-from-jumps (butlast coll))
;                 (+ (paths-from-jumps (butlast (butlast coll)))
;                    (paths-from-jumps (butlast coll))))
;             (= 1 value)
               (+ (paths-from-jumps (butlast coll))
                  (if (< value 3) (paths-from-jumps (butlast (butlast coll))) 0)
                  (if (< (+ value prev-1 prev-2) 3) (paths-from-jumps (butlast (butlast (butlast coll)))) 0)
               )

;               (cond (= 3 prev-1) (paths-from-jumps (butlast coll))
;                     (= 3 prev-2) (+ (paths-from-jumps (butlast coll))
;                                     (paths-from-jumps (butlast (butlast coll))))
;                     (= 2 prev-1) (+ (paths-from-jumps (butlast coll))
;                                     (paths-from-jumps (butlast (butlast coll))))
;                     :else (+ (paths-from-jumps (butlast coll))
;                              (paths-from-jumps (butlast (butlast coll)))
;                              (paths-from-jumps (butlast (butlast (butlast coll)))))
               )))

(defn possible-paths [coll]
  (let [jumps (joltage-jumps coll)
        paths (count (possible-continuations '() coll))]
;    (println "jumps" jumps)
;    (println "paths" paths)
    paths))

; looks like possible-paths <coll 1-jumps of size n> =
;  the sum of the paths for the colls of size (n-1), (n-2), (n-3)
; (time (possible-paths '(0 1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 19 22 25 28)))

(defn compare-paths [coll]
(println coll (joltage-jumps coll))
  (let [from-jumps (paths-from-jumps (joltage-jumps coll))
        possible (possible-paths coll)]
    (println "from jumps" from-jumps "possible" possible)
    (= from-jumps possible)))

(defn make-permutations []
  (for [a [1 2 3]
        b [1 2 3]
        c [1 2 3]]
  (println ";" [a b c])))

; previous number of paths is x
; [1 1 1] => x + 3
; [1 1 2] => x + 2
; [1 1 3] => x + 1
; [1 2 1] => x + 2
; [1 2 2] => x + 1
; [1 2 3] => x + 1
; [1 3 1] => x
; [1 3 2] => x
; [1 3 3] => x
; [2 1 1] => x + 2
; [2 1 2] => x + 2
; [2 1 3] => x + 1
; [2 2 1] => x + 1
; [2 2 2] => x
; [2 2 3] => x
; [2 3 1] => x
; [2 3 2] => x
; [2 3 3] => x
; [3 1 1] => x + 1
; [3 1 2] => x + 1
; [3 1 3] => x
; [3 2 1] => x + 1
; [3 2 2] => x
; [3 2 3] => x
; [3 3 1] => x
; [3 3 2] => x
; [3 3 3] => x



;; in jumps list
;; 2 1's in a row = 2x arrangements
;; 3 1's in a row = 4x arrangements

(defn set-top [coll x]
  (conj (pop coll) x))

; groups = [[]] 
(defn break-into-groups [groups l]
  (if (empty? l) groups
    (if (or (empty? (last groups)) (= (last (last groups)) (first l)))
        (break-into-groups (set-top groups (conj (last groups) (first l))) (rest l))
        (break-into-groups (conj groups (vector (first l))) (rest l))
        )))

(defn count-significant-groups [l]
  (let [groups (break-into-groups [[]] l)]
    (->> groups
         (filter #(= (first %) 1))
         (filter #(> (count %) 1))
         (map count)
  )))

(defn compute-combinations [l]
  (->> (count-significant-groups l)
       (map #(cond (= 2 %) 2
                   (= 3 %) 4
                   (= 4 %) 7))
       (reduce *)))

;    (compute-combinations (joltage-jumps (add-ends (read-numbers small-input))))
; => 19208
;    (compute-combinations (joltage-jumps (add-ends (read-numbers large-input))))
; => 28346956187648
   
