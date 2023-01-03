(ns day-20.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Infinite houses, infinite elves, infinite presents...

;; 120 presents for house 6, 6 has factors 1, 2, 3 and 6: 10 * (1 + 2 + 3 + 6) = 120
;;29000000 / 10 = 2900000 what are the factors?

(defn factors [n]
  (for [i (range 1 (inc n))
        :when (= 0 (mod n i))]
    i))

(defn sqrt-factors [n]
  (flatten
   (let  [limit (int (Math/pow n (/ 1 2)))]
    (for [i (range 1 (inc limit))
          :when (= 0 (mod n i))]
      (let [f (/ n i)]
        (if (= f i)
          (list i)
          (list i f)))))))

(defn find-first-house [goal]
  (first (drop-while
          (fn [[house presents]] (< presents goal))
          (map (fn [i] (list i (* 10 (reduce + (sqrt-factors i)))))
               (iterate inc 1)))))

;; (time (find-first-house 290000))
;; "Elapsed time: 710.1307 msecs"

;; Only factorize up to n/2
;; (time (find-first-house 29000000))
;; => (665280 29260800)
;; "Elapsed time: 28835.5393 msecs"




;; Part 2
;; Each elf visits only 50 houses, but leaves 11 presents.
;; The lowest-numbered elf to visit a house is (/ house 50)
;; The highest-numbered elf to visit a house is house
;; elf 1 visits 1-50
;; elf 2 visits 2-100
(defn lowest-elf [house]
  (inc (quot (dec house) 50)))

(defn highest-elf [house] house)

(defn presents-2 [house]
  (list house
   (reduce +
           (map #(* 11 %)
                (for [i (range (lowest-elf house) (inc (highest-elf house)))
                      :when (= 0 (mod house i))]
                  i)))))

(defn find-first-house-2-brute-force [goal]
  (first (drop-while
          #(< (second %) goal)
          (map
           #(presents-2 %)
           (iterate inc 1)))))

;; (time (find-first-house-2-brute-force 290000))
;; => "Elapsed time: 652.3668 msecs"

;; Just look at the 50 largest factors.
(defn largest-factors [i]
  (remove #(< % (lowest-elf i)) (sqrt-factors i)))

(defn find-first-house-2 [goal]
  (first (drop-while
          (fn [[house presents]] (< presents goal))
          (map (fn [i] (list i (* 11 (reduce + (largest-factors i)))))
               (iterate inc 1)))))

;; (time (find-first-house-2 29000000))
;; => (720720 30239370)  too high!

;; (time (find-first-house-2 29000000))
;; => (655200 29550620)  too low!
;; "Elapsed time: 22713.1233 msecs"
;; also, (presents-2 655200) => (655200 27800157)

;; largest-factors should drop values < lowest-elf, not take the highest 50
;; (time (find-first-house-2 29000000))
;; => (705600 29002446)
"Elapsed time: 24895.7382 msecs"


