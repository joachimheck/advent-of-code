(ns day-19.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Which elf gets all the presents?

;; 1 2 3
;; 3 1 - first is n
;; 3   - first is n (previous list length was even)

;; 1 2 3 4 5
;; 5 1 3 - first is n
;; 3 5   - first is n-2
;; 3     - first is n-2

;; 1 2 3 4 5 6 7
;; 7 1 3 5 - first is n
;; 7 3     - first is n (previous list length was even)
;; 7       - first is n

;; 1 2 3 4 5 6 7 8 9
;; 9 1 3 5 7 - first is n
;; 7 9 3     - first is n - 2
;; 3 7       - first is n - 6 ((n-2) - 4)
;; 3         - first is n - 6 (even)

;; 1 2 3 4 5 6 7 8 9 10 11
;; 11 1 3 5 7 9 - first is n
;; 11 3 7       - first is n (even)
;; 7 11         - first is (n-4)
;; 7            - first is (n-4) (even)

;; 1 2 3 4 5 6 7 8 9 10 11 12 13
;; 13 1 3 5 7 9 11
;; 11 13 3 7
;; 11 3
;; 11

(partition 2 (take 7 (iterate inc 1)))

(defn last-elf [n]
  (loop [elves (take n (iterate inc 1))]
    (let [firsts (map first (partition 2 elves))
          remaining (if (even? (count elves))
                      firsts
                      (concat (list (last elves)) firsts))]
      (if (= 1 (count remaining))
        (first remaining)
        (recur remaining)))))

;; (time (last-elf 3017957))
;;  => 1841611
;; "Elapsed time: 4330.4262 msecs"

(let [elves (take 5 (iterate inc 1))
      elf 3
      across-index (quot (count elves) 2)]
  (list elf elves)
)

;; Part 2
;; Elves steal presents from the elf across the circle.

;; 1 (2 3 4 5) - count 4
;; (4+1)/2 = 2
;; 3

;; 1 (2 3 4 5 6) - count 5
;; (5+1)/2 = 3
;; 4

;; (1 2) 3 (4 5)
;; 3 (4 5 1 2) - count 4
;; (4+1)/2 = 2
;; 5

;; (1 2 3) 4 (5)
;; 5/2 = 2
;; elf-index = 3
;; 3 + 2 = 5
;; 5 mod 5 = 0
;; elf = 1
