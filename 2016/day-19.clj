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

;; The array method from part 1. This is too slow.
(defn find-elf-across [elves elf]
  (let [elf-index (.indexOf elves elf)
        across-index (mod (+ elf-index (quot (count elves) 2)) (count elves))]
    (nth elves across-index)))

(defn last-elf-2 [elves elf]
  (loop [elves elves elf elf]
    (if (= (count elves) 1)
      (first elves)
      (let [across-elf (find-elf-across elves elf)
            elves-reduced (remove #{across-elf} elves)
            next-elf (nth elves-reduced (mod (inc (.indexOf elves-reduced elf)) (count elves-reduced)))]
        (recur elves-reduced next-elf)))))

;; (time (last-elf-2 (take 10000 (iterate inc 1)) 1))
;; 100 : "Elapsed time: 8.6645 msecs"
;; 1000: "Elapsed time: 308.6056 msecs"
;; 100 : "Elapsed time: 6.674499 msecs"
;; 1000: "Elapsed time: 235.7957 msecs"


;; Try using a linked list.
(defn build-circle [elf-count]
  (apply merge
         (for [i (range 1 (inc elf-count))]
           (assoc {} i (inc (mod i elf-count))))))

(defn find-elves-across-circle [circle elf]
  (let [circle-size (count circle)
        half-circle-size (quot (count circle) 2)
        prev-elf (nth (iterate (partial get circle) elf) (dec half-circle-size))
        elf (get circle prev-elf)]
    (list prev-elf elf)))

(defn remove-elf [circle prev-elf elf]
  (-> circle
      (assoc prev-elf (get circle elf))
      (dissoc elf)))

(defn last-elf-circle [circle elf]
  (loop [circle circle elf elf]
    (if (= (count circle) 1)
      (first (keys circle))
      (let [[prev-elf across-elf] (find-elves-across-circle circle elf)
            elves-reduced (remove-elf circle prev-elf across-elf)
            next-elf (get elves-reduced elf)]
        (recur elves-reduced next-elf)))))

;; 100:  "Elapsed time: 1.877899 msecs"
;; 1000: "Elapsed time: 62.9292 msecs"
;; 10000: "Elapsed time: 3402.3718 msecs"
;; This is still too slow, because walking the list takes too long.


;; Genius idea: actually, we're just deleting an elf, then skipping an elf
;; and deleting the next one, then deleting the next elf, then skipping, etc.

;; 1 2 3 4 5
;; remove 3 5 1 4
;; 2/3/4 remove 3 next is 2 (1 2 4 5)
;; 2/4/5 skip remove 5 next is 4 (1 2 4)
;; 4/1/2 remove 1 next is 4 (2 4)
;; 4/2/4 skip remove 4 next is 2 (2)

(defn last-elf-skip [circle elf]
  (let [[prev-elf _] (find-elves-across-circle circle elf)]
    ;; Remove the elf after prev-elf, or the one after that, if skipping.
    ;; The next prev-elf is the one after the removed elf.
   (loop [circle circle prev-elf prev-elf skip false]
     (if (= (count circle) 1)
       (first (keys circle))
       (let [a prev-elf
             b (get circle a)
             c (get circle b)
             smaller-circle (if skip (remove-elf circle b c) (remove-elf circle a b))
             next-elf (if skip b a)]
         (recur smaller-circle next-elf (not skip)))))))

;; (time (last-elf-skip (build-circle 100000) 1))
;; 100:    "Elapsed time: 0.4463 msecs"
;; 1000:   "Elapsed time: 3.3609 msecs"
;; 10000:  "Elapsed time: 34.789399 msecs"
;; 100000: "Elapsed time: 230.733 msecs"

;; (time (last-elf-skip (build-circle 3017957) 1))
;; => 1423634
;; "Elapsed time: 9701.178701 msecs"
