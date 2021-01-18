(ns day-11.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Generate new password from old by incrementing to the next valid value.

(def test-input '("hijklmmn" "abbceffg" "abbcegjk" "abbcdegjkdd"))
(def test-input-2 '(("abcdefgh" "abcdffaa") ("ghijklmn" "ghjaabcc")))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def triplets (set (partition 3 1 alphabet)))
(def not-allowed (set '(\i \o \l)))

(defn has-two-pairs? [s]
  (->> s
       (partition 2 1)
       (filter (fn [[a b]] (= a b)))
       (group-by second)
       count
       ((partial < 1))
       ))

(defn valid? [s]
  (and
   (some? (some triplets (partition 3 1 s)))
   (not-any? not-allowed s)
   (has-two-pairs? s)
   ))

(defn invalid? [s]
  (not (valid? s)))

(defn inc-c [c]
  (first (rest (drop-while #(not= c %) alphabet))))

(defn inc-s [s]
  (if (empty? s) '()
      (let [new-c (inc-c (last s))]
        (if (nil? new-c)
          (str/join (concat (inc-s (butlast s)) '(\a)))
          (str/join (concat (butlast s) (list new-c)))
          ))))

(defn find-next-password [password]
  (first (drop-while invalid? (rest (iterate inc-s password)))))

;; (time (find-next-password "hepxcrrq"))
;; => "hepxxyzz"
;; "Elapsed time: 3999.7289 msecs"


;; (time (find-next-password "hepxxyzz"))
;; "heqaabcc"
;; "Elapsed time: 10274.7498 msecs"
