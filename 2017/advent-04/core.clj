(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 4: High-Entropy Passphrases

;; Part 1
;; How many of the provided passphrases are valid?
(defn parse-input [f]
  (->> f
       read-lines
       (map #(str/split % #" "))))


(defn all-ones? [coll]
  (apply = (concat coll '(1))))

(defn valid? [phrase]
  (->> phrase
       frequencies
       vals
       all-ones?))

(defn count-valid [phrases]
  (->> phrases
       (map valid?)
       (filter true?)
       count))

;; 456
;; ---> answer <---

(defn find-duplicate-words [phrase]
  (->> phrase
       frequencies
       (remove (fn [[k v]] (= v 1)))
       (map first)))

;; (let [{valid-phrases true invalid-phrases false} (group-by valid? (parse-input large-input))]
;;                   (filter #(not (empty? (second %))) (map #(list % (find-duplicate-words %)) valid-phrases)))
;; ((["opjhdp" "svwezr" "svwezr" "opjhdp"] ("opjhdp" "svwezr")))

;; The valid? function was accepting phrases in which each item was duplicated the same number of times.

;; (time (count-valid (parse-input large-input)))
;; "Elapsed time: 6.3422 msecs"
;; 455



;; Part 2
;; No anagrams allowed.
(defn parse-input-sorted [f]
 (->> f
      read-lines
      (map #(str/split % #" "))
      (map #(map sort %))))
