(ns advent-02.core)

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

;; Day 2: Inventory Management System

;; Part 1
;; What is the checksum for the list of box ids?
(defn parse-input [f]
  (->> f
       (read-lines)
       (vec)))

(defn compute-checksum [ids]
  (let [freqs (map frequencies ids)
        with-2 (count (filter #(some #{2} (vals %)) freqs))
        with-3 (count (filter #(some #{3} (vals %)) freqs))]
    (* with-2 with-3)))

;; (compute-checksum (parse-input small-input))
;; 12

;; (compute-checksum (parse-input large-input))
;; 9633



;; Part 2
;; What letters are common between the two correct box ids?
(def small-input-2 "small-input-2.txt")

(defn differ-by-one? [a b]
  (= 1
     (count (for [i (range (count a))
            :when (not= (get a i) (get b i))]
        1))))

(defn find-ids-differing-by-one [ids]
  (for [i (range (count ids))
        j (range (count ids))
        :when (differ-by-one? (get ids i) (get ids j))]
    (list (get ids i) (get ids j))))

(defn letters-in-common [a b]
  (apply str
         (for [i (range (count a))
               :when (= (get a i) (get b i))]
           (get a i))))

;; (time (apply letters-in-common (first (find-ids-differing-by-one (parse-input small-input-2)))))
;; "Elapsed time: 1.0697 msecs"
;; "fgij"

;; (time (apply letters-in-common (first (find-ids-differing-by-one (parse-input large-input)))))
;; "Elapsed time: 101.3871 msecs"
;; "lujnogabetpmsydyfcovzixaw"
