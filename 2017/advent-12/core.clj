(ns advent-12.core)

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

;; Day 12: Digital Plumber

;; Part 1
;; How many programs are in the group that can communicate with program 0?
(defn parse-input [f]
  (->> f
       read-lines
       (map #(re-seq #"(\d+)" %))
       (map #(map first %))
       (map #(map parse-long %))
       (reduce (fn [acc x] (assoc acc (first x) (rest x))) {})))

(defn find-group [programs start]
  (loop [open-set #{start} group #{}]
    (if (empty? open-set)
      group
      (let [program (first open-set)
            reduced-open-set (disj open-set program group)
            expanded-open-set (apply conj reduced-open-set (get programs program))
            new-group (conj group program)
            new-open-set (apply disj expanded-open-set new-group)]
        (recur new-open-set new-group)))))

;; (time (count (find-group (parse-input small-input) 0)))
;; "Elapsed time: 0.5672 msecs"
;; 6

;; (time (count (find-group (parse-input large-input) 0)))
;; "Elapsed time: 20.0179 msecs"
;; 128



;; Part 2
(defn find-all-groups [programs]
  (loop [open-set (set (keys programs)) groups {}]
    (if (empty? open-set)
      groups
      (let [program (first open-set)
            group (find-group programs program)
            new-groups (assoc groups program group)
            reduced-open-set (apply disj open-set group)
            new-open-set (disj reduced-open-set program)]
        (recur new-open-set new-groups)))))

;; (time (count (find-all-groups (parse-input small-input))))
;; "Elapsed time: 0.5806 msecs"
;; 2

;; (time (count (find-all-groups (parse-input large-input))))
;; "Elapsed time: 50.2427 msecs"
;; 209
