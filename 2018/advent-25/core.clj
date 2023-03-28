(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input-1 "small-input-1.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 25: Four Dimensional Adventure

;; Part 1
;; How many constellations are formed by the fixed points in spacetime?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/join (list "[" % "]")))
       (map load-string)))

(defn distance [a b]
  (apply + (map #(abs (- %2 %1)) a b)))

(defn near [as bs]
  (first
   (filter #(<= % 3)
           (for [a as b bs] (distance a b)))))

(defn make-constellations [points]
  (loop [open-set (map #(set (list %)) points)
         constellations '()]
    ;; (println "loop" open-set constellations)
    (if (empty? open-set)
      constellations
      (let [[a & not-a] (sort-by count open-set)
            [new-open-set new-constellations] (loop [remaining not-a]
                                                ;; (println "loop2" a remaining)
                                                (if (empty? remaining)
                                                  [not-a (conj constellations a)]
                                                  (let [b (first remaining)]
                                                    (if (near a b)
                                                      [(conj (remove #{b} not-a) (apply conj a b)) constellations]
                                                      (recur (rest remaining))))))]
        (recur new-open-set new-constellations)))))


;; (time (count (make-constellations (parse-input large-input))))
;; "Elapsed time: 3475.7082 msecs"
;; 375
