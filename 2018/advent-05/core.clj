(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 5: Alchemical Reduction

;; Part 1
;; How many units are in the polymer after it reacts?
(defn parse-input [f]
  (->> f
       (read-lines)
       (first)))

(defn match? [a b]
  (= 32 (abs (- (int a) (int b)))))

(defn match-point [p]
  (->> p
       (partition 2 1)
       (map-indexed list)
       (filter (fn [[i [a b]]] (if (match? a b) i)))
       (first)
       (first)))

(def matches (let [alphabet (vec "abcdefghijklmnopqrstuvwxyz")
                   caps-alphabet (mapv #(char (- (int %) 32)) alphabet)]
               (str/join "|" (concat (map #(str/join [%1 %2]) alphabet caps-alphabet)
                                     (map #(str/join [%2 %1]) alphabet caps-alphabet)))))

(defn react-polymer [p]
  (loop [p p i 0]
    (let [new-p (str/replace p (re-pattern matches) "")]
      (if (= (count p) (count new-p))
        p
        (recur new-p (inc i))))))

;; (time (count (react-polymer (parse-input small-input))))
;; "Elapsed time: 0.3913 msecs"
;; 10

;; (time (count (react-polymer (parse-input large-input))))
;; "Elapsed time: 2863.4194 msecs"
;; 11194



;; Part 2
;; What's the length of the shortest polymer you can make by first removing all of one pair?
(def match-pairs (let [alphabet (vec "abcdefghijklmnopqrstuvwxyz")
                       caps-alphabet (mapv #(char (- (int %) 32)) alphabet)]
                   (map #(str/join "|" [%1 %2]) alphabet caps-alphabet)))

(defn remove-then-reduce [p]
  (apply min-key second
         (for [pair match-pairs]
           (list pair (count (react-polymer (str/replace p (re-pattern pair) "")))))))


;; (time (remove-then-reduce (parse-input small-input)))
;; "Elapsed time: 1.9753 msecs"
;; ("c|C" 4)

;; (time (remove-then-reduce (parse-input large-input)))
;; "Elapsed time: 80556.0782 msecs"
;; ("c|C" 4178)
