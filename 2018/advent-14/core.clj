(ns advent-14.core)

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

;; Day 14: Chocolate Charts

;; Part 1
;; What are the scores of the ten recipies after the given number?
(def initial-recipes 37)
(def large-input "077201")

;; In Clojure (and Java) numbers with a leading zero are assumed to be octal, so this doesn't work.
(defn digits [n]
  (if (< n 10)
    [n]
    (let [quotient (quot n 10)
          remainder (rem n 10)]
      (conj (digits quotient) remainder))))

(defn from-digits [digits]
  (loop [digits digits n 0]
    (if (empty? digits)
      n
      (recur (rest digits) (+ (* 10 n) (first digits))))))

(defn str-recipes [recipes p0 p1]
  (str/join
   (map-indexed (fn [i recipe]
                  (cond (= i p0) (str/join (list "(" recipe ")"))
                        (= i p1) (str/join (list "[" recipe "]"))
                        :else (str/join (list " " recipe " "))))
                recipes)))

(defn expand-recipes [recipe-num n]
  (loop [recipes (digits recipe-num) p0 0 p1 1]
    (if (>= (count recipes) (+ n 10))
      (from-digits (take 10 (drop n recipes)))
      (let [expanded (apply conj recipes (digits (+ (get recipes p0) (get recipes p1))))
            new-p0 (mod (+ p0 1 (get recipes p0)) (count expanded))
            new-p1 (mod (+ p1 1 (get recipes p1)) (count expanded))]
        (recur expanded new-p0 new-p1)))))

(deftest test-expand-recipes
  (is (= 5158916779) (expand-recipes initial-recipes 9)) 
  (is (= 124515891) (expand-recipes initial-recipes 5)) 
  (is (= 9251071085) (expand-recipes initial-recipes 18)) 
  (is (= 5941429882) (expand-recipes initial-recipes 2018)))

;; (expand-recipes initial-recipes large-input)
;; 9211134315



;; Part 2
;; How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?

;; Necessary to handle leading zeros.
(defn to-digits [s]
  (->> s
       (vec)
       (map str)
       (map parse-long)))

(defn recipes-to-left-of [target]
  (loop [i 0 recipes (digits initial-recipes) p0 0 p1 1]
    (if (= 0 (mod i 1000)) (println "iteration" i))
    (let [v0 (get recipes p0)
          v1 (get recipes p1)
          new-recipes (apply conj recipes (digits (+ v0 v1)))
          new-p0 (mod (+ p0 1 v0) (count new-recipes))
          new-p1 (mod (+ p1 1 v1) (count new-recipes))
          end (take-last (inc (count target)) new-recipes)]
      (cond (= (rest end) target) (- (count new-recipes) (count target))
            (= (drop-last end) target) (- (count new-recipes) (inc (count target)))
            :else (recur (inc i) new-recipes new-p0 new-p1)))))

(deftest test-recipes-to-left-of
  (is (= 9 (recipes-to-left-of (to-digits "51589"))))
  (is (= 5 (recipes-to-left-of (to-digits "01245"))))
  (is (= 18 (recipes-to-left-of (to-digits "92510"))))
  (is (= 2018 (recipes-to-left-of (to-digits "59414")))))

(defn debug-expand-recipes [recipe-num n]
  (loop [i 0 recipes (digits recipe-num) p0 0 p1 1]
    (if (= i n)
      recipes
      (let [expanded (apply conj recipes (digits (+ (get recipes p0) (get recipes p1))))
            new-p0 (mod (+ p0 1 (get recipes p0)) (count expanded))
            new-p1 (mod (+ p1 1 (get recipes p1)) (count expanded))]
        (recur (inc i) expanded new-p0 new-p1)))))

(defn recipes-to-left-linked-list [target]
  (loop [i 0
         recipes (into {} (map-indexed vector (digits initial-recipes)))
         recipes-end (digits initial-recipes)
         recipe-count (count recipes)
         p0 0
         p1 1]
    ;; (if (= 0 (mod i 1000000)) (println "iteration" i))
    (let [v0 (get recipes p0)
          v1 (get recipes p1)
          new-digits (digits (+ v0 v1))
          new-recipes (apply conj recipes (map-indexed (fn [i v] [(+ i recipe-count) v]) new-digits))
          new-recipe-count (+ recipe-count (count new-digits))
          new-p0 (mod (+ p0 1 v0) new-recipe-count)
          new-p1 (mod (+ p1 1 v1) new-recipe-count)
          new-recipes-end (vec (take-last (inc (count target)) (apply conj recipes-end new-digits)))]
      (cond (= (rest new-recipes-end) target) (- new-recipe-count (count target))
            (= (drop-last new-recipes-end) target) (- new-recipe-count (inc (count target)))
            :else (recur (inc i) new-recipes new-recipes-end new-recipe-count new-p0 new-p1)))))

;; (time (recipes-to-left-linked-list (to-digits "077201")))
;; "Elapsed time: 110886.4501 msecs"
;; 20357548
