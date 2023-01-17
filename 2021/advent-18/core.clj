(ns advent-18.core)

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

;; Day 15: Snailfish

;; Part 1
;; What is the magnitude of the sum of the given snailfish numbers?
(defn parse-snailfish-number [s]
  (as-> s s
       (str/replace s "[" "(")
       (str/replace s "]" ")")
       (str/join (list "'" s))
       (load-string s)))

(def some-numbers
  (map parse-snailfish-number
       (list "[1,2]"
             "[[1,2],3]"
             "[9,[8,7]]"
             "[[1,9],[8,5]]"
             "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
             "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
             "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")))

(defn explode [x]
  x)

(deftest snail-addition
  (is (= (explode (parse-snailfish-number "[[[[[9,8],1],2],3],4]")) (parse-snailfish-number "[[[[0,9],2],3],4]")))
  (is (= (explode (parse-snailfish-number "[7,[6,[5,[4,[3,2]]]]]")) (parse-snailfish-number "[7,[6,[5,[7,0]]]]")))
  (is (= (explode (parse-snailfish-number "[[6,[5,[4,[3,2]]]],1]")) (parse-snailfish-number "[[6,[5,[7,0]]],3]")))
  (is (= (explode (parse-snailfish-number "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
         (parse-snailfish-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))))

(defn depth
  ([x] (depth x 0))
  ([x d]
   (if (seq? x)
     (apply max (map #(depth % (inc d)) x))
     d)))

;; TODO: write a function to turn a number into graphviz
;; TODO: figure out what the left and right numbers are for an exploding pair.
