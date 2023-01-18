(ns advent-18.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

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

(defn label-nodes [tree]
  (let [node-count (atom 0)]
    (walk/postwalk (fn [node]
                     (let [node-name (str/join (list "n" (swap! node-count inc)))]
                       (if (number? node)
                         (apply conj [node-name] (list node))
                         (apply conj [node-name] node))))
                   tree)))

(defn print-graphviz [number]
  (println
   (str/join
    "\n"
    (map (fn [node]
           (if (> (count node) 2)
             (let [[node-name child1 child2] node]
               (format "%s -> %s, %s" node-name (first child1) (first child2)))
             (format "%s [label=\"%s\"]" (first node) (second node))))
         (tree-seq #(> (count %) 2) rest (label-nodes number))))))

(defn leaf? [x]
  (= 2 (count x)))

(defn dfs [node fn path]
  (let [[name child1 child2] node]
    (remove nil?
            (list (if (not (leaf? child1)) (dfs child1 fn (conj path 0)))
                  (fn node path)
                  (if (not (leaf? child2)) (dfs child2 fn (conj path 1)))))))

(defn process-number [number-string]
  (let [number (label-nodes (load-string number-string))
        prev-number (atom nil)
        deep-pair (atom nil)
        next-number (atom nil)
        process-node (fn [[name child1 child2 :as node] path]
                       (if (and (nil? @deep-pair)
                                (leaf? child1)
                                (leaf? child2)
                                (>= (count path) 4))
                         (reset! deep-pair (list node path)))
                       (if (and (nil? @deep-pair)
                                (or (leaf? child1) (leaf? child2)))
                         (reset! prev-number (if (leaf? child2) (list child2 (conj path 1)) (list child1 (conj path 0)))))
                       (if (and (not (nil? @deep-pair))
                                (not= (first @deep-pair) node)
                                (nil? @next-number)
                                (or (leaf? child1)
                                    (leaf? child2)))
                         (reset! next-number (if (leaf? child1) (list child1 (conj path 0)) (list child2 (conj path 1)))))
                       (println (list (list (if (leaf? child1) (second child1) (first child1))
                                            (if (leaf? child2) (second child2) (first child2)))
                                      path))
                       )]
    (dfs number process-node [])
    (list @prev-number @deep-pair @next-number)))

;; TODO: figure out what the left and right numbers are for an exploding pair.
;; AKA: enumerate the leaf nodes from left to right.
