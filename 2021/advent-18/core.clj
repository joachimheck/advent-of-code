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

;; Day 18: Snailfish

;; Part 1
;; What is the magnitude of the sum of the given snailfish numbers?
(def some-numbers
  (map load-string
       (list "[1,2]"
             "[[1,2],3]"
             "[9,[8,7]]"
             "[[1,9],[8,5]]"
             "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
             "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
             "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")))

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

(defn dfs [node fn path]
  (let [[child1 child2] node]
    (remove nil?
            (list (if (not (number? child1)) (dfs child1 fn (conj path 0)))
                  (fn node path)
                  (if (not (number? child2)) (dfs child2 fn (conj path 1)))))))

(defn explode [number]
  (let [prev-number (atom nil)
        deep-pair (atom nil)
        next-number (atom nil)
        process-node (fn [[child1 child2 :as node] path]
                       (if (and (nil? @deep-pair)
                                (number? child1)
                                (number? child2)
                                (>= (count path) 4))
                         (reset! deep-pair (list node path)))
                       (if (and (nil? @deep-pair)
                                (or (number? child1) (number? child2)))
                         (reset! prev-number (if (number? child2) (list child2 (conj path 1)) (list child1 (conj path 0)))))
                       (if (and @deep-pair
                                (not= (second @deep-pair) path)
                                (nil? @next-number)
                                (or (number? child1)
                                    (number? child2)))
                         (reset! next-number (if (number? child1) (list child1 (conj path 0)) (list child2 (conj path 1))))))
        assoc-if (fn [c m ks v] (if (c) (assoc-in m ks v) m))]
    (dfs number process-node [])
    (let [[prev-n prev-path] @prev-number
          [[deep-l deep-r] deep-path] @deep-pair
          [next-n next-path] @next-number]
      ;; (if @deep-pair (println "found deep pair" @deep-pair "prev" @prev-number "next" @next-number))
      (as-> number n
        (if (and @deep-pair @prev-number) (assoc-in n prev-path (+ prev-n deep-l)) n)
        (if @deep-pair (assoc-in n deep-path 0) n)
        (if (and @deep-pair @next-number) (assoc-in n next-path (+ next-n deep-r)) n)))))

(deftest test-explode
  (is (= (explode (load-string "[[[[[9,8],1],2],3],4]")) (load-string "[[[[0,9],2],3],4]")))
  (is (= (explode (load-string "[7,[6,[5,[4,[3,2]]]]]")) (load-string "[7,[6,[5,[7,0]]]]")))
  (is (= (explode (load-string "[[6,[5,[4,[3,2]]]],1]")) (load-string "[[6,[5,[7,0]]],3]")))
  (is (= (explode (load-string "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) (load-string "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))))

(defn split [number]
  (let [big-n (atom nil)
        process-node (fn [[child1 child2 :as node] path]
                       (if (nil? @big-n)
                         (cond (and (number? child1) (> child1 9)) (reset! big-n (list child1 (conj path 0)))
                               (and (number? child2)(> child2 9)) (reset! big-n (list child2 (conj path 1))))))]
    (dfs number process-node [])
    (if-let [[n path] @big-n]
      (do
        ;; (println "found big number" @big-n)
        (assoc-in number path [(int (/ n 2)) (int (/ (inc n) 2))]))
      number)))

(deftest test-split
  (is (= (split (load-string "[[[[0,7],4],[15,[0,13]]],[1,1]]")) (load-string "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")))
  (is (= (split (load-string "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")) (load-string "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))))

(defn reduce-number [number]
  (loop [number number]
    ;; (println "reducing" number)
    (let [exploded (explode number)]
      (if (= exploded number)
        (let [splitted (split number)]
          (if (= splitted number)
            number ;; done
            (recur splitted)))
        (recur exploded)))))

(defn add [n1 & ns]
  (reduce (fn [acc nx]
            ;; (println " " acc)
            ;; (println "+" nx)
            ;; (println "=" (reduce-number [acc nx]))
            ;; (newline)
            (reduce-number [acc nx]))
          n1
          ns))

(deftest test-add
  (is (= (apply add (mapv load-string '("[1,1]" "[2,2]" "[3,3]" "[4,4]")))
         (load-string "[[[[1,1],[2,2]],[3,3]],[4,4]]")))
  (is (= (apply add (mapv load-string '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]")))
         (load-string "[[[[3,0],[5,3]],[4,4]],[5,5]]")))
  (is (= (apply add (mapv load-string '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]" "[6,6]")))
         (load-string "[[[[5,0],[7,4]],[5,5]],[6,6]]")))
  (is (= (load-string "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
         (apply add (mapv load-string '("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                                        "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                                        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                                        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                                        "[7,[5,[[3,8],[1,4]]]]"
                                        "[[2,[2,2]],[8,[8,1]]]"
                                        "[2,9]"
                                        "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                                        "[[[5,[7,4]],7],1]"
                                        "[[[[4,2],2],6],[8,7]]"))))))

(defn magnitude [number]
  (if (number? number)
    number
    (let [[left right] number]
      (+ (* 3 (magnitude left)) (* 2 (magnitude right))))))

(deftest test-magnitude
  (is (= 143 (magnitude (load-string "[[1,2],[[3,4],5]]")))) 
  (is (= 1384 (magnitude (load-string "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))) 
  (is (= 445 (magnitude (load-string "[[[[1,1],[2,2]],[3,3]],[4,4]]")))) 
  (is (= 791 (magnitude (load-string "[[[[3,0],[5,3]],[4,4]],[5,5]]")))) 
  (is (= 1137 (magnitude (load-string "[[[[5,0],[7,4]],[5,5]],[6,6]]")))) 
  (is (= 3488 (magnitude (load-string "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))))


;; (time (->> small-input
;;            read-lines
;;            (map load-string)
;;            (apply add)
;;            magnitude))
;; "Elapsed time: 43.4053 msecs"
;; 4140

;; (time (->> large-input
;;            read-lines
;;            (map load-string)
;;            (apply add)
;;            magnitude))
;; "Elapsed time: 316.9501 msecs"
;; 3884



;; Part 2
;; What's the largest magnitude achievable by adding two of the given numbers?
(defn permute [coll]
  (if (not (seq? coll))
    coll
    (if (= 2 (count coll))
      (list coll (reverse coll))
      (let [pairs (map #(list (first coll) %) (rest coll))]
        (concat pairs (map reverse pairs) (permute (rest coll)))))))

;; (time (->> small-input
;;                      read-lines
;;                      (map load-string)
;;                      permute
;;                      (map (fn [[a b]] (add a b)))
;;                      (map magnitude)
;;                      (apply max)))
;; "Elapsed time: 56.4489 msecs"
;; 3993

;; (time (->> large-input
;;                      read-lines
;;                      (map load-string)
;;                      permute
;;                      (map (fn [[a b]] (add a b)))
;;                      (map magnitude)
;;                      (apply max)))
;; "Elapsed time: 3774.9361 msecs"
;; 4595
