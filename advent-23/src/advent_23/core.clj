(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def input-1 [3 8 9 1 2 5 4 6 7])
(def puzzle-input [2 1 5 6 9 4 7 8 3])

;; Part 1
;; Move cups 100 times

(defn wrap [x]
  (+ 1 (mod (- x 1) 9)))

(defn destination [start skip-set]
  (let [proposed (wrap (- start 1))]
    (if (skip-set proposed) (destination proposed skip-set)
        proposed)))

(defn move-cups [cups]
  (let [current (get cups 0)
        to-move (subvec cups 1 4)
        destination (destination current (set to-move))
        removed (subvec cups 4)
        dest-idx (.indexOf removed destination)
        until-dest (subvec removed 0 dest-idx)
        after-dest (subvec removed (inc dest-idx))
        ]
      (println cups current to-move destination removed until-dest after-dest)
      (into [] (concat
                until-dest
                (vector destination)
                to-move
                after-dest
                (vector current)
                       ))))

(defn move-repeat [cups n]
  (let [cups (nth (iterate move-cups cups) n)
        one-idx (.indexOf cups 1)]
    (str/join
     (concat
      (subvec cups (inc one-idx))
      (subvec cups 0 one-idx)))))

;; (move-repeat puzzle-input 100)
;; "46978532"



;; Part 2
;; A million cups!

;; 5 4 3 2 1 <6-1000000> -- pick up [4 3 2] dest 1 next 1
;; 1 4 3 2 <6-1000000> 5 -- pick up [4 3 2] dest 1000000 next 6
;; 6 <7-1000000> 4 3 2 5 1 -- pick up [7 8 9] dest 5 next 10
;; 10 <11-1000000> 4 3 2 5 7 8 9 1 6

(defn expand-range
  ([[min max :as range]] (expand-range range 4))
  ([[min max] n]
   (if (< (- max min) (inc n))
     (for [i (range (inc (- max min)))] (+ i min))
     (conj
      (apply vector
             (flatten (for [i (range n)] (+ i min))))
      (vector (+ n min) max)))))
             
(defn expand-ranges [cups]
  (reduce concat []
          (map (fn [e]
                 (if (vector? e)
                   (expand-range e)
                   [e]))
               cups)))

(defn contract-ranges
  ;;  (println a b rest (apply (partial vector (vector a b)) rest))
  [[a b & rest]]
  (cond (nil? a) []
        (nil? b) [a]
        (and (number? a) (number? b))
        (if (= (inc a) b)
          (contract-ranges (apply (partial vector (vector a b)) rest))
          (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

        (and (number? a) (vector? b))
        (if (= (inc a) (first b))
          (contract-ranges (apply (partial vector (vector a (second b))) rest))
          (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

        (and (vector? a) (number? b))
        (if (= (inc (second a)) b)
          (contract-ranges (apply (partial vector (vector (first a) b)) rest))
          (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

        (and (vector? a) (vector? b))
        (if (= (inc (second a)) (first b))
          (contract-ranges (apply (partial vector (vector (first a) (second b))) rest))
          (vec (concat [a] (contract-ranges (concat [b] rest)))))))

(defn break-range-at [[a b :as rng] n]
  (cond (< (- b a) 5)
        (apply vector (for [i (range a (inc b))] i))
        (= a n) [n [(inc n) b]]
        (= b n) [[a (dec n)] b]
        (= (inc a) n) [a n [(inc n) b]]
        (= (dec b) n) [[a (dec n)] n b]
        :else [[a (dec n)] n [(inc n) b]]))

(let [v [1 2 [5 10] 12 13]
      n 6]
  (if (some #{n} v)
    v
    (mapcat (fn [e]
              (if (number? e) [e]
                  (let [[a b] e]
                    (if (<= a n b)
                      (break-range-at e n)
                      e))))
            v)))


;; (take 4 (iterate contract-ranges [1 2 3 [4 10] 11 [12 150]]))
(partition 2 (contract-ranges [1 2 3 [4 10] 11 [12 150]]))
(contract-ranges [11 1 2 3 [4 10] [12 150]])
(expand-ranges [[1 10]])

(defn move-cups-2 [cups-compressed]
  (let [cups (expand-ranges cups-compressed)
        current (get cups 0)
        to-move (subvec cups 1 4)
        destination (destination current (set to-move))
        removed (subvec cups 4)
        expanded (expand-range-around removed destination)
        dest-idx (.indexOf expanded destination)
        until-dest (subvec expanded 0 dest-idx)
        after-dest (subvec expanded (inc dest-idx))
        ]
      (println cups current to-move destination removed until-dest after-dest)
      (into [] (concat
                until-dest
                (vector destination)
                to-move
                after-dest
                (vector current)
                       ))))
