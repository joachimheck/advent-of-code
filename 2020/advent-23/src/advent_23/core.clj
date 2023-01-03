(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def input-1 [3 8 9 1 2 5 4 6 7])
(def puzzle-input [2 1 5 6 9 4 7 8 3])

;; Part 1
;; Move cups 100 times

(def ^:dynamic maxnum 9)

(defn wrap [x]
  (+ 1 (mod (- x 1) maxnum)))

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
                (vector current)))))

(defn move-repeat [cups n move-f]
  (let [cups (nth (iterate move-f cups) n)
        one-idx (.indexOf cups 1)]
    (str/join
     (concat
      (subvec cups (inc one-idx))
      (subvec cups 0 one-idx)))))

;; (move-repeat puzzle-input 100 move-cups)
;; "46978532"



;; Part 2
;; A million cups!

;; 5 4 3 2 1 <6-1000000> -- pick up [4 3 2] dest 1 next 1
;; 1 4 3 2 <6-1000000> 5 -- pick up [4 3 2] dest 1000000 next 6
;; 6 <7-1000000> 4 3 2 5 1 -- pick up [7 8 9] dest 5 next 10
;; 10 <11-1000000> 4 3 2 5 7 8 9 1 6

;; (defn expand-range
;;   ([[min max :as range]] (expand-range range 4))
;;   ([[min max] n]
;;    (if (< (- max min) (inc n))
;;      (for [i (range (inc (- max min)))] (+ i min))
;;      (conj
;;       (apply vector
;;              (flatten (for [i (range n)] (+ i min))))
;;       (vector (+ n min) max)))))
             
;; (defn expand-ranges [cups]
;;   (vec (reduce concat []
;;                (map (fn [e]
;;                       (if (vector? e)
;;                         (expand-range e)
;;                         [e]))
;;                     cups))))

;; (defn contract-ranges
;;   ;;  (println a b rest (apply (partial vector (vector a b)) rest))
;;   [[a b & rest]]
;;   (cond (nil? a) []
;;         (nil? b) [a]
;;         (and (number? a) (number? b))
;;         (if (= (inc a) b)
;;           (contract-ranges (apply (partial vector (vector a b)) rest))
;;           (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

;;         (and (number? a) (vector? b))
;;         (if (= (inc a) (first b))
;;           (contract-ranges (apply (partial vector (vector a (second b))) rest))
;;           (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

;;         (and (vector? a) (number? b))
;;         (if (= (inc (second a)) b)
;;           (contract-ranges (apply (partial vector (vector (first a) b)) rest))
;;           (vec (concat [a] (contract-ranges (vec (concat [b] rest))))))

;;         (and (vector? a) (vector? b))
;;         (if (= (inc (second a)) (first b))
;;           (contract-ranges (apply (partial vector (vector (first a) (second b))) rest))
;;           (vec (concat [a] (contract-ranges (concat [b] rest)))))))

;; (defn break-range-at [[a b :as rng] n]
;;   (cond (< (- b a) 5)
;;         (apply vector (for [i (range a (inc b))] i))
;;         (= a n) [n [(inc n) b]]
;;         (= b n) [[a (dec n)] b]
;;         (= (inc a) n) [a n [(inc n) b]]
;;         (= (dec b) n) [[a (dec n)] n b]
;;         :else [[a (dec n)] n [(inc n) b]]))

;; (defn expand-range-around [v n]
;;   (if (some #{n} v)
;;     v
;;     (mapcat (fn [e]
;;               (if (number? e) [e]
;;                   (let [[a b] e]
;;                     (if (<= a n b)
;;                       (break-range-at e n)
;;                       e))))
;;             v)))


;; ;; (take 4 (iterate contract-ranges [1 2 3 [4 10] 11 [12 150]]))
;; (partition 2 (contract-ranges [1 2 3 [4 10] 11 [12 150]]))
;; (contract-ranges [11 1 2 3 [4 10] [12 150]])
;; (expand-ranges [[1 10]])

;; (defn move-cups-2 [cups-compressed]
;;   (let [cups (expand-ranges cups-compressed)
;;         current (get cups 0)
;;         to-move (subvec cups 1 4)
;;         destination (destination current (set to-move))
;;         removed (subvec cups 4)
;;         expanded (expand-range-around removed destination)
;;         dest-idx (.indexOf expanded destination)
;;         until-dest (subvec expanded 0 dest-idx)
;;         after-dest (subvec expanded (inc dest-idx))]
;;     ;; (println cups current to-move destination removed until-dest after-dest)
;;     (contract-ranges (into [] (concat
;;                                until-dest
;;                                (vector destination)
;;                                to-move
;;                                after-dest
;;                                (vector current))))))

;; ;; (move-repeat puzzle-input 100 move-cups-2)
;; ;; => "46978532"

;; (def puzzle-input-2 [2 1 5 6 9 4 7 8 3 [10 1000000]])

;; ;;(time (move-repeat puzzle-input-2 200 move-cups-2))
;; ;;(time (nth (iterate move-cups-2 puzzle-input-2) 200))
;; (take 200 (map count (iterate move-cups-2 puzzle-input-2)))


;; Following a hint from https://www.reddit.com/r/adventofcode/comments/kixn6z/2020_day_23_part_2python/
;; I think I'll try a linked list.

(defn make-list [input]
  (into {} (map vec (partition 2 1 (conj input (first input))))))

(defn make-long-list [input end]
  (into {} (map vec (partition 2 1 (concat [end] input [(inc (apply max input))])))))

(list input-1 (make-list input-1))
;; (make-list input-1)
;; (make-long-list input-1 12345)

(defn get-or-next [m k]
  (let [v (get m k)]
    (if v v (inc k))))

(defn move-cups-list
  ([[cups current]] (move-cups-list cups current))
  ([cups current]
   ;; (println "move-cups-list" cups current)
   (let [[a b c after-trio] (drop 1 (take 5 (iterate (partial get-or-next cups) current)))
         dest (destination current (set (list a b c)))
         after-dest (get-or-next cups dest)]
     (list (assoc cups
                  current after-trio
                  dest a
                  c after-dest)
           after-trio))))

(defn move-repeat-list [cups start n mx]
  (binding [maxnum mx]
    (let [out-length (dec (count cups))
          result (first (nth (iterate move-cups-list (list cups start)) n))]
      (take
       8
       (iterate (partial get-or-next result) (get-or-next result 1))))))


;; (str/join (move-repeat-list (make-list input-1) (first input-1) 100 9))
;; "67384529"

;; (time (move-repeat-list (make-long-list input-1 1000000) (first input-1) 10000000))
;; => (934001 159792)
;; "Elapsed time: 58402.4097 msecs"

;; (time (move-repeat-list (make-long-list puzzle-input 1000000) (first puzzle-input) 10000000))
;; => (250343 651247)
;; "Elapsed time: 58402.4097 msecs"

;; (* 250343 651247)
;; => 163035127721
