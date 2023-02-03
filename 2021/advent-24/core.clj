(ns advent-24.core)

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

;; Day 23: Arithmetic Logic Unit

;; Part 1
;; Find the largest number accepted by the MONAD validation program.
(defn parse-line [[op arg1 arg2]]
  (let [variables #{"w" "x" "y" "z"}]
   {:op op
    :arg1 (if (some variables #{arg1})
            arg1
            (parse-long arg1))
    :arg2 (cond (nil? arg2) nil
                (some variables #{arg2}) arg2
                :else (parse-long arg2))}))

(defn parse-lines [lines]
  (->> lines
       (map #(re-matches #"(...) ([w-z0-9\-]+) ?(.+)?" %))
       (map rest)
       (map parse-line)))

(defn parse-input [f]
  (parse-lines (read-lines f)))

(def micro-input '("inp x"
                   "mul x -1"))

(def mini-input '("inp z"
                  "inp x"
                  "mul z 3"
                  "eql z x"))

(defn value [variables arg]
  (if (some #{"w" "x" "y" "z"} #{arg})
    (get variables arg)
    arg))

(defn execute
  ([program inputs] (execute program inputs {"w" 0 "x" 0 "y" 0 "z" 0}))
  ([program inputs init-values]
   (loop [program program inputs inputs variables (into (sorted-map) init-values)]
     (if (or (empty? program) (empty? inputs))
       variables
       (let [{op :op arg1 :arg1 arg2 :arg2} (first program)
             a (value variables arg1)
             b (value variables arg2)]
         (case op
           "inp" (recur (rest program) (rest inputs) (assoc variables arg1 (first inputs)))
           "add" (recur (rest program) inputs (assoc variables arg1 (+ a b)))
           "mul" (recur (rest program) inputs (assoc variables arg1 (* a b)))
           "div" (if (= b 0)
                   :divide-by-zero
                   (recur (rest program) inputs (assoc variables arg1 (long (/ a b)))))
           "mod" (cond (< a 0) :mod-negative
                       (< b 0) :mod-by-negative
                       (= b 0) :mod-by-zero
                       :else (recur (rest program) inputs (assoc variables arg1 (mod a b))))
           "eql" (recur (rest program) inputs (assoc variables arg1 (if (= a b) 1 0)))))))))

(defn execute-z
  ([program inputs] (get (execute program inputs) "z"))
  ([program inputs init-values] (get (execute program inputs init-values) "z")))

(defn digits [num]
  (->> num
       str
       vec
       (map str)
       (map parse-long)))

(defn execute-monad [program num]
  (let [result (execute program (digits num))]
    (list (if (= 0 (get result "z")) :valid :invalid)
          result)))

(defn binary-search-monad [f]
  (let [start-min 11111111111111
        start-max 99999999999999
        program (parse-input f)]
    (loop [min-num start-min max-num start-max]
      (println "binary search from" min-num "to" max-num)
      (let [num (long (/ (+ min-num max-num) 2))
            result (execute program (digits num))
            valid (= 0 (get result "z"))]
        (if (= min-num max-num)
          {:valid valid :result result}
         (if valid
           (recur num max-num)
           (recur min-num num)))))))

(defn exhaustive-monad [f iterations]
  (let [program (parse-input f)]
    (loop [i 99999999999999]
      (if (<= i (- 99999999999999 iterations))
        {:invalid-down-to i}
        (let [digits (digits i)]
          (if (some #{0} digits)
            (recur (dec i))
            (let [result (execute program digits)
                  valid (= 0 (get result "z"))]
              (if valid
                {:valid i}
                (recur (dec i))))))))))

;; x 0
;; x (+ x z)
;; x (mod x 26)
;; z (long (/ z v1))
;; x (+ x v2)
;; x (if (= x w) 1 0)
;; x (if (= x 0) 1 0)
;; y 0
;; y (+ y 25)
;; y (* y x)
;; y (+ y 1)
;; z (* z y)
;; y (* y 0)
;; y (+ y w)
;; y (+ y v3)
;; y (* y x)
;; result (+ z y)

(def v1s '( 1  1  1  1  26  1 26  1 26 26  26 26  1  26))
(def v2s '(14 11 11 14 -11 12 -1 10 -3 -4 -13 -8 13 -11))
(def v3s '(12  8  7  4   4  1 10  8 12 10  15  4 10   9))

(defn do-math [w z idx]
  (let [z-div (long (/ z (nth v1s idx)))
        x (+ (mod z 26) (nth v2s idx))
        result (if (= x w) z-div (+ (* z-div 26) w (nth v3s idx)))]
    result))

;; result = (+ (* (long (/ z (nth v1s idx))) 26) w (nth v3s idx))
;; (- result w (nth v3s idx)) = (* (long (/ z (nth v1s idx))) 26)
;; (/ (- result w (nth v3s idx)) 26) = (/ z (nth v1s idx))
;; (* (nth v1s idx) (/ (- result w (nth v3s idx)) 26)) = z
;; x = w
;; (+ (mod z 26) v2) = w
;; (mod z 26) = (- w v2)
(defn valid-prev-z? [x]
  (and
   (>= x 0)
   (integer? x)))

(defn determine-prev-zs [result idx]
  (let [v1 (nth v1s idx)
        v2 (nth v2s idx)
        v3 (nth v3s idx)]
    (filter #(valid-prev-z? (first %))
            (concat (for [digit (range 1 10)]
              (list (* v1 (/ (- result digit v3) 26)) digit))
            (for [digit (range 1 10)]
              (list (- digit v2) digit))))))

;; For the last digit, digit 13, we're looking for a result of 0, which requires z-div=0:
;; z-div is zero when z<v1, non-zero otherwise, so we only need to consider prev-z<26.
;; 1<=w<=9, so the minimum x such that x=w is 12 (12-11=1)
;; Therefore we only need to consider prev-z>=12
;; We only need to consider 12 <= prev-z <= 25.

;; For digit 12, we want 12 <= result <= 20.
;; Either z in [12*26, 13*26, ..., 20*26]
;; Or 10 + digit + 26*z = result, which only works if z=0 (for digit=2)

(defn do-multi [ws]
  (loop [ws ws i 0 z 0]
    (if (empty? ws)
      z
      (recur (rest ws) (inc i) (do-math (first ws) z i)))))

(defn generate-numbers []
  (for [d14 (range 9 0 -1)
        d13 (range 9 0 -1)
        d12 (range 9 0 -1)
        d11 (range 9 0 -1)
        d10 (range 9 0 -1)
        d9 (range 9 0 -1)
        d8 (range 9 0 -1)
        d7 (range 9 0 -1)
        d6 (range 9 0 -1)
        d5 (range 9 0 -1)
        d4 (range 9 0 -1)
        d3 (range 9 0 -1)
        d2 (range 9 0 -1)
        d1 (range 9 0 -1)]
    (list d14 d13 d12 d11 d10 d9 d8 d7 d6 d5 d4 d3 d2 d1)))

(def test-input
  '("inp w"
    "mul x 0"
    "add x z"
    "mod x 26"
    "div z 1"
    "add x 14"
    "eql x w"
    "eql x 0"
    "mul y 0"
    "add y 25"
    "mul y x"
    "add y 1"
    "mul z y"
    "mul y 0"
    "add y w"
    "add y 12"
    "mul y x"
    "add z y"))

(def test-input-2
  '("inp w"
    "mul x 0"
    "add x z"
    "mod x 26"
    "div z 1"
    "add x 11"
    "eql x w"
    "eql x 0"
    "mul y 0"
    "add y 25"
    "mul y x"
    "add y 1"
    "mul z y"
    "mul y 0"
    "add y w"
    "add y 8"
    "mul y x"
    "add z y"))

(def test-input-3
  (concat
   '("inp w"
     "mul x 0"
     "add x z"
     "mod x 26"
     "div z 1"
     "add x 14"
     "eql x w"
     "eql x 0"
     "mul y 0"
     "add y 25"
     "mul y x"
     "add y 1"
     "mul z y"
     "mul y 0"
     "add y w"
     "add y 12"
     "mul y x"
     "add z y")
   '("inp w"
     "mul x 0"
     "add x z"
     "mod x 26"
     "div z 1"
     "add x 11"
     "eql x w"
     "eql x 0"
     "mul y 0"
     "add y 25"
     "mul y x"
     "add y 1"
     "mul z y"
     "mul y 0"
     "add y w"
     "add y 8"
     "mul y x"
     "add z y")))

(def test-input-end
  '("inp w"
    "mul x 0"
    "add x z"
    "mod x 26"
    "div z 26"
    "add x -11"
    "eql x w"
    "eql x 0"
    "mul y 0"
    "add y 25"
    "mul y x"
    "add y 1"
    "mul z y"
    "mul y 0"
    "add y w"
    "add y 9"
    "mul y x"
    "add z y"))

(def test-input-end2
  '("inp w"
    "mul x 0"
    "add x z"
    "mod x 26"
    "div z 1"
    "add x 13"
    "eql x w"
    "eql x 0"
    "mul y 0"
    "add y 25"
    "mul y x"
    "add y 1"
    "mul z y"
    "mul y 0"
    "add y w"
    "add y 10"
    "mul y x"
    "add z y"))


(defn compare-path-targets [{path1 :path} {path2 :path}]
  (- (compare path1 path2)))

(defn generate-paths-with-execute [full-program path-target]
  (let [sub-programs (reverse (partition 18 full-program))
        initial-state {"w" 0 "x" 0 "y" 0 "z" 0}
        {target-num :target path :path :as path-target} path-target
        path-length (count path)
        program (nth sub-programs path-length)
        new-digits (filter (fn [[z d]] (let [result (execute program (list d) (assoc initial-state "z" z))]
                                         (= target-num (mod (get result "z") 26))))
                           (for [z (range 26)
                                 d (range 1 10)]
                             (list z d)))]
    (map (fn [[z d]] {:target z :path (conj path d)}) new-digits)))

(defn generate-paths-with-math [full-program path-target]
  (let [{target-num :target path :path :as path-target} path-target
        path-length (count path)]
    (map (fn [[z d]] {:target z :path (conj path d)}) (determine-prev-zs target-num (- 13 path-length)))))

(defn reverse-monad [full-program digits digit-generator-fn]
  (loop [paths (sorted-set-by compare-path-targets {:target 0 :path []}) iterations 0]
    (let [{target-num :target path :path :as path-target} (first paths)
          path-length (count path)]
      ;; (println "path-target" path-target)
      (cond
        (empty? paths) (format "no paths after %d iterations." iterations)
        (= digits path-length) path
        :else
        (recur (apply conj (disj paths path-target) (digit-generator-fn full-program path-target))
               (inc iterations))))))

;; (time (reverse-monad (parse-input large-input) 8))
;; Gets stuck after 6 digits.

(defn compute-ranges [f max-digits]
  (let [program (parse-input f)
        sub-programs (partition 18 program)
        initial-state {"w" 0 "x" 0 "y" 0 "z" 0}]
    (loop [i 0 zs '(0) ranges []]
      (if (= i max-digits)
        ranges
        (let [new-zs (for [z zs
                           d (range 1 10)]
                       ;; (get (execute (nth sub-programs i) (list d) (assoc initial-state "z" z)) "z")
                       (do-math d z i)
                       )]
          (recur (inc i) new-zs (conj ranges
                                      (list (apply min new-zs) (apply max new-zs) (count new-zs)
                                            ;; (apply < new-zs) (some #{0} new-zs)
                                            ))))))))
