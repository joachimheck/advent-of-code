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
     (if (empty? program)
       variables
       (let [{op :op arg1 :arg1 arg2 :arg2} (first program)]
         ;; (if (= op "inp") (println "executing z =" (get variables "z") "w =" (first inputs)))
         (case op
           "inp" (recur (rest program) (rest inputs) (assoc variables arg1 (first inputs)))
           "add" (recur (rest program) inputs (assoc variables arg1 (+ (value variables arg1) (value variables arg2))))
           "mul" (recur (rest program) inputs (assoc variables arg1 (* (value variables arg1) (value variables arg2))))
           "div" (recur (rest program) inputs (assoc variables arg1 (long (/ (value variables arg1) (value variables arg2)))))
           "mod" (recur (rest program) inputs (assoc variables arg1 (mod (value variables arg1) (value variables arg2))))
           "eql" (recur (rest program) inputs (assoc variables arg1 (if (= (value variables arg1) (value variables arg2)) 1 0)))))))))

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
    (loop [i 99999999999999 max-valid 0]
      (if (<= i (- 99999999999999 iterations))
        {:max-valid max-valid}
        (let [digits (digits i)]
          (if (some #{0} digits)
            (recur (dec i) max-valid)
            (let [result (execute program digits)
                  valid (= 0 (get result "z"))]
              (if valid
                (recur (dec i) i)
                (recur (dec i) max-valid)))))))))

(def v1s '(1 1 1 26 1 26 1 26 26 26 26 1 1 26))
(def v2s '(14 11 14 -11 12 -1 10 -3 -4 -13 -8 13 11 -11))
(def v3s '(12 8 4 4 1 10 8 12 10 15 4 10 7 9))

(defn do-math [w z v1 v2 v3]
  (println "do-math" w z)
  (let [x (mod z 26)
        z (long (/ z v1))
        x (if (not= w (+ x v2)) 1 0)
        y (+ 1 (* 25 x))
        z (* z y)
        y (* (+ w v3) x)
        z (+ z y)]
    z))

(defn do-multi [ws]
  (loop [ws ws v1s v1s v2s v2s v3s v3s z 0]
    (if (empty? ws)
      z
      (recur (rest ws) (rest v1s) (rest v2s) (rest v3s) (do-math (first ws) z (first v1s) (first v2s) (first v3s))))))

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


;; I guess I can work backwards from the least significant digit and figure out what digits will give
;; me a right answer at each step. Break the program down into 14 sub-programs.
;; (let [program (parse-lines test-input-end)]
;;                   (filter (fn [[z d]] (let [result (execute program (list d) {"w" 0 "x" 0 "y" 0 "z" z})]
;;                                         (= 0 (get result "z"))))
;;                        (for [z (range 26)
;;                              d (range 1 10)]
;;                          (list z d))))
;; ((12 1) (13 2) (14 3) (15 4) (16 5) (17 6) (18 7) (19 8) (20 9))
;; advent-24.core> (let [program (parse-lines test-input-end2)]
;;                   (filter (fn [[z d]] (let [result (execute program (list d) {"w" 0 "x" 0 "y" 0 "z" z})]
;;                                         (= 20 (get result "z"))))
;;                        (for [z (range 26)
;;                              d (range 1 10)]
;;                          (list z d))))
;; ()
;; advent-24.core> (let [program (parse-lines test-input-end2)]
;;                   (filter (fn [[z d]] (let [result (execute program (list d) {"w" 0 "x" 0 "y" 0 "z" z})]
;;                                         (= 19 (get result "z"))))
;;                        (for [z (range 26)
;;                              d (range 1 10)]
;;                          (list z d))))
;; ((0 9))
;; advent-24.core> 
