(ns advent-21.core)

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

;; Day 21: Fractal Art

;; Part 1
;; How many pixels are lit after five iterations?
(def initial-pattern [(vec ".#.")
                      (vec "..#")
                      (vec "###")])

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"(.+) => (.+)" %))
       (map rest)
       (map vec)
       (into {})))

(def test-pattern [(vec "abcd")
                   (vec "efgh")
                   (vec "ijkl")
                   (vec "mnop")])

(defn split-pattern [pattern]
  (let [pattern-size (count (first pattern))
        new-size (if (= 0 (mod pattern-size 2)) 2 3)
        rows-split (mapv #(partition new-size %) pattern)
        steps (range (quot pattern-size new-size))]
    (vec
     (apply concat
            (for [j steps]
              (for [i steps]
                (vec
                 (for [k (range new-size)]
                   (vec (nth (nth rows-split (+ (* j new-size) k)) i))))))))))

(defn merge-patterns [patterns]
  (if (= 1 (count patterns))
    (first patterns)
    (let [rows (int (Math/sqrt (count patterns)))
          columns rows
          sub-rows (count (first patterns))]
      (vec
       (apply concat
              (for [j (range rows)]
                (for [k (range sub-rows)]
                  (vec
                   (apply concat
                    (for [i (range columns)]
                      (get-in patterns [(+ (* j columns) i) k])))))))))))

(defn flip-pattern [pattern]
  (map vec (list pattern
                 (reverse pattern)
                 (map vec (map reverse pattern))
                 (map vec (map reverse (reverse pattern))))))

(defn rotate-pattern [pattern]
  (let [pattern-size (count (first pattern))]
    (if (= pattern-size 2)
      (list pattern
            [[(get-in pattern [1 0]) (get-in pattern [0 0])]
             [(get-in pattern [1 1]) (get-in pattern [0 1])]]
            [[(get-in pattern [1 1]) (get-in pattern [1 0])]
             [(get-in pattern [0 1]) (get-in pattern [0 0])]]
            [[(get-in pattern [0 1]) (get-in pattern [1 1])]
             [(get-in pattern [0 0]) (get-in pattern [1 0])]])
      (list pattern
            [[(get-in pattern [2 0]) (get-in pattern [1 0]) (get-in pattern [0 0])]
             [(get-in pattern [2 1]) (get-in pattern [1 1]) (get-in pattern [0 1])]
             [(get-in pattern [2 2]) (get-in pattern [1 2]) (get-in pattern [0 2])]]
            [[(get-in pattern [2 2]) (get-in pattern [2 1]) (get-in pattern [2 0])]
             [(get-in pattern [1 2]) (get-in pattern [1 1]) (get-in pattern [1 0])]
             [(get-in pattern [0 2]) (get-in pattern [0 1]) (get-in pattern [0 0])]]
            [[(get-in pattern [0 2]) (get-in pattern [1 2]) (get-in pattern [2 2])]
             [(get-in pattern [0 1]) (get-in pattern [1 1]) (get-in pattern [2 1])]
             [(get-in pattern [0 0]) (get-in pattern [1 0]) (get-in pattern [2 0])]]))))

(defn rotate-and-flip [pattern]
  (distinct (apply concat (map rotate-pattern (flip-pattern pattern)))))

(defn to-rule [pattern]
  (str/join "/" (map str/join pattern)))

(defn to-pattern [rule]
  (mapv vec (str/split rule #"/")))

(defn match [pattern rules]
  (let [patterns (rotate-and-flip pattern)
        match (first (filter #(contains? rules (to-rule %)) patterns))]
    (to-pattern (get rules (to-rule match)))))

(defn enhance [pattern rules]
  (let [patterns (split-pattern pattern)
        matches (mapv #(match % rules) patterns)
        merged (merge-patterns matches)]
    merged))

(defn count-pixels [pattern]
  (list (get (frequencies (flatten pattern)) \#)
        (format "%dx%d" (count pattern) (count pattern))))

(defn multi-enhance [pattern rules iterations]
  (nth (iterate #(enhance % rules) pattern) iterations))

(defn draw-pattern [pattern]
  (str/join "\n" (map str/join pattern)))

;; (time (count-pixels (multi-enhance initial-pattern (parse-input large-input) 5)))
;; "Elapsed time: 11.4253 msecs"
;; 147



;; Part 2
;; How many pixels are on after 18 iterations?



;; (time (count-pixels (multi-enhance initial-pattern (parse-input large-input) 18)))
;; "Elapsed time: 71718.8116 msecs"
;; (1936582 "2187x2187")
