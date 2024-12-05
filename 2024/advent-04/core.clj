(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")
(def small-input-2 "small-input-2.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (vec (read-lines input)))


;; Part 1
;; How many times does the word 'XMAS' appear?
(defn horizontal [[x y]]
  (for [i (range 4)]
    [(+ x i) y]))

(defn vertical [[x y]]
  (for [i (range 4)]
    [x (+ y i)]))
 
(defn diag-right [[x y]]
  (for [i (range 4)]
    [(+ x i) (+ y i)]))

(defn diag-left [[x y]]
  (for [i (range 4)]
    [(- x i) (+ y i)]))


(defn count-terms [input term]
  (let [grid (parse-input input)
        width (count (first grid))
        height (count grid)
        term (vec term)
        r-term (vec (reverse term))]
    (count
     (filter #(or (= term %) (= r-term %))
             (apply concat
                    (for [i (range width)
                          j (range height)]
                      (list
                       (map #(get-in grid %) (horizontal [i j]))
                       (map #(get-in grid %) (vertical [i j]))
                       (map #(get-in grid %) (diag-right [i j]))
                       (map #(get-in grid %) (diag-left [i j])))))))))

;; (time (count-terms small-input "XMAS"))
;; "Elapsed time: 5.2237 msecs"
;; 18
;; (time (count-terms large-input "XMAS"))
;; "Elapsed time: 262.9865 msecs"
;; 2297


;; Part 2
;; Search for crossed MASes, not XMASes.
(defn x-shape [[x y]]
  [[[x y] [(inc x) (inc y)] [(+ x 2) (+ y 2)]]
   [[(+ x 2) y] [(inc x) (inc y)] [x (+ y 2)]]])

(defn count-terms-2 [input term]
  (let [grid (parse-input input)
        width (count (first grid))
        height (count grid)
        term (vec term)
        r-term (vec (reverse term))]
    (count
     (filter #(and (or (= term (first %)) (= r-term (first %)))
                   (or (= term (second %)) (= r-term (second %))))
             (for [i (range width)
                   j (range height)
                   :let [[stroke-a stroke-b] (x-shape [i j])]]
               (list
                (map #(get-in grid %) stroke-a)
                (map #(get-in grid %) stroke-b)))))))


;; (time (count-terms-2 small-input "MAS"))
;; "Elapsed time: 1.9102 msecs"
;; 9
;; (time (count-terms-2 large-input "MAS"))
;; "Elapsed time: 58.2642 msecs"
;; 1745
