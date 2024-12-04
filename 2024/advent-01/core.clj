(ns advent-01.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

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

;; Part 1
;; What is the product of the sizes of the two groups of components
;; that can be created by separating three wires.
(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #" +"))
       (map #(mapv parse-long %))))

(defn sum-distances [input]
  (let [nums (parse-input input)
        firsts (sort (map first nums))
        seconds (sort (map second nums))]
    (->> (map list firsts seconds)
         (map #(apply - %))
         (map abs)
         (apply +))))


;; advent-01.core> (time (sum-distances small-input))
;; "Elapsed time: 0.5847 msecs"
;; 11

;; advent-01.core> (time (sum-distances large-input))
;; "Elapsed time: 11.3532 msecs"
;; 2066446



(defn sum-similarities [input]
  (let [nums (parse-input input)
        firsts (map first nums)
        seconds (map second nums)
        freqs (frequencies seconds)]
    (apply +
           (for [n firsts]
             (* n (get freqs n 0))))))


;; advent-01.core> (time (sum-similarities small-input))
;; "Elapsed time: 0.6684 msecs"
;; 31
;; advent-01.core> (time (sum-similarities large-input))
;; "Elapsed time: 4.5762 msecs"
;; 24931009
