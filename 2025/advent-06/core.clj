(ns advent-06.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

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
  (->> input
       (read-lines)
       (map #(str/split % #" +"))
       (map #(remove #{""} %))
       ))
    

;; Part 1
;; What is the sum of the solutions to the math problems?
(defn call [f args]
  (apply (resolve (symbol f)) args))

(defn sum-solutions [input]
  (->> input
       (parse-input)
       (apply mapv vector)
       (map #(list (last %) (butlast %)))
       (map (fn [[op args]] (call op (map parse-long args))))
       (apply +)))

;; (time (sum-solutions small-input))
;; "Elapsed time: 0.8993 msecs"
;; 4277556
;; (time (sum-solutions large-input))
;; "Elapsed time: 7.877 msecs"
;; 4412382293768


;; Part 2
;; The numbers are actually read in columns.
(defn parse-input-2 [input]
  (let [lines (read-lines input)
        len (count (first lines))
        last-len (count (last lines))
        new-last (concat (last lines)
                         (repeat (- len last-len) \space))
        lines (concat (butlast lines) (list new-last))]
    (->> lines
         (mapv vec)
         (apply mapv vector)
         (partition-by #(every? #{\space} %))
         (remove #(every? #{\space} (first %))))))

(defn get-op [[l]]
  (cond (some #{\+} l) +
        (some #{\*} l) *
        :else nil))

(defn get-numbers [l]
  (->> l
       (map #(str/join (remove #{\+\*\space} %)))
       (map parse-long)))

(defn sum-solutions-2 [input]
  (let [parsed (parse-input-2 input)
        pairs (map #(list (get-op %) (get-numbers %)) parsed)
        results (map #(apply (first %) (second %)) pairs)]
    (apply + results)))

;; (time (sum-solutions-2 small-input))
;; "Elapsed time: 1.0942 msecs"
;; 3263827
;; (time (sum-solutions-2 large-input))
;; "Elapsed time: 22.6154 msecs"
;; 7858808482092
