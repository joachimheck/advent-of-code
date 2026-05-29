(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
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

(defn parse-line [line]
  (let [substrings (str/split line #":? ")]
    {:name (first substrings)
     :outputs (rest substrings)}))


(defn parse-input [input]
  (reduce (fn [acc line]
                 (let [parsed (parse-line line)
                       name (:name parsed)
                       outputs (:outputs parsed)]
                   (assoc acc name (concat (or (get acc name) []) outputs))))
               {}
               (read-lines input)
               ))

;; Part 1
;; How many paths lead from "you" to "out"?

(defn find-paths-to-out [input]
  (let [output-map (parse-input input)]
    (loop [open-paths [["you"]]
           finished []
           n 0]
      (cond (empty? open-paths)
            {:result finished}
            (> n 10)
            {:error :error-over-n
             :open-paths open-paths
             :finished finished}
            :else
            (let [extended (apply concat (for [p open-paths]
                                           (let [end (last p)
                                                 nexts (get output-map end)]
                                             (for [n nexts]
                                               (conj p n)))))
                  new-extended (remove #(= (last %) "out") extended)
                  new-finished (concat finished (filter #(= (last %) "out") extended))]
              (recur new-extended new-finished (inc n)))))))

(defn count-paths-to-out [input]
  (count (:result (find-paths-to-out input))))


;; (time (count-paths-to-out small-input))
;; "Elapsed time: 2.9757 msecs"
;; 5
;; (time (count-paths-to-out large-input))
;; "Elapsed time: 50.8217 msecs"
;; 701
