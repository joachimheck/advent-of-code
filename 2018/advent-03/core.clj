(ns advent-03.core)

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

;; Day 3: No Matter How You Slice It

;; Part 1
;; How many square inches of fabric are within two or more claims?
(defn parse-line [line]
  (let [[id x y w h] (map parse-long (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)))]
    {id {"x" x "y" y "w" w "h" h}}))

(defn parse-input [f]
  (->> f
       (read-lines)
       (map parse-line)
       (apply merge)))

(defn squares [{:strs [x y w h] :as rectangle}]
  (set
   (for [i (range x (+ x w))
         j (range y (+ y h))]
     [i j])))

(defn claim-counts [dimensions]
  (reduce (fn [acc p] (update acc p (fnil inc 0)))
          {}
          (apply concat (map squares dimensions))))

(defn count-overlaps [claim-map]
  (count (filter #(> (second %) 1) claim-map)))

;; (time (count-overlaps (claim-counts (vals (parse-input small-input)))))
;; "Elapsed time: 1.3333 msecs"
;; 4

;; (time (count-overlaps (claim-counts (vals (parse-input large-input)))))
;; "Elapsed time: 2000.171 msecs"
;; 104439



;; Part 2
;; Which claim does not overlap?
(defn claim-ids [claims]
  (reduce (fn [acc [id dimensions]]
            (merge-with concat
                        acc
                        (reduce (fn [acc p] (update acc p #(conj % id)))
                                {}
                                (squares dimensions))))
          {}
          claims))

(defn non-overlapping [claims]
  (let [all-squares (keys claims)
        claims-map (claim-ids claims)
        overlapping (set (apply concat (vals (filter (fn [[k v]] (> (count v) 1)) claims-map))))]
    (remove overlapping all-squares)))

;; (time (non-overlapping (parse-input small-input)))
;; "Elapsed time: 1.0751 msecs"
;; (3)

;; (time (non-overlapping (parse-input large-input)))
;; "Elapsed time: 2241.8556 msecs"
;; (701)
