(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; How many trees are visible from outside the grid?
(defn line-to-numbers [line]
  (mapv #(Long/parseLong (str (first %))) (partition 1 line)))

(defn read-trees [f]
  (mapv line-to-numbers (read-lines f)))

(defn get-tree [trees x y]
  (get-in trees [y x]))

(defn sightlines [x y max-x max-y]
  (vector
   (map #(vector x %) (range (- y 1) -1 -1))
   (map #(vector % y) (range (+ x 1) max-x))
   (map #(vector x %) (range (+ y 1) max-y))
   (map #(vector % y) (range (- x 1) -1 -1))))

(defn heightlines [trees x y max-x max-y]
  (let [lines (sightlines x y max-x max-y)]
    (mapv (fn [sightline] (mapv (fn [[i j]] (get-tree trees i j)) sightline)) lines)))

(defn is-visible? [trees x y max-x max-y]
  (let [tree-height (get-tree trees x y)
        height-lines (heightlines trees x y max-x max-y)]
    (< (apply min (map #(apply max (conj % -1)) height-lines)) tree-height)))

(defn count-visible [f]
  (let [trees (read-trees f)
        max-x (count (get trees 0))
        max-y (count trees)]
    (count (filter true?
                   (for [x (range 0 max-x) y (range 0 max-y)]
                     (is-visible? trees x y max-x max-y))))))

(defn display-visible [f]
  (let [lines (read-lines f)
        trees (read-trees f)
        max-y (count trees)
        max-x (count (get trees 0))
        visibilities (for [y (range 0 max-y)]
                       (for [x (range 0 max-x)]
                         (if (is-visible? trees x y) 1 0)))]
    (let [viz-lines (mapv (fn [line] (str/join (map str line))) visibilities)]
      (doseq [i (range 0 (count lines))]
        (println (get lines i) (get viz-lines i))))))

;; (count-visible small-input)
;; 21
;; (count-visible large-input)
;; 1703


;; Part 2
;; What is the highest scenic score for any tree?

;; From internet - supposed to be in core?
(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(defn view-distances [heightlines tree-height]
  (for [height-line heightlines]
    (count (take-until #(>= % tree-height) height-line))))

(defn compute-scenic-score [trees x y max-x max-y]
  (apply * (view-distances (heightlines trees x y max-x max-y) (get-tree trees x y))))

(defn find-max-scenic-score [f]
  (let [trees (read-trees f)
        max-x (count (first trees))
        max-y (count trees)]
    (apply max (for [x (range 0 max-x) y (range 0 max-y)]
                 (compute-scenic-score trees x y max-x max-y)))))

;; (find-max-scenic-score small-input)
;; 8
;; (find-max-scenic-score large-input)
;; 496650
