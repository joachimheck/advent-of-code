(ns advent-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; How many positions does the tail of a rope visit when following the head?
(def dir-vectors {"L" [-1 0] "R" [1 0] "U" [0 1] "D" [0 -1]})

(defn vec+ [[a b] [c d]]
  (vector (+ a c) (+ b d)))

(defn process-command [command]
  (let [[direction n] (str/split command #" ")]
    (repeat (Long/parseLong n) (get dir-vectors direction))))

(defn read-move-vectors [f]
  (mapcat process-command (read-lines f)))

(defn get-head-path [move-vectors]
  (let [path [[0 0]]]
    (reduce (fn [path move-vector] (conj path (vec+ (last path) move-vector))) [[0 0]] move-vectors)))

(defn touching? [[a b] [c d]]
  (and (<= (abs (- a c)) 1)
       (<= (abs (- b d)) 1)))

(defn move-tail [[head-x head-y :as head-pos] [tail-x tail-y :as tail-pos]]
  (if (touching? head-pos tail-pos)
    tail-pos
    (cond
      (= head-x tail-x) (if (> head-y tail-y)
                          (vec+ tail-pos [0 1])
                          (vec+ tail-pos [0 -1]))
      (= head-y tail-y) (if (> head-x tail-x)
                          (vec+ tail-pos [1 0])
                          (vec+ tail-pos [-1 0]))
      (and (> head-x tail-x) (> head-y tail-y)) (vec+ tail-pos [1 1])
      (and (> head-x tail-x) (< head-y tail-y)) (vec+ tail-pos [1 -1])
      (and (< head-x tail-x) (> head-y tail-y)) (vec+ tail-pos [-1 1])
      (and (< head-x tail-x) (< head-y tail-y)) (vec+ tail-pos [-1 -1]))))

(defn get-tail-path [head-path]
  (reduce
   (fn [tail-path head-pos] (conj tail-path (move-tail head-pos (last tail-path))))
   [[0 0]]
   head-path))


;; (count (distinct (get-tail-path (get-head-path (read-move-vectors small-input)))))
;; 13
;; (count (distinct (get-tail-path (get-head-path (read-move-vectors large-input)))))
;; 6642


;; Part 2
;; How many positions are visited by the tail of a ten knot rope?
(defn compute-rope-path [f]
  (let [head-path (get-head-path (read-move-vectors f))]
    (loop [i 9
           paths (vector head-path)]
      (if (= i 0)
        paths
        (recur (- i 1) (conj paths (get-tail-path (last paths))))))))

;; (count (distinct (last (compute-rope-path large-input))))
;; 2765
;; (count (distinct (last (compute-rope-path small-input))))
;; 1
