(ns advent-17.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (for [[y line] (map-indexed list (doall (line-seq rdr)))
          [x c] (map-indexed list line)]
      (list (vector x y 1) (= \# c)))))


(def cubes (reduce #(assoc %1 (* %2 %2 %2) %2) {} (range 1 21)))

(defn vector-array
  ([size] (vec (repeat (* size size size) false)))
  ([size in]
   (reduce (fn [arr p] (apply set-array arr p))
           (vector-array size) in)))

(defn array-size [arr]
  (get cubes (count arr)))

(defn get-array [arr [x y z]]
  (let [size (array-size arr)]
    (if (seq (filter #(or (< % 0) (>= % size)) [x y z])) nil
        (get arr (+ (* size size z) (* size y) x)))))

(defn set-array [arr [x y z] v]
  (let [size (array-size arr)]
    (assoc arr (+ (* size size z) (* size y) x) v)))
           
(defn print-array [arr]
  (let [size (array-size arr)]
    (doseq [array
            (for [z (range size)]
              (concat (list (str "z=" z))
                      (for [y (range size)]
                        (apply str
                               (for [x (range size)]
                                 (if (get-array arr [x y z]) \# \.))))
                      (list "")))
            line array]
      (println line))))

(defn get-all-set [arr]
  (let [size (array-size arr)]
    (for [x (range size)
          y (range size)
          z (range size)
          :when (= true (get-array arr [x y z]))]
      (vector x y z))))

(defn get-neighbors [arr [x y z]]
  (remove #(= [x y z] %)
          (let [size (array-size arr)]
            (for [i (range (- x 1) (+ x 2))
                  j (range (- y 1) (+ y 2))
                  k (range (- z 1) (+ z 2))]
              (vector i j k )))))

(defn count-neighbors [arr [x y z] v]
  (count
   (filter #(= v %)
           (map #(apply get-array arr (vector %)) (get-neighbors arr [x y z])))))

(defn get-new-state [arr [x y z]]
  (if (get-array arr [x y z])
    (if (<= 2 (count-neighbors arr [x y z] true) 3) true false)
    (if (= 3 (count-neighbors arr [x y z] true)) true false)))

(defn evolve-array [arr-init]
  (let [arr (expand-array arr-init)
        size (array-size arr)]
    (vector-array size
                  (for [x (range size)
                        y (range size)
                        z (range size)]
;;(do
;;                    (println x y z "active?" (get-array arr [x y z])
;;                             (count-neighbors arr [x y z] true) "active neighbors" (get-new-state arr [x y z]))
                    (list (vector x y z) (get-new-state arr [x y z]))
;;)
))))

(defn list-array [arr]
  (let [size (array-size arr)]
    (for [x (range size)
          y (range size)
          z (range size)]
      (list (vector x y z) (get-array arr [x y z])))))

(defn expand-array [arr]
  (let [size (array-size arr)]
    (reduce (fn [acc p]
              (let [[x y z] (first p)
                    v (second p)]
                (set-array acc [(inc x) (inc y) (inc z)] v)))
            (vector-array (+ size 2)) (list-array arr))))

;; (count (filter #(second %) (list-array (nth (iterate evolve-array (vector-array 8 (read-input large-input))) 6))))
;; 240

