(ns day-03.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def puzzle-input (first  (str/split-lines (slurp "puzzle-input.txt"))))

;; Part 1
;; Count houses visited by Santa.

(defn move [[x y] dir]
  (case dir
    \> [(inc x) y]
    \^ [x (inc y)]
    \< [(dec x) y]
    \v [x (dec y)]))

(defn follow-dirs [dirs]
  (reduce
   (fn [acc dir]
     (conj acc (move (last acc) dir)))
   [[0 0]]
   dirs))

;; (time (count (distinct (follow-dirs (seq puzzle-input)))))
;; => 2572
;; "Elapsed time: 1124.0845 msecs"



;; Part 2
;; Robo-Santa and Santa take alternating directions from the input.

(defn follow-dirs-with-robo-santa [dirs]
  (let [{s-dir-pairs true r-dir-pairs false} (group-by first (map-indexed #(list (even? %1) %2) dirs))
        s-dirs (map second s-dir-pairs)
        r-dirs (map second r-dir-pairs)]
    (concat (follow-dirs s-dirs) (follow-dirs r-dirs))))

;; (time (count (distinct (follow-dirs-with-robo-santa (seq puzzle-input)))))
;; => 2631
;; "Elapsed time: 580.3025 msecs"

