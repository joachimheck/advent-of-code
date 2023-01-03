(ns day-13.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Build a maze and solve it.

(def test-input 10)
(def real-input 1364)

(defn count-bits [x]
  (if (= x 0)
    0
    (+ (bit-and x 1) (count-bits (bit-shift-right x 1)))))

(defn cell-val [x y fav]
  (let [sum (+ fav (* x x) (* 3 x) (* 2 x y) y (* y y))]
    (if (even? (count-bits sum)) "." "#")))

(defn build-maze [fav max-x max-y]
  (into {} (for [j (range (inc max-y))
                 i (range (inc max-x))]
             [[i j] (cell-val i j fav)])))

(defn header [max-x]
  (str/join
   (list
    "   "
    (let [spaces "         "]
      (str/join
       (for [i (range (/ max-x 10))]
         (str/join (list i spaces)))))
   "\n"
   "   "
   (str/join (take (inc max-x) (cycle "0123456789"))))))

;; (defn print-map [m]
;;   (let [keys (keys m)
;;         max-x (apply max (map first keys))
;;         max-y (apply max (map second keys))]
;;     (println (header max-x))
;;     (for [j (range (inc max-y))]
;;       (printf "%2d %s\n"
;;               j
;;               (str/join (for [i (range (inc max-x))] (get m [i j])))))))
(defn print-maze
  ([maze] (print-maze maze '()))
  ([maze solution]
   (let [keys (keys maze)
         max-x (apply max (map first keys))
         max-y (apply max (map second keys))
         solution-set (set solution)]
     (println (header max-x))
     (for [j (range (inc max-y))]
       (printf "%2d %s\n"
               j
               (str/join (for [i (range (inc max-x))]
                           (if (solution-set [i j])
                             "O"
                             (get maze [i j])))))))))

(defn neighbors [[x y]]
  (filter (fn [[x y]] (and (>= x 0) (>= y 0)))
          (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)])))

(defn neighboring-spaces [maze [x y :as pos]]
  (let [neighbor-spots (neighbors pos)]
    (filter #(= "." (get maze %)) neighbor-spots)))

(apply conj #{1} '(2 3))

(defn solve [maze start target]
  (loop [visited #{}
         states (list (assoc {} :path (list start) :nexts (neighboring-spaces maze start)))]
    (let [state (first states)
          path-end (last (:path state))
          all-nexts (mapcat :nexts states)
          visited (apply conj (conj visited path-end) all-nexts)]
      (if (= target path-end)
        (:path state)
        (recur
         visited
         (->> states
              (remove #{state})
              (concat (map
                       (fn [pos]
                         (assoc {}
                                :path (concat (:path state) (list pos))
                                :nexts (remove visited (neighboring-spaces maze pos))))
                       (:nexts state)))))))))

(defn solution-length [solution] (dec (count solution)))

;; (solution-length (solve (build-maze 10 9 6) [1 1] [7 4]))
;; => 11

;; (solution-length (solve (build-maze 1364 32 40) [1 1] [31 39]))
;; => 100
;; too high

;; (let [maze (build-maze 1364 32 40)
;;       solution (solve maze [1 1] [31 39])]
;;   (print-maze maze solution))

;; (solution-length (solve (build-maze 1364 32 40) [1 1] [31 39]))
;; => 86




;; Part 2
;; How many locations can be reached in 50 steps?
(defn flood [maze start max-steps]
  (loop [visited #{start}
         spaces (list start)
         steps max-steps]
    (let [new-spaces (remove visited (distinct (mapcat (partial neighboring-spaces maze) spaces)))    
          new-visited (apply conj visited new-spaces)]
      (if (= 0 steps)
        visited
        (recur new-visited new-spaces (dec steps))))))

;; (let [maze (build-maze 1364 32 40)]
;;   (print-maze maze (flood maze [1 1] 50)))

;; (let [maze (build-maze 1364 32 40)]
;;   (count (flood maze [1 1] 50)))
;; => 127
