(ns advent-19.core)

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

;; Day 19: A Series of Tubes

;; Part 1
;; What letters will the packet see as it follows the route on the map?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map-indexed #(list %1 (vec %2)))
       (map (fn [[j row]] (list j (map-indexed (fn [i v] (list i j v)) row))))
       (map second)
       (apply concat)
       (remove (fn [[_ _ v]] (= \space v)))
       (reduce (fn [acc [i j v]] (assoc acc [i j] v)) {})))

(defn start-point [diagram]
  (first (first (filter (fn [[[i j] v]] (= j 0)) diagram))))

(defn raw-next [[i j] direction]
  (case direction
    :right [(inc i) j]
    :down [i (inc j)]
    :left [(dec i) j]
    :up [i (dec j)]))

(def perpendicular {:right '(:up :down)
                    :down '(:left :right)
                    :left '(:up :down)
                    :up '(:left :right)})

(defn next-point [[i j :as p] direction diagram]
  (let [current-symbol (get diagram p)]
    (if (= current-symbol \+)
      (first (filter #(get diagram (first %)) (map #(list (raw-next p %) %) (perpendicular direction))))
      (list (raw-next p direction) direction))))

(defn letter? [c]
  (re-matches #"[A-Z]" (str c)))

(defn follow-path [diagram]
  (let [start (start-point diagram)
        start-direction :down]
    (loop [position start direction start-direction letters []]
      (let [symbol (get diagram position)]
        (if symbol
          (let [[new-position new-direction] (next-point position direction diagram)]
            (recur new-position new-direction (if (letter? symbol) (conj letters symbol) letters)))
          (str/join letters))))))

;; (time (follow-path (parse-input small-input)))
;; "Elapsed time: 1.2828 msecs"
;; "ABCDEF"

;; (time (follow-path (parse-input large-input)))
;; "Elapsed time: 123.0448 msecs"
;; "ITSZCJNMUO"



;; Part 2
;; How many steps does the packet take?
(defn follow-path-with-steps [diagram]
  (let [start (start-point diagram)
        start-direction :down]
    (loop [position start direction start-direction letters [] steps 0]
      (let [symbol (get diagram position)]
        (if symbol
          (let [[new-position new-direction] (next-point position direction diagram)]
            (recur new-position new-direction (if (letter? symbol) (conj letters symbol) letters) (inc steps)))
          (list (str/join letters) steps))))))

;; (time (follow-path-with-steps (parse-input small-input)))
;; "Elapsed time: 0.8495 msecs"
;; ("ABCDEF" 38)

;; (time (follow-path-with-steps (parse-input large-input)))
;; "Elapsed time: 113.0736 msecs"
;; ("ITSZCJNMUO" 17420)
