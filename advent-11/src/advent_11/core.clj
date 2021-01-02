(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-seats
  "Returns a vector containing the seat map."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce conj [] (line-seq rdr))))

;; Part 1
;; Determine seat occupancy
(defrecord Point [x y])

(defn- get-adjacent-points [^Point p]
  (filter #(and (not= p %)
                (>= (:x %) 0)
                (>= (:y %) 0))
    (for [i '(-1 0 1) j '(-1 0 1)]
      (->Point (+ (:x p) i) (+ (:y p) j)))))

(defn- get-seat [seats ^Point p]
  (get (get seats (:y p)) (:x p)))

(defn- get-adjacent-seats [seats ^Point p]
  (map (partial get-seat seats) (get-adjacent-points p)))

(defn- can-occupy?
  "A seat can be occupied of it is empty and no adjacent seats are occupied."
  [seats ^Point p]
  (let [seat (get-seat seats p)
        adjacent (get-adjacent-seats seats p)]
    (and (= \L seat)
         (not-any? #{\#} adjacent))))

(defn- can-vacate?
  "A seat can be vacated if it and four adjacent seats are occupied."
  [seats ^Point p]
  (let [seat (get-seat seats p)
        adjacent (get-adjacent-seats seats p)]
    (and (= \# seat)
         (<= 4 (count (filter #{\#} adjacent))))))

(defn- process-seats [seats]
  (let [height (count seats)
        width (count (first seats))]
    (vec
      (for [y (range height)]
        (apply str
          (for [x (range width)]
            (let [p (->Point x y)]
              (cond (can-occupy? seats p) \#
                    (can-vacate? seats p) \L
                    :else (get-seat seats p)))))))))

(defn- equal-seat-maps? [m1 m2]
  (= (apply str m1) (apply str m2)))

(defn- count-occupied [seats]
  (count (filter #{\#} (apply str seats))))

(defn process-seats-until-stable [seats]
  (let [processed (process-seats seats)]
    (if (equal-seat-maps? processed seats)
      (count-occupied processed)
      (process-seats-until-stable processed))))

(defn process-seats-with-loop [seats-initial]
  (let [seats-before (atom [])
        seats-after (atom seats-initial)]
  (loop [cnt 0]
    (if (equal-seat-maps? @seats-before @seats-after)
      (count-occupied @seats-after)
      (do (swap! seats-before (fn [a] @seats-after))
        (swap! seats-after process-seats)
        (recur (inc cnt)))))))



;; Part 2
;; Instead of considering adjacent seats, consider seats that can be seen
;; (the direct path is not blocked) from the current seat.
(def directions {:n '(0 -1)
                 :ne '(1 -1)
                 :e '(1 0)
                 :se '(1 1)
                 :s '(0 1)
                 :sw '(-1 1)
                 :w '(-1 0)
                 :nw '(-1 -1)})

(defn- get-first-visible-seat [seats ^Point p dir]
  (let [dir-diff (get directions dir)
        x-diff (first dir-diff)
        y-diff (second dir-diff)
        new-p (->Point (+ (:x p) x-diff) (+ (:y p) y-diff))
        seat (get-seat seats new-p)]
    (if (nil? seat) nil
        (if (= \. seat)
          (get-first-visible-seat seats new-p dir)
          seat))))

(defn- get-visible-seats [seats ^Point p]
  (filter some?
          (map #(get-first-visible-seat seats p %)
               (keys directions))))
  
(defn- can-occupy-2?
  "A seat can be occupied of it is empty and no visible seats are occupied."
  [seats ^Point p]
  (let [seat (get-seat seats p)
        visible (get-visible-seats seats p)]
    (and (= \L seat)
         (not-any? #{\#} visible))))

(defn- can-vacate-2?
  "A seat can be vacated if it and five visible seats are occupied."
  [seats ^Point p]
  (let [seat (get-seat seats p)
        visible (get-visible-seats seats p)]
    (and (= \# seat)
         (<= 5 (count (filter #{\#} visible))))))

(defn- process-seats-2 [seats]
  (let [height (count seats)
        width (count (first seats))]
    (vec
      (for [y (range height)]
        (apply str
          (for [x (range width)]
            (let [p (->Point x y)]
              (cond (can-occupy-2? seats p) \#
                    (can-vacate-2? seats p) \L
                    :else (get-seat seats p)))))))))

(defn process-seats-with-loop-2 [seats-initial]
  (let [seats-before (atom [])
        seats-after (atom seats-initial)]
  (loop [cnt 0]
    (if (equal-seat-maps? @seats-before @seats-after)
      (count-occupied @seats-after)
      (do (swap! seats-before (fn [a] @seats-after))
        (swap! seats-after process-seats-2)
        (recur (inc cnt)))))))
