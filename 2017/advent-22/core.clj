(ns advent-22.core)

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

;; Day 22: Sporifica Virus

;; Part 1
;; How many virus activity bursts cause a node to become infected?
(defn parse-input [f]
  (let [vectorized (mapv vec (read-lines f))
        size (count vectorized)
        half (/ (dec size) 2)
        start [half half]]
    {:position start
     :direction :north
     :grid (apply merge
                  (for [j (range (count vectorized))
                        i (range (count vectorized))
                        :let [value (get-in vectorized [j i])]
                        :when (= \# value)]
                    {[i j] value}))
     :infections 0}))

(def directions [:north :east :south :west])

(defn get-direction [current turn]
  (let [current-index (.indexOf directions current)]
    (case turn
      :right (get directions (mod (inc current-index) 4))
      :left (get directions (mod (dec current-index) 4))
      :reverse (get directions (mod (+ 2 current-index) 4)))))

(defn move [[x y] direction]
  (case direction
    :north [x (dec y)]
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]))

(defn process-bursts [state max-bursts]
  (loop [{:keys [position direction grid infections] :as state} state bursts 0]
    (if (= bursts max-bursts)
      state
      (let [infected? (get grid position)
            new-direction (if infected? (get-direction direction :right) (get-direction direction :left))
            new-grid (update grid position #(if % nil \#))
            new-position (move position new-direction)
            new-infections (if infected? infections (inc infections))]
        (recur {:position new-position
                :direction new-direction
                :grid new-grid
                :infections new-infections}
               (inc bursts))))))

(defn draw-grid [{:keys [position direction grid]}]
  (let [[minx maxx miny maxy] (reduce (fn [[minx maxx miny maxy] [x y]]
                                        (let [new-minx (if (< x minx) x minx)
                                              new-maxx (if (> x maxx) x maxx)
                                              new-miny (if (< y miny) y miny)
                                              new-maxy (if (> y maxy) y maxy)]
                                          [new-minx new-maxx new-miny new-maxy]))
                                      [Integer/MAX_VALUE Integer/MIN_VALUE Integer/MAX_VALUE Integer/MIN_VALUE]
                                      (keys grid))]
    (str/join "\n"
     (for [j (range miny (inc maxy))]
       (str/join
        (for [i (range minx (inc maxx))]
          (let [value (get grid [i j])]
           (if value (str/join (list (str value) " ")) ". "))))))))

;; (time (get (process-bursts (parse-input small-input) 10000) :infections))
;; "Elapsed time: 126.2366 msecs"
;; 5587

;; (time (get (process-bursts (parse-input large-input) 10000) :infections))
;; "Elapsed time: 105.3268 msecs"
;; 5411



;; Part 2
;; The virus has evolved!
(defn compute-status [status]
  (case status
    nil \W
    \W \#
    \# \F
    \F nil))

(defn direction-from-status [status direction]
  (case status
    nil (get-direction direction :left)
    \W direction
    \# (get-direction direction :right)
    \F (get-direction direction :reverse)))

(defn process-bursts-evolved [state max-bursts]
  (loop [{:keys [position direction grid infections] :as state} state bursts 0]
    (if (= bursts max-bursts)
      state
      (let [status (get grid position)
            new-direction (direction-from-status status direction)
            new-grid (update grid position #(compute-status %))
            new-position (move position new-direction)
            new-infections (if (= \W (get grid position)) (inc infections) infections)]
        (recur {:position new-position
                :direction new-direction
                :grid new-grid
                :infections new-infections}
               (inc bursts))))))


;; (time (get (process-bursts-evolved (parse-input small-input) 10000000) :infections))
;; "Elapsed time: 96210.8227 msecs"
;; 2511944

;; (time (get (process-bursts-evolved (parse-input large-input) 10000000) :infections))
;; "Elapsed time: 95747.4643 msecs"
;; 2511416
