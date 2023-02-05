(ns advent-25.core)

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

;; Day 25: Sea Cucumber

;; Part 1
;; What is the first step on which no sea cucumbers move?
(defn parse-input [f]
  (let [lines (read-lines f)
        width (count (first lines))
        height (count lines)]
    {:width width
     :height height
     :positions (as-> lines ls
                  (map vec ls)
                  (map #(keep-indexed list %) ls)
                  (keep-indexed list ls)
                  (map (fn [[y line]] (map (fn [[x c]] (list x y c)) line)) ls)
                  (apply concat ls)
                  (reduce (fn [result [x y c]]
                            (if (= c \.)
                              result
                              (assoc result [x y] c))) {} ls))}))

(defn draw-board [board]
   (let [positions (:positions board)]
     (str/join
      "\n"
      (for [j (range (:height board))]
         (str/join
          (for [i (range (:width board))]
            (let [position (get positions [i j])]
              (if position position "."))))))))

(defn get-destination [[x y] cucumber board]
  (let [width (:width board)
        height (:height board)
        positions (:positions board)
        [raw-x raw-y] (cond (= cucumber \^) [x (dec y)]
                            (= cucumber \>) [(inc x) y]
                            (= cucumber \v) [x (inc y)]
                            (= cucumber \<) [(dec x) y])
        destination [(cond (= raw-x -1) (dec width)
                           (= raw-x width) 0
                           :else raw-x)
                     (cond (= raw-y -1) (dec height)
                           (= raw-y height) 0
                           :else raw-y)]]
    (if (nil? (get positions destination)) destination [x y])))

(defn move-positions [board]
  (let [positions (:positions board)
        {east-movers \> south-movers \v} (group-by (fn [[k v]] v) (:positions board))
        south-only (into {} (remove (set east-movers) positions))
        east-moved (into south-only (map (fn [[[x y] c]] [(get-destination [x y] c board) c])) east-movers)
        east-moved-board (assoc board :positions east-moved)
        east-only (into {} (remove (set south-movers) east-moved))
        south-moved (into east-only (map (fn [[[x y] c]] [(get-destination [x y] c east-moved-board) c])) south-movers)]
    (assoc board :positions south-moved)))

(defn steps-until-stopped [f]
  (->> f
      (parse-input)
      (iterate move-positions)
      (partition 2 1)
      (map-indexed (fn [n [b1 b2]] (if (= b1 b2) (inc n))))
      (filter identity)
      (first)))


;; (time (steps-until-stopped small-input))
;; "Elapsed time: 15.7422 msecs"
;; 58

;; (time (steps-until-stopped large-input))
;; "Elapsed time: 26151.3569 msecs"
;; 523
