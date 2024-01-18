(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many bricks could be safely disintegrated?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(re-matches #"(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)" %))
       (map rest)
       (map #(map parse-long %))
       (map (fn [[a b c d e f]] [{:x a :y b :z c} {:x d :y e :z f}]))
       (map (fn [v] (vec (sort-by :z v))))
       (sort-by #(apply min (map :z %))(fn [[p1 p2]] (:z p1)))))

(defn find-axis [[p1 p2]]
  (cond (not= (:x p1) (:x p2)) :x
        (not= (:y p1) (:y p2)) :y
        :else :z))

(defn get-extents [[p1 p2]]
  {:x [(min (:x p1) (:x p2)) (max (:x p1) (:x p2))]
   :y [(min (:y p1) (:y p2)) (max (:y p1) (:y p2))]
   :z [(min (:z p1) (:z p2)) (max (:z p1) (:z p2))]})

(defn overlap? [brick1 brick2]
  (let [axis1 (find-axis brick1)
        axis2 (find-axis brick2)]
    ;; (if (some nil? [(get-in (get-extents brick2) [axis2 0]) (get-in brick1 [0 axis2]) (get-in (get-extents brick2) [axis2 1])
    ;;                 (get-in (get-extents brick1) [axis1 0]) (get-in brick2 [0 axis1]) (get-in (get-extents brick1) [axis1 1])])
    ;;   (println "nil value for bricks" brick1 brick2))
    ;; (println "axis1" axis1 "axis2" axis2)
    ;; (println "extents1" (get-extents brick1) "extents2" (get-extents brick2))
    ;; (let [axis3 (first (remove #{axis1 axis2} [:x :y :z]))]
    ;;   (println "axis3" (= (get-in brick1 [0 axis3]) (get-in brick2 [0 axis3]))))
    ;; (println "(<=" (get-in (get-extents brick2) [axis2 0]) (get (first brick1) axis2) (get-in (get-extents brick2) [axis2 1]) ")"
    ;;          (<= (get-in (get-extents brick2) [axis2 0]) (get (first brick1) axis2) (get-in (get-extents brick2) [axis2 1])))
    ;; (println "(<=" (get-in (get-extents brick1) [axis1 0]) (get (first brick2) axis1) (get-in (get-extents brick1) [axis1 1]) ")"
    ;;          (<= (get-in (get-extents brick1) [axis1 0]) (get (first brick2) axis1) (get-in (get-extents brick1) [axis1 1])))
    (if (= axis1 axis2)
      (let [other-axes (remove #{axis1} [:x :y :z])]
        (every? #{true} (map #(= (get (first brick1) %) (get (first brick2) %)) other-axes)))
      (let [axis3 (first (remove #{axis1 axis2} [:x :y :z]))]
        (and (= (get-in brick1 [0 axis3]) (get-in brick2 [0 axis3]))
             (<= (get-in (get-extents brick2) [axis2 0]) (get-in brick1 [0 axis2]) (get-in (get-extents brick2) [axis2 1]))
             (<= (get-in (get-extents brick1) [axis1 0]) (get-in brick2 [0 axis1]) (get-in (get-extents brick1) [axis1 1])))))))

(defn move-down
  ([[p1 p2 :as brick]] (move-down brick 1))
  ([[p1 p2] amount]
   [(update p1 :z #(- % amount)) (update p2 :z #(- % amount))]))

(defn can-fall? [brick bricks]
  (not (or (some #{1} (map :z brick))
           (some true? (map #(overlap? (move-down brick) %) (remove #{brick} bricks))))))

(defn settle-bricks [bricks]
  (loop [floating (vec (sort-by #(apply min (map :z %)) bricks))
         resting '()
         max-z 0]
    (if (empty? floating)
      resting
      (let [current (first floating)
            d (- (apply min (map :z current)) max-z)]
        (cond
          (> d 1)
          (recur (doall (assoc floating 0 (move-down current (dec d)))) resting max-z)
          (can-fall? current resting)
          (recur (doall (assoc floating 0 (move-down current))) resting max-z)
          :else
          (recur (vec (rest floating)) (doall (conj resting current)) (max max-z (apply max (map :z current)))))))))

(defn can-disintegrate? [brick bricks]
  (let [others (remove #{brick} bricks)]
    (every? false? (map #(can-fall? % others) others))))

(defn count-disintegratable-bricks [input]
  (let [settled (settle-bricks (parse-input input))]
    (count
     (filter true?
             (for [brick settled]
               (can-disintegrate? brick settled))))))

;; (time (count-disintegratable-bricks large-input))
;; "Elapsed time: 1930960.1694 msecs"

;; 493
;; ---> answer <---
;; 522
;; 549

;; TODO: only look at the bricks directly above the potentially disintegrated brick.
(defn get-cubes [[{x1 :x y1 :y z1 :z :as p1} {x2 :x y2 :y z2 :z :as p2} :as brick]]
  (let [next-cube (cond (not= x1 x2) #(update % :x inc)
                        (not= y1 y2) #(update % :y inc)
                        (not= z1 z2) #(update % :z inc))
        start-cube (first (sort-by #(+ (:x %) (:y %) (:z %)) brick))
        length (inc (apply + (map #(int (Math/abs (- (% p1) (% p2)))) [:x :y :z])))]
    (take length (iterate next-cube start-cube))))

(defn is-supported? [brick bricks]
  (or (some #{1} (map :z brick))
      (let [cubes (get-cubes (move-down brick))
            min-z (apply min (map :z brick))
            cubes-below (map #(update % :z dec) (filter #(= min-z (:z %)) cubes))
            all-cubes (mapcat get-cubes bricks)]
        (set/subset? (set cubes-below) (set all-cubes)))))

(defn settle-bricks-2 [bricks]
  (let [x-max (apply max (map #(apply max (map :x %)) bricks))
        y-max (apply max (map #(apply max (map :y %)) bricks))]
    (loop [floating bricks
           bottom-surface (for [x (range (inc x-max)) y (range (inc y-max))] {:x x :y y :z 0})
           bottom-bricks (into {} (for [cube bottom-surface] [cube :floor]))
           settled '()]
      (if (empty? floating)
        settled
        (let [brick (first floating)
              min-z (apply min (map :z brick))
              max-z (apply max (map :z brick))
              footprint (filter #(= min-z (:z %)) (get-cubes brick))
              matching-bottom (filter #(some (fn [{:keys [x y]}] (and (= x (:x %)) (= y (:y %)))) footprint) bottom-surface)
              new-brick (move-down brick (dec (- min-z (apply max (map :z matching-bottom)))))
              new-bottom (map (fn [b-c] (if (some #{b-c} matching-bottom) (assoc b-c :z max-z) b-c)) bottom-surface)]
          (recur (remove #{brick} floating)
                 new-bottom
                 bottom-bricks
                 (conj settled new-brick)))))))

