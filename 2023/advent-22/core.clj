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
       (sort-by (fn [[p1 p2]] (:z p1)))))

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
  (loop [floating bricks
         resting '()
         max-z 0]
    (if (empty? floating)
      resting
      (let [current (first floating)
            d (- (apply min (map :z current)) max-z)]
        (cond
          (> d 1)
          (recur (doall (vec (conj (remove #{current} floating) (move-down current (dec d))))) resting max-z)
          (can-fall? current resting)
          (recur (doall (vec (conj (remove #{current} floating) (move-down current)))) resting max-z)
          :else
          (recur (vec (remove #{current} floating)) (doall (conj resting current)) (max max-z (apply max (map :z current)))))))))

(defn can-disintegrate? [brick bricks]
  (let [others (remove #{brick} bricks)]
    (every? false? (map #(can-fall? % others) others))))

(defn count-disintegratable-bricks [input]
  (let [settled (settle-bricks (parse-input input))]
    (count
     (filter true?
             (for [brick settled]
               (can-disintegrate? brick settled))))))

;; advent-22.core> (time (count-disintegratable-bricks large-input))
;; "Elapsed time: 1930960.1694 msecs"

;; ---> answer <---
;; 549

;; TODO: only look at the bricks directly above the potentially disintegrated brick.
