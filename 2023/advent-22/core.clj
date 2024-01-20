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

(defn settle-bricks-1 [bricks]
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

(defn count-disintegratable-bricks-1 [input]
  (let [settled (settle-bricks-1 (parse-input input))]
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
        start-cube (first (sort-by #(+ (:x %) (:y %) (:z %)) (take 2 brick)))
        length (inc (apply + (map #(int (Math/abs (- (% p1) (% p2)))) [:x :y :z])))]
    (take length (iterate next-cube start-cube))))

(defn is-supported? [brick bricks]
  (or (some #{1} (map :z brick))
      (let [cubes (get-cubes (move-down brick))
            min-z (apply min (map :z (take 2 brick)))
            cubes-below (map #(update % :z dec) (filter #(= min-z (:z %)) cubes))
            all-cubes (mapcat get-cubes bricks)]
        (set/subset? (set cubes-below) (set all-cubes)))))

(defn move-to [[p1 p2 label :as brick] z]
  (let [base-brick [(assoc p1 :z z) (assoc p2 :z (+ z (- (:z p2) (:z p1))))]]
    (if label (conj base-brick label) base-brick)))

(defn settle-bricks [bricks]
  (let [x-max (apply max (map #(apply max (map :x (take 2 %))) bricks))
        y-max (apply max (map #(apply max (map :y (take 2 %))) bricks))]
    (loop [floating bricks
           bottom-surface (into {} (for [x (range (inc x-max)) y (range (inc y-max))] [[x y] 0]))
           bottom-bricks (into {} (for [x (range (inc x-max)) y (range (inc y-max))] [[x y] :floor]))
           settled '()
           resting-on {}]
      (if (empty? floating)
        resting-on
        (let [brick (first floating)
              min-z (apply min (map :z (take 2 brick)))
              footprint (map (fn [cube] [(:x cube) (:y cube)]) (filter #(= min-z (:z %)) (get-cubes brick)))
              matching-bottom (select-keys bottom-surface footprint)
              max-bottom-height (apply max (vals matching-bottom))
              new-brick (move-to brick (inc max-bottom-height))
              max-z (apply max (map :z (take 2 new-brick)))
              new-cubes (get-cubes new-brick)
              new-bottom-surface (reduce (fn [acc {:keys [x y z]}] (assoc acc [x y] max-z)) bottom-surface new-cubes)
              new-bottom-bricks (reduce (fn [acc {:keys [x y z]}] (assoc acc [x y] new-brick)) bottom-bricks new-cubes)
              new-resting-on (assoc resting-on new-brick (set (for [p (keys matching-bottom)
                                                                    :when (= max-bottom-height (get matching-bottom p))]
                                                                (get bottom-bricks p))))]
          (recur (remove #{brick} floating)
                 new-bottom-surface
                 new-bottom-bricks
                 (conj settled new-brick)
                 new-resting-on))))))

(defn count-disintegratable-bricks [input]
  (let [bricks (parse-input input)
        resting-on (settle-bricks bricks)
        total (count resting-on)
        single-supports (set (for [[brick supports] resting-on
                                   :when (and (= (count supports) 1) (not= supports #{:floor}))]
                               (first supports)))
        ]
    (- total (count single-supports))))

;; (time (count-disintegratable-bricks small-input))
;; "Elapsed time: 7.9574 msecs"
;; 5

;; (time (count-disintegratable-bricks large-input))
;; "Elapsed time: 442.9719 msecs"
;; 519



;; Part 2
;; What is the sum of the numbers of bricks that would fall if each brick were disintegrated?
(defn label-bricks [bricks]
  (map (fn [brick]
         (case brick
           [{:x 1, :y 0, :z 1} {:x 1, :y 2, :z 1}] [{:x 1, :y 0, :z 1} {:x 1, :y 2, :z 1} \A]
           [{:x 0, :y 0, :z 2} {:x 2, :y 0, :z 2}] [{:x 0, :y 0, :z 2} {:x 2, :y 0, :z 2} \B]
           [{:x 0, :y 2, :z 3} {:x 2, :y 2, :z 3}] [{:x 0, :y 2, :z 3} {:x 2, :y 2, :z 3} \C]
           [{:x 0, :y 0, :z 4} {:x 0, :y 2, :z 4}] [{:x 0, :y 0, :z 4} {:x 0, :y 2, :z 4} \D]
           [{:x 2, :y 0, :z 5} {:x 2, :y 2, :z 5}] [{:x 2, :y 0, :z 5} {:x 2, :y 2, :z 5} \E]
           [{:x 0, :y 1, :z 6} {:x 2, :y 1, :z 6}] [{:x 0, :y 1, :z 6} {:x 2, :y 1, :z 6} \F]
           [{:x 1, :y 1, :z 8} {:x 1, :y 1, :z 9}] [{:x 1, :y 1, :z 8} {:x 1, :y 1, :z 9} \G]))
       bricks))

;; (defn compute-chain-reaction-fall-total [input]
;;   (let [bricks (parse-input input)
;;         bricks (if (= input small-input) (label-bricks bricks) bricks)
;;         resting-on (settle-bricks bricks)
;;         ;;brick (first (filter #(= \A (last %)) (keys resting-on)))
;;         ]
;;     (apply +
;;            (for [brick (keys resting-on)]
;;              (loop [open-set #{brick}
;;                     fallen #{}]
;;                ;; (println "open-set" open-set "fallen" fallen)
;;                ;; do I need to conj fallen into open-set here? -------------------------------v
;;                (if (empty? open-set)
;;                  (dec (count fallen))
;;                  (let [new-unsupported (set (map first (filter (fn [[b s]] (set/subset? s (apply conj fallen open-set))) resting-on)))]
;;                    (recur new-unsupported (apply conj fallen open-set)))))))))

;; 101588
;; ---> ANSWER <---

(defn compute-chain-reaction-fall-total [input]
  (let [bricks (parse-input input)
        bricks (if (= input small-input) (label-bricks bricks) bricks)
        resting-on (settle-bricks bricks)]
    (apply +
           (for [
                 brick (keys resting-on)
                 ;; brick (filter #(= \A (last %)) (keys resting-on))
                 ]
             (loop [open-set #{brick}
                    resting-on resting-on
                    fallen #{}]
               ;; (println "open-set" open-set)
               ;; (println "resting-on" resting-on)
               ;; (println "fallen" fallen)
               (if (empty? open-set)
                 (dec (count fallen))
                 (let [new-resting (into {} (map (fn [[b s]] [b (set (remove open-set s))]) resting-on))
                       new-fallen (apply conj fallen open-set)
                       new-open-set (set (remove new-fallen (map first (filter #(empty? (second %)) new-resting))))
                       ]
                   (recur new-open-set new-resting new-fallen))))))))

;; (time (compute-chain-reaction-fall-total small-input))
;; "Elapsed time: 2.2963 msecs"
;; 7

;; (time (compute-chain-reaction-fall-total large-input))
;; "Elapsed time: 58596.8089 msecs"
;; 109531
