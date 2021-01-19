(ns day-15.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Maximize cookie quality.

(defn parse-line [line]
 (let [[name properties] (str/split line #":")]
   (assoc {} name
          (reduce (fn [acc [k v]] (assoc acc k (Long/parseLong v)))
                  {}
                  (map rest (re-seq #"(\w+) ([-\d]+)" properties))))))

(def test-lines
  '("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
    "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"))

(def real-lines
  '("Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5"
    "Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8"
    "Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6"
    "Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"))

(def test-ingredients (reduce merge {} (map parse-line test-lines)))

(def real-ingredients (reduce merge {} (map parse-line real-lines)))

(defn score-recipe [ingredients amounts]
  ;; (println ingredients amounts)
  (let []
    (->>
     (for [[ingredient properties] ingredients
           [prop-name prop-val] (remove #(= "calories" (first %)) properties)]
       (do
         (if (nil? (get amounts ingredient))
           (println "no amount for ingredient" ingredient "in" amounts))
         (list prop-name (* (get amounts ingredient) prop-val))))
     (group-by first)
     (map (fn [[property vals]]
            (list property (max 0 (reduce +(map second vals))))))
     (map second)
     (reduce *)
     )))

(defn amounts-sum-to? [amounts n]
  (= n (apply + (vals amounts))))

(defn has-calories? [ingredients amounts n]
  (= n
     (reduce +
             (map
              (fn [[ingredient-name amount]]
                (* amount (get (get ingredients ingredient-name) "calories")))
              amounts))))

(defn combo-amounts-inner [tsps ingredients]
  (if (empty? ingredients) '(())
      (for [i (range (max (inc tsps) 0))
            more (combo-amounts-inner (- tsps i) (rest ingredients))]
        (merge (assoc {} (first (first ingredients)) i) more))))

(defn combo-amounts [tsps calories ingredients]
  (filter
   #(and (amounts-sum-to? % tsps)
         (or (nil? calories)
             (has-calories? ingredients % calories)))
   (combo-amounts-inner tsps ingredients)))

(time (count (combo-amounts 100 500 real-ingredients)))


(defn find-best-recipe [ingredients calories]
 (->> ingredients
      (combo-amounts 100 calories)
      (map (partial score-recipe ingredients))
      (remove #(= 0 %))
      sort
      reverse
      (take 3)
      ))

;; (time (find-best-recipe real-ingredients nil))
;; => "Elapsed time: 22069.7049 msecs"
;; (18965440 18957312 18939200)



;; Part 2
;; 500 calorie recipes only.

;; (time (find-best-recipe real-ingredients 500))
;; => (15862900 15713280 15628800)
;; "Elapsed time: 15797.4983 msecs"

