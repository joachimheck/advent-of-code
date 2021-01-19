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
              (fn [ingredient-name]
                (get (get ingredients ingredient-name) "calories"))
              (keys amounts)))))

(defn combo-amounts-inner [ingredients remaining-amount]
  (if (empty? ingredients) '(())
      (for [i (range (max (inc remaining-amount) 0))
            more (combo-amounts-inner (rest ingredients) (- remaining-amount i))]
        (merge (assoc {} (first (first ingredients)) i) more))))

(defn combo-amounts [calories ingredients]
  (let [n 100]
    (filter
     #(and (amounts-sum-to? % n)
           (or (nil? calories)
               (has-calories? ingredients % calories)))
     (combo-amounts-inner ingredients n))))

(defn find-best-recipe [ingredients max calories]
 (->> ingredients
      (combo-amounts calories)
      (map (partial score-recipe ingredients))
      (remove #(= 0 %))
      (take max)
      sort
      reverse
      (take 3)
      ))

;; (time (find-best-recipe real-ingredients 1000000 nil))
;; => "Elapsed time: 22069.7049 msecs"
;; (18965440 18957312 18939200)



;; Part 2
;; 500 calorie recipes only.

;; (time (find-best-recipe real-ingredients 100 1000000))
