(ns advent-21.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall (map
            (fn [line]
              (let [[_ ingredients allergens] (re-matches #"(.+)\(contains (.+)\)" line)]
                (list (str/split ingredients #" ") (str/split allergens #", "))))
            (line-seq rdr)))))

(read-input small-input)

;; Part 1
;; Which ingredients contain no allergens? Count their occurrances in the input.

;; ((["mxmxvkd" "kfcds" "sqjhc" "nhms"] ["dairy" "fish"])
;;  (["trh" "fvjkl" "sbzzf" "mxmxvkd"] ["dairy"])
;;  (["sqjhc" "fvjkl"] ["soy"])
;;  (["sqjhc" "mxmxvkd" "sbzzf"] ["fish"]))

;; mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
;; trh fvjkl sbzzf mxmxvkd (contains dairy)
;; sqjhc fvjkl (contains soy)
;; sqjhc mxmxvkd sbzzf (contains fish)

;; one of (mxmxvkd kfcds sqjhc nhms) contains dairy
;; one of (trh fvjkl sbzzf mxmxvkd) contains dairy
;; the intersection is (mxmxvkd), so that contains dairy!

;; one of (sqjhc mxmxvkd sbzzf) contains fish
;; one of (mxmxvkd kfcds sqjhc nhms) contains fish
;; the intersection is (mxmxvkd sqjhc), so one of those contains fish

;; That means sqjhc must contain fish!

;; one of (sqjhc fvjkl) contains soy
;; we already know that sqjhc contains fish,
;; so fvjkl must contain soy!

(defn foods-by-allergen [foods]
  (reduce
   (fn [m [allergen food]]
     (assoc m allergen (conj (get m allergen #{}) food)))
   {}
   (apply concat
          (for [food foods]
            (map
             (fn [allergen]
               (list allergen (first food)))
             (second food))))))

(defn intersection [coll]
  (if (= 1 (count coll)) (into '() (first coll))
      (reduce
       (fn [l1 l2]
         (keep (set l1) l2))
       coll)))

(defn intersect-ingredients
  "Finds sets of ingredients that may contain the allergens."
  [foods-by-allergen]
  (map
   (fn [k] (list k (intersection (get foods-by-allergen k))))
   (keys foods-by-allergen)))

(defn singles-map
  "Returns a map of elements with only one ingredient."
  [intersected]
  (->> intersected
       (filter (fn [[a foods]] (= 1 (count foods))))
       (reduce (fn [acc [allergen [ingredient]]] (assoc acc allergen ingredient)) {})
       ))  

(defn remove-singles [singles-map intersected]
  (remove (fn [[allergen _]] (get singles-map allergen)) intersected))

(defn remove-single-ingredients
  "Removes ingredients already associated with an allergen."
  [singles-map remaining]
  (let [to-remove (set (vals singles-map))]
    (map (fn [[allergen ingredients]]
           (list allergen (remove to-remove ingredients)))
         remaining)))

(defn reduce-ingredients
  "Reduces the number of ingredients containing allergens."
  [singles intersected-ingredients]
  (println singles intersected-ingredients)
  (if (empty? intersected-ingredients) singles
    (let [singles (merge singles (singles-map intersected-ingredients))]
      (reduce-ingredients
       singles
       (remove-single-ingredients
        singles
        (remove-singles singles intersected-ingredients))))))
  
(defn ingredients [input]
  (->> input
       (map first)
       flatten))


;; (let [input (read-input small-input)
;;       allerfoods (vals (reduce-ingredients {} (intersect-ingredients (foods-by-allergen input))))
;;       ingredients (ingredients input)
;;       safe (remove (set allerfoods) ingredients)]
;;   (count safe))
;; => 5

;; (let [input (read-input large-input)
;;       allerfoods (vals (reduce-ingredients {} (intersect-ingredients (foods-by-allergen input))))
;;       ingredients (ingredients input)
;;       safe (remove (set allerfoods) ingredients)]
;;   (count safe))
;; => 2282




;; Part 2
;; Seems like I already did the work for this - maybe I did something extra in part 1?

;; (let [input (read-input small-input)
;;       pairs (reduce-ingredients {} (intersect-ingredients (foods-by-allergen input)))]
;;   (str/join "," (vals (sort pairs))))
;; => "mxmxvkd,sqjhc,fvjkl"

;; (let [input (read-input large-input)
;;       pairs (reduce-ingredients {} (intersect-ingredients (foods-by-allergen input)))]
;;   (str/join "," (vals (sort pairs))))
;; => "vrzkz,zjsh,hphcb,mbdksj,vzzxl,ctmzsr,rkzqs,zmhnj"

