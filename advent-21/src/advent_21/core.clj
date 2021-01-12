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

(let [foods (read-input small-input)
      foods-by-allergen (foods-by-allergen foods)]
  (map
   (fn [k] (list k (intersection (get foods-by-allergen k))))
   (keys foods-by-allergen))
)

;; TODO: reduce that list by removing allergens that match only one
;; ingredient - along with that ingredient from the remaining
;; allergen/ingredient lists.
