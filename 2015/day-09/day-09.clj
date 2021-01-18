(ns day-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-input
  (list
   "London to Dublin = 464"
   "London to Belfast = 518"
   "Dublin to Belfast = 141"))

(defn parse-line [line]
  (let [[_ start end dist] (re-matches #"(\w+) to (\w+) = (\d+)" line)]
    [start end (Long/parseLong dist)]))

(defn read-input [f]
  (map parse-line (str/split-lines (slurp f))))

;; Part 1
;; Find the shortest path.
(defn distance-map [input]
  (reduce
   (fn [acc [start end dist]]
     (assoc acc [start end] dist [end start] dist))
   {}
   input))

(defn possible-routes [cities]
  (if (empty? cities) '(())
      (for [city cities
            more (myf (remove #(= % city) cities))]
        (cons city more))))

(defn route-distance [route distances]
  (let [partitioned (partition 2 1 route)]
    (reduce +
            (map (fn [pair] (get distances pair))
                 partitioned))))

(defn routes [input]
  (let [cities (distinct (mapcat #(list (first %) (second %)) input))
        routes (possible-routes cities)
        distances (distance-map input)]
    (map (fn [route] (list route (route-distance route distances)))
         routes)))

(defn shortest-route [input]
  (first (sort-by second (routes input))))

(defn longest-route [input]
  (last (sort-by second (routes input))))


;; (time (shortest-route (map parse-line test-input)))
;; => (("London" "Dublin" "Belfast") 605)
;; "Elapsed time: 1.6785 msecs"

;; (time (shortest-route (read-input "puzzle-input.txt")))
;; => (("Tristram" "AlphaCentauri" "Norrath" "Straylight" "Faerun" "Snowdin" "Tambi" "Arbre") 141)
;; "Elapsed time: 9769.2523 msecs"



;; Part 2
;; Longest distance

;; (time (longest-route (map parse-line test-input)))
;; => (("Belfast" "London" "Dublin") 982)
;; "Elapsed time: 2.3605 msecs"

;; (time (longest-route (read-input "puzzle-input.txt")))
;; => (("Faerun" "Norrath" "Tambi" "Straylight" "Snowdin" "Tristram" "Arbre" "AlphaCentauri") 736)
;; "Elapsed time: 6306.8446 msecs"

