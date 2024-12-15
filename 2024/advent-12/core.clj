(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn print-disk [disk]
  (str/join
   (map #(if (nil? %) "." (format "%d" %)) disk)))

(defn parse-input [input]
  (let [array (->> input
                   (read-lines)
                   (mapv vec))
        height (count array)
        width (count (first array))]
    (into {:width width :height height}
          (for [i (range width)
                j (range height)]
            [[i j] (get-in array [j i])]))))

;; Part 1
;; What is the total price of fencing all regions on your map?
(defn neighbors [[x y :as pos]]
  [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn find-region [grid pos]
  (let [plant (get grid pos)]
    (loop [current [pos]
           region #{}]
      (Thread/sleep 1)
      (let [new (->> current
                     (map neighbors)
                     (apply concat)
                     (distinct)
                     (filter #(and (= (get grid pos) (get grid %))
                                   (not-any? region [%]))))]
        (if (empty? new)
          (apply conj region current)
          (recur new (apply conj region current)))))))

(defn group-regions [grid]
  (loop [tiles (keys (dissoc grid :width :height))
         regions []]
    (if (empty? tiles)
      regions
      (let [region (find-region grid (first tiles))
            ;; _ (doall (println "Found region of size" (count region) "at" (first tiles)))
            ]
        (recur (remove region tiles)
               (conj regions region))))))

(defn area [region]
  (count region))

(defn perimeter [grid region]
  (count
   (apply concat
          (let [plant (get grid (first region))]
            (for [pos region]
              (->> pos
                   (neighbors)
                   (filter #(not= plant (get grid %)))))))))

(defn fencing-total-price [input]
  (let [grid (parse-input input)
        regions (group-regions grid)]
    (apply +
           (for [region regions]
             (let [a (area region)
                   p (perimeter grid region)]
               ;; (println "A region of" (str (get grid (first region))) "plants with price" a " * " p " = " (* a p) ".")
               (* a p))))))



;; (time (fencing-total-price small-input))
;; "Elapsed time: 104.0629 msecs"
;; 1930
;; (time (fencing-total-price large-input))
;; "Elapsed time: 10261.1795 msecs"
;; 1424006

(defn sides [grid region]
  (let [plant (get grid (first region))
        ;; _ (println "Plant:" plant)
        side-map (into {}
                       (vec
                        (for [[x y :as pos] region
                              :let [neighbors (list [(inc x) y :east] [x (inc y) :south] [(dec x) y :west] [x (dec y) :north])]]
                          (vector pos (mapcat #(drop 2 %) (filter #(not= plant (get grid (take 2 %))) neighbors))))))
        side-elements (for [k (keys side-map)
                            v (get side-map k)]
                        (conj k v))]
    (loop [elements side-elements
           sides []]
      (Thread/sleep 1)
      ;; (println "elements" elements)
      (if (empty? elements)
        sides
        (let [new-side (loop [current (into #{} (list (first elements)))
                              side #{}]
                         (Thread/sleep 1)
                         (if (empty? current)
                           side
                           (let [ ;; _ (println "current:" current)
                                 ;; _ (println "last:" (last (first current)))
                                 dir (last (first current))
                                 new (filter #(some #{dir} (get side-map (take 2 %)))
                                             (if (or (= dir :north) (= dir :south))
                                               (mapcat (fn [[x y d]] (list [(dec x) y d] [(inc x) y d])) current)
                                               (mapcat (fn [[x y d]] (list [x (dec y) d] [x (inc y) d])) current)))
                                 ;; _ (println "new" new)
                                 new-side (apply conj side current)]
                             (recur (remove new-side new) new-side))))]
          (recur (remove (set new-side) elements)
                 (conj sides new-side)))))))

(defn fencing-total-price-sides [input]
  (let [grid (parse-input input)
        regions (group-regions grid)]
    (apply +
           (for [region regions]
             (let [a (area region)
                   s (count (sides grid region))]
               ;; (println "A region of" (str (get grid (first region))) "plants with price" a " * " s " = " (* a s) ".")
               (* a s))))))

;; (time (fencing-total-price-sides small-input))
;; "Elapsed time: 817.2016 msecs"
;; 1206
;; (time (fencing-total-price-sides large-input))
;; "Elapsed time: 82418.9818 msecs"
;; 858684
