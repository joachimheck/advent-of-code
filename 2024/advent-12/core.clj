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

