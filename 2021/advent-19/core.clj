(ns advent-19.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 15: Beacon Scanner

;; Part 1
;; Find beacons detected by multiple scanners to assemble a full map of beacons. How many beacons are there?
(defn parse-input [f]
  (->> f
       read-lines
       (partition-by #{""})
       (remove #{'("")})
       (map rest)
       (map (fn [coll] (map #(str/join ["[" % "]"]) coll)))
       (mapv (fn [coll] (set (map #(load-string %) coll))))))

(def mini-input [#{[0 2 0] [4 1 0] [3 3 0]} #{[-1 -1 0] [-5 0 0] [-2 1 0]}])

(defn vec+ [v1 v2]
  (mapv (fn [a b] (+ a b)) v1 v2))

(defn vec- [v1 v2]
  (mapv (fn [a b] (- a b)) v1 v2))

(defn shift-beacons [beacons shift]
  (into #{} (map #(vec+ % shift) beacons)))

(defn find-matching-shift [beacons-1 beacons-2 match-count]
  (first
   (filter #(>= (:count %) match-count)
           (mapcat (fn [beacon-2]
                     (map (fn [beacon-1]
                            (let [diff (vec- beacon-1 beacon-2)
                                  shifted (shift-beacons beacons-2 diff)]
                              {:count (count (set/intersection beacons-1 shifted))
                               :scanner-location diff
                               :shifted shifted}
                              ))
                          beacons-1))
                   beacons-2))))

(defn rotate-around-x [[x y z]]
  (list [x y z] [x (- z) y] [x (- y) (- z)] [x z (- y)]))

(defn rotate-around-y-and-z [[x y z]]
  (list [x y z] [(- z) y x] [(- x) y (- z)] [z y (- x)] [(- y) x z] [y (- x) z]))

(defn rotate-point [pos]
  (mapcat rotate-around-x (rotate-around-y-and-z pos)))

(defn match-beacons [beacons-1 beacons-2 match-count]
  (let [match-count 3
        beacons-2-rotated (apply map list (map rotate-point beacons-2))]
    (first (filter some?
                   (map (fn [beacons-2]
                          (find-matching-shift beacons-1 beacons-2 match-count))
                        beacons-2-rotated)))))

(defn locate-all-beacons [beacon-sets match-count]
  (loop [beacons (into #{} (first beacon-sets))
         open-set (rest beacon-sets)
         scanners [[0 0 0]]]
    (println "beacon sets remaining" (count open-set))
    (if (empty? open-set)
      {:beacons beacons
       :scanners scanners}
      (let [matched (match-beacons beacons (first open-set) match-count)]
        (if matched
          (recur (apply conj beacons (:shifted matched)) (rest open-set) (conj scanners (:scanner-location matched)))
          (recur beacons (conj (vec (rest open-set)) (first open-set)) scanners))))))

;; (time (count (:beacons (locate-all-beacons (parse-input small-input) 12))))
;; "Elapsed time: 4936.1624 msecs"
;; 79

;; (time (count (:beacons (locate-all-beacons (parse-input large-input) 12))))
;; "Elapsed time: 150685.5706 msecs"
;; 326



;; Part 2
;; What is the greatest manhattan distance between scanners?
(defn permutations [s]
  (if (seq (rest s))
    (concat (map #(list (first s) %) (rest s))
            (permutations (rest s)))))

(defn manhattan-distance [v1 v2]
  (apply + (map #(abs (- %1 %2)) v1 v2)))


;; (apply max (map (fn [[a b]] (manhattan-distance a b))
;;                                 (permutations (:scanners (locate-all-beacons (parse-input small-input) 12)))))
;; 3621

;; (apply max (map (fn [[a b]] (manhattan-distance a b))
;;                                 (permutations (:scanners (locate-all-beacons (parse-input large-input) 12)))))
;; 10630
