(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 13: Mine Cart Madness

;; Part 1
;; What is the location of the first crash?
(defn find-neighbors [[x y] points]
  (case (get points [x y])
    \- (list [(dec x) y] [(inc x) y])
    \| (list [x (dec y)] [x (inc y)])
    \/ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])
    \\ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])
    \+ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])))

(defn find-next-point [[x y :as p] [prev-x prev-y :as prev] points]
  (let [neighbors (find-neighbors p points)
        c (get points p)]
    (if (nil? prev)
      (first neighbors)
      (cond
        (= c \+) [(+ x (- x prev-x)) (+ y (- y prev-y))] ; The opposite side
        (= c \/) (case [(- x prev-x) (- y prev-y)]
                   [1 0] [x (dec y)]
                   [-1 0] [x (inc y)]
                   [0 1] [(dec x) y]
                   [0 -1] [(inc x) y])
        (= c \\) (case [(- x prev-x) (- y prev-y)]
                   [1 0] [x (inc y)]
                   [-1 0] [x (dec y)]
                   [0 1] [(inc x) y]
                   [0 -1] [(dec x) y])
        :else (first (remove #(= prev %) neighbors))))))

(defn find-path [points]
  (loop [path [(first (first (filter (fn [[p c]] (= c \-)) points)))]
         points points]
    (let [[x y :as p] (last path)
          c (get points p)
          [next-x next-y :as next-p] (find-next-point p (if (> (count path) 1) (last (drop-last path))) points)
          next-point [next-p (get points next-p)]
          new-points (let [prev-p (if (> (count path) 2) (last (drop-last (drop-last path))))]
                       (if (and (= \+ c) (not (empty? (remove #{prev-p next-p} (find-neighbors p points)))))
                         (assoc points [x y] (if (= x next-x) \- \|))
                         (dissoc points [x y])))]
      (if (some #{next-p} path)
        (list path new-points)
        (recur (conj path next-p) new-points)))))

(defn find-paths [points]
  (loop [points points paths '()]
    (if (empty? points)
      paths
      (let [[path new-points] (find-path points)]
        (recur new-points (conj paths path))))))

(defn parse-input [f]
  (let [vectorized (->> f
                        (read-lines)
                        (map #(str/replace % #" +$" ""))
                        (mapv vec))
        mapped (into {}
                     (apply concat
                            (for [j (range (count vectorized))
                                  :let [line (get vectorized j)]
                                  i (range (count line))
                                  :let [c (get line i)]
                                  :when (not= \space c)]
                              {[i j] c})))
        carts (filter (fn [[k v]] (or (= v \>) (= v \<) (= v \^) (= v \v))) mapped)
        without-carts (reduce (fn [acc [k v]]
                                (case v
                                  \> (assoc acc k \-)
                                  \< (assoc acc k \-)
                                  \^ (assoc acc k \|)
                                  \v (assoc acc k \|)
                                  acc))
                              mapped
                              mapped)
        paths (find-paths without-carts)]
    {:points without-carts
     :paths paths
     :carts (map (fn [[p c]] [p c :left]) carts)}))

;; TODO: remember to turn the carts at each intersection.
(defn move-carts [])
