(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 20: Particle Swarm

;; Part 1
;; Which particle will stay closest to the origin in the long term?
(defrecord Particle [px py pz vx vy vz ax ay az])

(defn p-position [p]
  [(:px p) (:py p) (:pz p)])

(defn p-velocity [p]
  [(:vx p) (:vy p) (:vz p)])

(defn p-acceleration [p]
  [(:ax p) (:ay p) (:az p)])

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"p=<([-\d]+),([-\d]+),([-\d]+)>, v=<([-\d]+),([-\d]+),([-\d]+)>, a=<([-\d]+),([-\d]+),([-\d]+)>" %))
       (map rest)
       (map #(map parse-long %))
       (map #(apply ->Particle %))
       (map-indexed (fn [i p] {i p}))
       (apply merge)))

(defn manhattan [x y z]
  (+ (abs x) (abs y) (abs z)))

(defn manhattan-particle [p]
  {:p (apply manhattan (p-position p)) :v (apply manhattan (p-velocity p)) :a (apply manhattan (p-acceleration p))})

(defn vec+ [x y z]
  (+ x y z))

(defn vec+-particle [p]
  {:p (apply vec+ (p-position p)) :v (apply vec+ (p-velocity p)) :a (apply vec+ (p-acceleration p))})


(defn find-eventual-slowest [particles]
 (let [min-a (apply min (map #(apply manhattan (p-acceleration %)) (vals particles)))
       with-min-a (filter #(= min-a (apply manhattan (p-acceleration (get particles %)))) (keys particles))
       v-divided-by-a (apply merge (map (fn [p-idx]
                                          (let [p (get particles p-idx)]
                                            {p-idx (abs (/ (:v (vec+-particle p)) (:a (vec+-particle p))))})) with-min-a))]
   (first (reduce (fn [[a-i a-vda :as acc] [i vda]] (if (> vda a-vda) [i vda] acc)) v-divided-by-a))))

;; (time (find-eventual-slowest (parse-input small-input)))
;; "Elapsed time: 1.0656 msecs"
;; 0

;; (time (find-eventual-slowest (parse-input large-input)))
;; "Elapsed time: 18.4305 msecs"
;; 308
