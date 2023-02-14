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



;; Part 2
;; How many particles will never collide?

;; at t=0, p=4 v=0 a=-2
;; at t=1, p=2 v=-2 a=-2
;; at t=2, p=-2 v=-4 a=-2
;; at t=3, p=-8 v=-6 a=-2

(defn particle-at-time [particle t]
  {:ax (:ax particle)
   :ay (:ay particle)
   :az (:az particle)
   :vx (+ (:vx particle) (* t (:ax particle)))
   :vy (+ (:vy particle) (* t (:ay particle)))
   :vz (+ (:vz particle) (* t (:az particle)))
   :px (+ (:px particle) (+ (* t (:vx particle)) (* (:ax particle) (apply + (range (inc t))))))
   :py (+ (:py particle) (+ (* t (:vy particle)) (* (:ay particle) (apply + (range (inc t))))))
   :pz (+ (:pz particle) (+ (* t (:vz particle)) (* (:az particle) (apply + (range (inc t))))))})

(defn format-particle [p]
  (format "p=<%d,%d,%d> v=<%d,%d,%d> a=<%d,%d,%d>"
          (:px p) (:py p) (:pz p)
          (:vx p) (:vy p) (:vz p)
          (:ax p) (:ay p) (:az p)))

(defn a-matches-v? [p]
  (and
   (or (= 0 (:ax p)) (= (neg? (:ax p)) (neg? (:vx p))))
   (or (= 0 (:ay p)) (= (neg? (:ay p)) (neg? (:vy p))))
   (or (= 0 (:az p)) (= (neg? (:az p)) (neg? (:vz p))))))

(defn with-same-position [particles]
  (let [grouped (vals (group-by (fn [i] (let [p (get particles i)]
                                          [(:px p) (:py p) (:pz p)]))
                                (keys particles)))]
    (set (apply concat (filter #(> (count %) 1) grouped)))))

(defn resolve-collisions [particles max-t]
  (loop [particles particles t 0]
    (let [at-time (apply merge (map (fn [[i p]] {i (particle-at-time p t)}) particles))
          same-position (with-same-position at-time)]
      (if (>= t max-t)
        (count at-time)
        (recur (apply dissoc particles same-position) (inc t))))))

(def small-input-2 "small-input-2.txt")

;; I was going to write some code to figure out whether the particles would collide in the future,
;; but I guess I got the right answer just by trying a fixed max-t, so... ¯\_(ツ)_/¯

;; (time (resolve-collisions (parse-input small-input-2) 500))
;; "Elapsed time: 23.6245 msecs"
;; 1

;; (time (resolve-collisions (parse-input large-input) 500))
;; "Elapsed time: 9908.0758 msecs"
;; 504
