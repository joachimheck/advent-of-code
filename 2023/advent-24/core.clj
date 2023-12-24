(ns advent-24.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Ignoring the Z axis, how many hailstones' paths intersect in the test area?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(re-matches #"([\-\d]+), +([\-\d]+), +([\-\d]+) +@ +([\-\d]+), +([\-\d]+), +([\-\d]+)" %))
       (map rest)
       (map #(mapv parse-long %))
       (map (fn [[x y z vx vy vz]] {:x x :y y :z z :vx vx :vy vy :vz vz}))))

(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
          (pairs (rest coll)))))

(defn find-intersection [stone1 stone2]
  (let [slope1 (/ (:vy stone1) (:vx stone1))
        slope2 (/ (:vy stone2) (:vx stone2))]
    (if (not= slope1 slope2)
      (let [x (/ (+ (- (:y stone2) (:y stone1)) (- (* (:x stone1) slope1) (* (:x stone2) slope2))) (- slope1 slope2))
            y (+ (:y stone1) (* (- x (:x stone1)) slope1))]
        [x y]))))

(defn find-time-for-point [stone [x y]]
  (cond (and (= (:x stone) x) (= (:y stone) y)) :present
        (= (neg? (- x (:x stone))) (neg? (:vx stone))) :future
        :else :past))

(defn stone-to-string [{:keys [x y z vx vy vz]}]
  (format "%d, %d, %d @ %d, %d, %d" x y z vx vy vz))

(defn find-intersections-print [stones min-pos max-pos]
  (let [intersections (map (fn [[a b]] (list a b (find-intersection a b))) (pairs stones))]
    (doseq [[a b intersection] intersections]
      (println "Hailstone A: " (stone-to-string a))
      (println "Hailstone B: " (stone-to-string b))
      (cond (nil? intersection)
            (println "Hailstones' paths are parallel; they never intersect.")
            (and (= :past (find-time-for-point a intersection))
                 (= :past (find-time-for-point b intersection)))
            (println "Hailstones' paths crossed in the past for both hailstones.")
            (= :past (find-time-for-point a intersection))
            (println "Hailstones' paths crossed in the past for hailstone A.")
            (= :past (find-time-for-point b intersection))
            (println "Hailstones' paths crossed in the past for hailstone B.")
            :else
            (let [[x y] intersection]
              (if (and (<= min-pos x max-pos) (<= min-pos y max-pos))
                (printf "Hailstones' paths will cross INSIDE the test area (at x=%.3f, y=%.3f).\n" (double x) (double y))
                (printf "Hailstones' paths will cross outside the test area (at x=%.3f, y=%.3f).\n" (double x) (double y))))))))

(defn find-intersections [stones min-pos max-pos]
  (for [[a b [x y :as intersection]] (map (fn [[a b]] (list a b (find-intersection a b))) (pairs stones))
        :when (and (some? intersection)
                   (= :future (find-time-for-point a intersection))
                   (= :future (find-time-for-point b intersection))
                   (<= min-pos x max-pos)
                   (<= min-pos y max-pos))]
    [x y]))

(defn count-intersections [input min-pos max-pos]
  (count (find-intersections (parse-input input) min-pos max-pos)))

;; (time (count-intersections small-input 7 27))
;; "Elapsed time: 1.2314 msecs"
;; 2

;; (time (count-intersections large-input 200000000000000 400000000000000))
;; "Elapsed time: 758.8846 msecs"
;; 11098
