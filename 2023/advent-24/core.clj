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



;; Part 2
;; Throw a rock that will hit every hailstone.

;; At time t, stone {:x x0 :y y0 :z z0 :vx vx :vy vy :vz vz} will be at position
;; [(+ x0 (* t vx)) (+ y0 (* t vy)) (+ z0 (* t vz))]


(defn find-intersection-3 [stone1 stone2]
  (let [slope1 (/ (:vy stone1) (:vx stone1))
        slope2 (/ (:vy stone2) (:vx stone2))]
    (if (not= slope1 slope2)
      (let [x (/ (+ (- (:y stone2) (:y stone1)) (- (* (:x stone1) slope1) (* (:x stone2) slope2)))
                 (- slope1 slope2))
            y (+ (:y stone1) (* (- x (:x stone1)) slope1))]
        [x y]))))

;;  {:x 19, :y 13, :z 30, :vx -2, :vy 1, :vz -2}
;;  {:x 18, :y 19, :z 22, :vx -1, :vy -1, :vz -2}
;;  {:x 20, :y 25, :z 34, :vx -2, :vy -2, :vz -4}
;;  {:x 12, :y 31, :z 28, :vx -1, :vy -2, :vz -1}
;;  {:x 20, :y 19, :z 15, :vx 1, :vy -5, :vz -3}

;; -2t + t -2t = 62
;; 19 + -2t

;; y = mx + b
;; y = (-1/2)x + 22.5

;; x = 19 + -2t
;; y = 13 + t
;; z = 30 -2t

;; x - 19 = -2t
;; y - 13 = t
;; z - 30 = -2t

;; 19/2 - x/2 = t
;; y - 13 = t
;; 15 - z/2 = t

;; 19/2 - x/2 = y - 13
;; 45/2 - x/2 - y = 0

;; 45/2 - x/2 - y = 15 - z/2
;; 45/2 - 30/2 - x/2 - y + z/2 = 0
;; 15/2 - x/2 - y + z/2 = 0
;; 15 - x - 2y + z = 0
;; - x - 2y + z = - 15


;; {:x 19, :y 13, :vx -2, :vy 1}
;; {:x 18, :y 19, :vx -1, :vy -1}

;; x = (+ 19 (* -2 t)) = (+ 18 (* -1 s))
;; y = (+ 13 (* 1 t)) = (+ 19 (* -1 s))

;; (+ 19 (* -2 t)) = (+ 18 (* -1 s))
;; (* -2 t) = (+ 18 (* -1 s) -19)
;; (* -2 t) = (+ -1 (* -1 s))
;; (- (* 2 t)) = (- (+ s 1))
;; (* 2 t) = (+ s 1)
;; t = (/ (+ s 1) 2)

;; (+ 13 t) = (+ 19 (- s))
;; t = (+ 19 (- s) -13)
;; t = (+ 6 (- s))

;; (/ (+ s 1) 2) = (+ 6 (- s))
;; (+ s 1) = (+ 12 (* -2 s))
;; (* 3 s) = 11
;; s = (/ 11 3)

;; x(s) = (+ 18 - (/ 11 3)) = (/ (- 54 11) 3) = (/ 43 3) =~ 14.333
;; y(s) = (+ 19 (* -1 (/ 11 3))) = (- (/ 57 3) (/ 11 3)) = (/ 46 3) =~ 15.333

;; OK, I was able to manually take the parameterized equations for the two lines,
;; solve for t in terms of s, then set x(s) = x(t) and solve for s, then backfill
;; to get the intersection point. Next I need to write code to do this, and then
;; it needs to be expanded to three (four?) dimensions to solve the problem.


;; (let [parsed (parse-input small-input)
;;                       with-t (map #(assoc % :t 0 :vt 1) parsed)
;;                       inputs (map list [\A \B \C \D \E] [\a \b \c \d \e] with-t)
;;                       format-eq (fn [[line-name param coefficients]]
;;                                   (for [[axis v axis-name] [[:x :vx "x"] [:y :vy "y"] [:z :vz "z"] [:t :vt "t"]]]
;;                                     (format "%s%s(%s) = %d%s + %d" line-name axis-name param (get coefficients v) param (get coefficients axis))))]
;;                   (mapcat format-eq inputs))
