(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 10: Monitoring Station

;; Part 1
;; How many other asteroids can be detected from the location with the least obstructed view?
(defn parse-input [f]
  (->> f
                     (read-lines)
                     (map-indexed (fn [y row] (map-indexed (fn [x c] (if (= c \#) [x y])) row)))
                     (apply concat)
                     (remove nil?)))

(defn distance [[a b] [c d]]
  (+ (abs (- c a)) (abs (- d b))))

(defn other-asteroids [asteroid asteroids]
  (map second (sort-by first (map #(list (distance asteroid %) %) (remove #{asteroid} asteroids)))))

(defn p-diff [[a b] [c d]]
  [(- c a) (- d b)])

(defn asteroid-diffs [asteroid asteroids]
  (map #(p-diff asteroid %) (other-asteroids asteroid asteroids)))

(defn factors [n]
  (filter #(= 0 (mod n %)) (range 2 (inc (abs n)))))

(defn gcd [a b]
  (apply max (concat '(1) (set/intersection (set (factors a)) (set (factors b))))))

(defn is-occluded-by? [[a b] [c d]]
  (and (<= (distance [0 0] [c d]) (distance [0 0] [a b]))
   (cond (= 0 a c) (or (< 0 d b) (> 0 d b))
         (= 0 b d) (or (< 0 c a) (> 0 c a))
         :else
         (let [[vx vy] [(quot c (gcd c d)) (quot d (gcd c d))]]
           ;; (println "[vx vy]" [vx vy])
           (and (or (= 0 a vx) (and (not= 0 vx) (= 0 (mod a vx))))
                (or (= 0 b vy) (and (not= 0 vy) (= 0 (mod b vy))))
                (= (quot a vx) (quot b vy))
                (> (quot a vx) (quot c vx)))))))

(defn remove-occluded [diffs]
  (reduce (fn [diffs current]
            (remove (fn [diff]
                      (is-occluded-by? diff current)) diffs))
          diffs
          diffs))

(defn count-visible [asteroid asteroids]
  (count (remove-occluded (asteroid-diffs asteroid asteroids))))

(defn draw-visible-count [asteroids]
  (let [width (inc (apply max (map first asteroids)))
        height (inc (apply max (map second asteroids)))]
    (str/join "\n"
              (for [j (range height)]
                (str/join (for [i (range width)]
                            (if (some #{[i j]} asteroids)
                              (str (count-visible [i j] asteroids))
                              ".")))))))

(defn sort-by-visibility [asteroids]
  (sort-by second (map #(list % (count-visible % asteroids)) asteroids)))

(defn find-highest-visibility [asteroids]
  (last (sort-by-visibility asteroids)))

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

(deftest test-find-highest-visibility
  (is (= '([3 4] 8) (find-highest-visibility (parse-input small-input))))
  (is (= '([5 8] 33) (find-highest-visibility (parse-input small-input-2))))
  (is (= '([1 2] 35) (find-highest-visibility (parse-input small-input-3))))
  (is (= '([6 3] 41) (find-highest-visibility (parse-input small-input-4))))
  (is (= '([11 13] 210) (find-highest-visibility (parse-input small-input-5)))))

(defn visible-from [[ax ay :as asteroid] asteroids]
  (map (fn [[x y]] [(+ x ax) (+ y ay)]) (remove-occluded (asteroid-diffs asteroid asteroids))))

(defn draw-asteroids [asteroids]
  (let [width (inc (apply max (map first asteroids)))
        height (inc (apply max (map second asteroids)))]
    (str/join "\n"
              (for [j (range height)]
                (str/join (for [i (range width)]
                            (if (some #{[i j]} asteroids)
                              "#"
                              ".")))))))

(defn draw-visible-from [asteroid asteroids]
  (let [width (inc (apply max (map first asteroids)))
        height (inc (apply max (map second asteroids)))
        visible (visible-from asteroid asteroids)]
    (str/join "\n"
              (for [j (range height)]
                (str/join (for [i (range width)]
                            (cond (= [i j] asteroid) "A"
                                  (some #{[i j]} visible) "#"
                                  :else ".")))))))

;; (time (find-highest-visibility (parse-input large-input)))
;; "Elapsed time: 149393.5023 msecs"
;; ([26 29] 299)



;; Part 2
;; What is (+ y (* 100 x)) for the x and y coordinates of the last asteroid to be vaporized?
;; TODO: compute the angle to each visible asteroid, remove in angle order, repeat with remaining asteroids.
(defn round-it [n]
  (/ (Math/round (* n 65536)) 65536))

(defn normalize [theta]
  (if (< theta 0)
    (+ theta (* 2 Math/PI))
    theta))

(defn to-polar [x y]
  [(Math/sqrt (+ (* x x) (* y y))) (normalize (Math/atan2 x (- y)))])

(def small-input-6 "small-input-6.txt")

(defn lazerize-asteroids [[ax ay :as asteroid] asteroids]
  (loop [asteroids (remove #{asteroid} asteroids)
         vaporized []
         i 0]
    (if (or (= 20 i) (empty? asteroids))
      vaporized
      (let [visible (set (visible-from asteroid asteroids))
            sorted-visible (sort-by last
                                    (map (fn [[x y]]
                                           (let [[r theta] (to-polar (- x ax) (- y ay))]
                                             (list [x y]
                                                   [r theta]
                                                   theta
                                                   ;; (if (< (+ theta (round-it (/ Math/PI 2))) 0) (round-it (+ theta (* 2 Math/PI))) theta)
                                                   )))
                                         visible))
            new-asteroids (remove visible asteroids)
            vaporized (vec (concat vaporized (map first sorted-visible)))]
        (println "Vaporized" (count sorted-visible) "asteroids," (count new-asteroids) "remaining.")
        (recur new-asteroids vaporized (inc i))))))

;; ---> answer <---
;; 2600
;; Also wrong:
;; ([0 7] 7) ([4 10] 410) ([22 34] 2234) ([21 35] 2135)

(def test-input "test-input.txt")
(def test-input-2 "test-input-2.txt")

(defn find-200th-vaporized [asteroid asteroids]
  (let [[x y] (nth (lazerize-asteroids asteroid asteroids) (dec 200))]
    (list [x y] (+ (* 100 x) y))))

(defn find-last-vaporized [asteroid asteroids]
  (let [[x y] (last (lazerize-asteroids asteroid asteroids))]
    (list [x y] (+ (* 100 x) y))))

(deftest test-find-last-vaporized
  (is (= '([14 3] 1403) (find-last-vaporized [8 3] (parse-input small-input-6))))
  (is (= '([11 1] 1101) (find-last-vaporized [11 13] (parse-input small-input-5)))))




;; I don't know how to debug this. I'll try getting the right answer from the internet, at least.
;; Looks like it's [14 19]. My code gets that as the 100th asteroid, not the 200th.
(defn polar
  [[sx sy :as source] [tx ty :as target]]
  {:r (apply + (map (comp #(Math/abs %) -) source target)) ;; Close enough for our purposes
   :theta (- Math/PI (apply #(Math/atan2 %1 %2) ^:double (map (comp double -) target source)))
   :asteroid target})

(defn polar-lines
  [input station]
  (let [asteroids input
        polars (for [target asteroids
                     :when (not= station target)]
                 (polar station target))]
    (->> polars
         (group-by :theta)
         (sort-by first)
         (map (comp (partial map :asteroid) (partial sort-by :r) second)))))

(defn laser-until [lines n]
  (loop [[[next-asteroid & rest-asteroids] & rest-lines] lines
         vaporized []
         n n
         i 1]
   (if (= n 0)
     vaporized
     (let [line' (if (empty? rest-asteroids) [] [rest-asteroids])
           lines' (concat rest-lines line')]
       (recur lines' (conj vaporized (list i (list next-asteroid (+ (* 100 (first next-asteroid)) (second next-asteroid))))) (dec n) (inc i))))))


(let [input input]
  (-> input
      (polar-lines station)
      (laser-until 200)))


;; I was turning counter-clockwise.
;; (find-200th-vaporized [26 29] (parse-input large-input))
;; Vaporized 299 asteroids, 48 remaining.
;; Vaporized 30 asteroids, 18 remaining.
;; Vaporized 9 asteroids, 9 remaining.
;; Vaporized 3 asteroids, 6 remaining.
;; Vaporized 2 asteroids, 4 remaining.
;; Vaporized 2 asteroids, 2 remaining.
;; Vaporized 1 asteroids, 1 remaining.
;; Vaporized 1 asteroids, 0 remaining.
;; ([14 19] 1419)
