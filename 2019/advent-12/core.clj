(ns advent-12.core)

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

;; Day 12: The N-Body Problem

;; Part 1
;; 
(def moon-names ["Io" "Europa" "Ganymede" "Callisto"])

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(rest (re-matches #".+?([-\d]+).+?([-\d]+).+?([-\d]+).+" %)))
       (map #(mapv parse-long %))
       (map-indexed (fn [i pos] {(get moon-names i) {:position pos :velocity [0 0 0]}}))
       (apply merge)))

(defn pairs [xs]
  (if (< (count xs) 3)
    (list xs)
    (concat (map #(list (first xs) %) (rest xs))
            (pairs (rest xs)))))

(defn velocity-change [this other]
  (if (= this other)
    0
    (/ (- other this) (abs (- other this)))))

(defn vec+ [v1 v2]
  (mapv + v1 v2))

(defn apply-gravity [moons]
  (reduce (fn [moons [[name1 moon1] [name2 moon2]]]
            (let [p1 (:position moon1)
                  p2 (:position moon2)
                  change1 [(velocity-change (get p1 0) (get p2 0))
                           (velocity-change (get p1 1) (get p2 1))
                           (velocity-change (get p1 2) (get p2 2))]
                  change2 [(velocity-change (get p2 0) (get p1 0))
                           (velocity-change (get p2 1) (get p1 1))
                           (velocity-change (get p2 2) (get p1 2))]]
              (-> moons
                  (update-in [name1 :velocity] #(vec+ % change1))
                  (update-in [name2 :velocity] #(vec+ % change2)))))
          moons
          (pairs moons)))

(defn apply-velocity [moons]
  (reduce (fn [moons [name {:keys [position velocity] :as moon}]]
            (assoc moons name {:position (vec+ position velocity) :velocity velocity}))
          {}
          moons))

(defn apply-pe [moons]
  (reduce (fn [moons [name {:keys [position velocity] :as moon}]]
            (assoc moons name (assoc moon :pe (apply + (map (comp + abs) position)))))
          {}
          moons))

(defn apply-ke [moons]
  (reduce (fn [moons [name {:keys [position velocity] :as moon}]]
            (assoc moons name (assoc moon :ke (apply + (map (comp + abs) velocity)))))
          {}
          moons))

(defn apply-total-energy [moons]
  (reduce (fn [moons [name {:keys [ke pe] :as moon}]]
            (assoc moons name (assoc moon :total-energy (* ke pe))))
          {}
          moons))

(defn compute-orbit [moons steps]
  (nth (iterate (comp apply-total-energy apply-ke apply-pe apply-velocity apply-gravity) moons) steps))

(defn sum-total-energy [moons steps]
  (apply + (map :total-energy (vals (compute-orbit moons steps)))))

(def small-input-2 "small-input-2.txt")

(deftest test-sum-total-energy
  (is (= 179 (sum-total-energy (parse-input small-input) 10)))
  (is (= 1940 (sum-total-energy (parse-input small-input-2) 100))))

;; (sum-total-energy (parse-input small-input) 10)
;; 179
;; (sum-total-energy (parse-input small-input-2) 100)
;; 1940
;; (sum-total-energy (parse-input large-input) 1000)
;; 7636



;; Part 2
;; 
