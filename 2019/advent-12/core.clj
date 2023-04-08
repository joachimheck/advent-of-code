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

(defn vec+ [& vs]
  (apply mapv + vs))

(defn apply-gravity [moons]
  (reduce (fn [moons [[name1 moon1] [name2 moon2]]]
            (let [p1 (:position moon1)
                  p2 (:position moon2)
                  change1 (vec (map velocity-change p1 p2))
                  change2 (vec (map velocity-change p2 p1))]
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
;; How many steps does it take to reach a previous state?
(def apply-all (comp apply-total-energy apply-ke apply-pe apply-velocity apply-gravity))

(defn apply-gravity-one-dimension [moons dimension]
  (let [sorted (sort-by first (apply merge-with concat (map (fn [[name stats]] {(get-in stats [:position dimension]) (list name)}) moons)))
        pos-count (count sorted)
        indexed (map-indexed list sorted)]
    ;; (println "indexed" indexed)
    (reduce (fn [acc [index [pos moons-at-pos]]]
              (let [before (apply + (map #(count (second (second %))) (filter #(< (first %) index) indexed)))
                    after (apply + (map #(count (second (second %))) (filter #(> (first %) index) indexed)))]
                (reduce (fn [acc m] (assoc acc m (- after before))) acc moons-at-pos)))
            {}
            indexed)))

(defn apply-gravity-2 [moons]
  (let [accelerations (vec (for [dimension [0 1 2]]
                          (apply-gravity-one-dimension moons dimension)))]
    (reduce (fn [moons name]
              (update-in moons [name :velocity] (fn [v] (vec+ v (mapv #(get-in accelerations [% name]) [0 1 2])))))
            moons
            (keys moons))))

(defn apply-acceleration [moons]
  (let [by-dimension (vec (for [dimension [0 1 2]]
                   (apply-gravity-one-dimension moons dimension)))]
    (reduce (fn [acc name]
              (assoc-in acc [name :acceleration] (mapv #(get-in by-dimension [% name]) [0 1 2])))
            moons
            moon-names)))

(defn run-until-initial-state [moons process-fn]
  (let [initial-moons (process-fn moons)]
    (loop [moons initial-moons
           i 0]
      ;; (if (and (> i 0) (= 0 (mod i 100)))
      ;;   (println ))
      (if (and (> i 0) (= moons initial-moons))
        i
        (recur (apply-all moons) (inc i))))))

(def apply-all-2 (comp apply-total-energy apply-ke apply-pe apply-velocity apply-gravity-2))

(defn compute-orbit-2 [moons steps]
  (nth (iterate (comp apply-total-energy apply-ke apply-pe apply-velocity apply-gravity-2) moons) steps))

(defn sum-total-energy-2 [moons steps]
  (apply + (map :total-energy (vals (compute-orbit-2 moons steps)))))


(deftest test-sum-total-energy-2
  (is (= 179 (sum-total-energy-2 (parse-input small-input) 10)))
  (is (= 1940 (sum-total-energy-2 (parse-input small-input-2) 100))))

;; TODO: From the accelerations, figure out how many steps we can go until the moons change position.
;; p(t) = (+ p (* v t) (/ (* a t t) 2))
;; pa(t) = (+ pa (* va t) (/ (* aa t t) 2))
;; pb(t) = (+ pb (* vb t) (/ (* ab t t) 2))
;;
;; (+ pa (* va t) (/ (* aa t t) 2)) = (+ pb (* vb t) (/ (* ab t t) 2))
;; (+ (* 2 pa) (* 2 va t) (* aa t t)) = (+ (* 2 pb) (* 2 vb t) (* ab t t))
;; (- (+ (* 2 va t) (* aa t t)) (+ (* 2 vb t) (* ab t t))) = (- (* 2 pb) (* 2 pa))
(defn roots [a b c]
  [(/ (+ (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
   (/ (- (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))])

(defn intersection [p1 v1 a1 p2 v2 a2]
  (roots (- a1 a2) (- (* 2 v1) (* 2 v2)) (- p1 p2)))

(defn steps-until-intersection [moon1 moon2]
  (let [[p1 v1 a1] (let [{:keys [position velocity acceleration]} moon1] [position velocity acceleration])
        [p2 v2 a2] (let [{:keys [position velocity acceleration]} moon2] [position velocity acceleration])
        _ (if (some nil? [p1 v1 a1 p2 v2 a2]) (println "nil input" moon1 moon2))
        intersections (map #(intersection (get p1 %) (get v1 %) (get a1 %) (get p2 %) (get v2 %) (get a2 %)) [0 1 2])]
    (apply min (map #(int (Math/ceil %)) (flatten (map (fn [v] (filter #(> % 0) v)) intersections))))))

(defn vec* [v k]
  (mapv #(* k %) v))

(defn compute-velocity-and-position [moons steps]
  (reduce (fn [moons [name {:keys [position velocity acceleration] :as moon}]]
            (-> moons
                (assoc name {:velocity (vec+ velocity (vec* acceleration steps))})
                (assoc name {:position (vec+ position (vec* velocity steps) (vec* acceleration (/ (* steps steps) 2)))})))
          {}
          moons))

(defn run-until-initial-state-2 [moons]
  (let [initial-moons (apply-acceleration moons)]
    (loop [moons initial-moons
           i 0
           total-steps 0]
      (if (and (> i 0) (= moons initial-moons))
        {:iterations i
         :steps total-steps}
        (let [steps (apply min (map (fn [[[_ moon1] [_ moon2]]] (steps-until-intersection moon1 moon2)) (pairs moons)))]
          (if (= steps 0) (println "zero steps"))
          (recur (compute-velocity-and-position moons steps) (inc i) (+ total-steps steps)))))))


;; We're losing acceleration and velocity, and getting fractional position values.
;; Also, the current code doesn't handle the points where the moons change orders (and accelerations).
;; (run-until-initial-state-2 (parse-input small-input))
;; nil input {:position [1/2 -1/2 3/2]} {:position [5/2 -17/2 -11/2]}
