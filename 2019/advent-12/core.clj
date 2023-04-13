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

(def apply-all-2 (comp apply-acceleration apply-velocity apply-gravity))

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
  (filter
   #(and (not= ##NaN %) (not= ##Inf %) (not= ##-Inf %))
   [(/ (+ (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
    (/ (- (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))]))

(defn intersection [p1 v1 a1 p2 v2 a2]
  (if (= p1 p2)
    [1]
    (roots (- a1 a2) (- (* 2 v1) (* 2 v2)) (- p1 p2))))

(defn steps-until-intersection [moon1 moon2]
  ;; (println "steps-until-intersection" moon1 moon2)
  (let [[p1 v1 a1] (let [{:keys [position velocity acceleration]} moon1] [position velocity acceleration])
        [p2 v2 a2] (let [{:keys [position velocity acceleration]} moon2] [position velocity acceleration])
        _ (if (some nil? [p1 v1 a1 p2 v2 a2]) (println "nil input" moon1 moon2))
        intersections (map #(intersection (get p1 %) (get v1 %) (get a1 %) (get p2 %) (get v2 %) (get a2 %)) [0 1 2])
        int-intersections (map #(int (Math/floor %)) (flatten (map (fn [v] (filter #(> % 0) v)) intersections)))]
    ;; (println "int-intersections" int-intersections)
    (if (= (count int-intersections) 0)
      1
      (max 1 (apply min int-intersections)))))

(defn steps-until-direction-change [moon1 moon2 axis]
  (let [v1 (get-in (second moon1) [:velocity axis])
        a1 (get-in (second moon1) [:acceleration axis])
        v2 (get-in (second moon2) [:velocity axis])
        a2 (get-in (second moon2) [:acceleration axis])]
    (if (or (and (neg? v1) (pos? v2)) (and (pos? v1) (neg? v2)))
      (min (filter pos? (list (if (= a1 0) -1 (int (Math/ceil (- (/ v1 a1))))) (if (= a2 0) -1 (int (Math/ceil (- (/ v2 a2))))))))
      1)))

(defn steps-until-intersection-2 [moons]
  (let [moon-pairs (pairs moons)]
    (apply min (flatten (map (fn [[moon1 moon2]] (map (fn [axis] (steps-until-direction-change moon1 moon2 axis)) [0 1 2])) moon-pairs)))))

(defn vec* [v k]
  (mapv #(* k %) v))

;; if even (n+1)(n/2)
;; if odd (n+1)(floor n/2) + (ceil n/2)

(defn sum-to-n [n]
  (if (even? n)
    (* (+ n 1) (quot n 2))
    (+ (* (+ n 1) (quot n 2)) (+ 1 (quot n 2)))))

(defn compute-velocity-and-position [moons steps]
  (reduce (fn [moons [name {:keys [position velocity acceleration] :as moon}]]
            (let [new-v (vec+ velocity (vec* acceleration steps))
                  new-p (vec+ position (vec+ (vec* velocity steps) (vec* acceleration (sum-to-n steps))))]
              (-> moons
                  (assoc-in [name :velocity] new-v)
                  (assoc-in [name :position] new-p))))
          moons
          moons))

(defn state-after-steps [moons steps]
  (let [initial-moons (apply-acceleration moons)]
    (loop [moons initial-moons
           step 0]
      (if (= step steps)
        moons
        (recur (apply-all-2 moons) (inc step))))))

(defn run-until-initial-state-2 [moons max-iterations]
  (let [initial-moons (apply-acceleration moons)]
    (loop [moons initial-moons
           check-moons initial-moons
           i 0
           total-steps 0]
      (println i (:position (get moons "Io")))
      ;; (let [with-energy (apply-ke (apply-pe moons))]
      ;;  (println i "/" total-steps "pe" (apply + (map (comp :pe second) with-energy)) "ke" (apply + (map (comp :ke second) with-energy))))
      ;; (if (and (> i 0) (= 0 (mod i 100))) (println i))
      (if (not= moons check-moons)
          {:error "incorrect state"
           :iterations i
           :steps total-steps
           :moons moons
           :correct-moons check-moons}
          (if (or (= i max-iterations) (and (> i 0) (= moons initial-moons)))
            {:iterations i
             :steps total-steps
             :moons moons}
            (let [
                  ;; steps (apply min (map (fn [[[_ moon1] [_ moon2]]] (steps-until-intersection moon1 moon2)) (pairs moons)))
                  steps (steps-until-intersection-2 moons)
                  ]
              ;; (println "About to take" steps "steps. moons:" moons)
              (if (= steps 0) (println "zero steps"))
              (recur (apply-acceleration (compute-velocity-and-position moons steps))
                     (state-after-steps moons steps)
                     (inc i)
                     (+ total-steps steps))))))))


;; We're losing acceleration and velocity, and getting fractional position values.
;; Also, the current code doesn't handle the points where the moons change orders (and accelerations).
;; (run-until-initial-state-2 (parse-input small-input))
;; nil input {:position [1/2 -1/2 3/2]} {:position [5/2 -17/2 -11/2]}

;; (run-until-initial-state-2 (parse-input small-input) 2800)
;; {:iterations 2800,
;;  :steps 2802,
;;  :moons
;;  {"Io" {:position [-1 -3 6], :velocity [0 -2 1], :acceleration [3 -1 -3]},
;;   "Europa" {:position [4 -2 3], :velocity [1 5 3], :acceleration [-3 -3 -1]},
;;   "Ganymede" {:position [2 -4 -2], :velocity [-1 3 -3], :acceleration [1 2 1]},
;;   "Callisto" {:position [3 -4 -5], :velocity [0 -6 -1], :acceleration [-1 2 3]}}}

;; I think the code is working as designed now but we're skipping over the initial state, so we don't stop correctly.
;; Furthermore, doing 2802 steps in 2800 iterations is hardly enough savings to solve the problem.
;; Weird - does that mean we are only skipping steps exactly as we reach the initial state again? Is that a coincidence?
;; I don't see how the energy calculations from part 1 can help, but maybe that could bear some more consideration.

;; (run-until-initial-state-2 (parse-input small-input) 3000)
;; {:iterations 2772,
;;  :steps 2772,
;;  :moons
;;  {"Io" {:position [-1 0 2], :velocity [0 0 0], :acceleration [3 -1 -1]},
;;   "Europa" {:position [2 -10 -7], :velocity [0 0 0], :acceleration [1 3 3]},
;;   "Ganymede" {:position [4 -8 8], :velocity [0 0 0], :acceleration [-3 1 -3]},
;;   "Callisto" {:position [3 5 -1], :velocity [0 0 0], :acceleration [-1 -3 1]}}}

;; I replaced the intersection computation with one to check when velocities stop having opposite direction.
;; Now it seems we don't skip any steps at all!

;; I checked with the internet - I hadn't noticed that the individual coordinates of the moons' positions move in
;; repeating cycles. So the trick is to identify the lengths of all the cycles.

(defn all-indexes-of [n coll]
  (map first (filter #(= (second %) n) (map-indexed vector coll))))

(defn loops [seen potential-loops]
  (filter (fn [[a b]]
            (> (- (count seen) b) (- b a))
            )
          potential-loops))

(defn find-loop [ns]
  (loop [ns ns
         seen []
         potential-loops []]
    (let [loops (filter (fn [[a b]] (> (- (count seen) b) (inc (- b a)))) potential-loops)]
      (if (empty? loops)
       (let [n (first ns)
             new-seen (conj seen n)
             current-index (count seen)
             previous-indices (all-indexes-of n seen)
             new-potential-loops (apply conj potential-loops (mapv #(list % current-index) previous-indices))
             new-potential-loops (vec (remove (fn [[a b]]
                                                (let [s2 (subvec seen b)
                                                      s1 (subvec seen a (+ a (count s2)))]
                                                  (or (not= s1 s2)
                                                      (< (- b a) 3))))
                                              new-potential-loops))]
         (recur (rest ns) new-seen new-potential-loops))
       loops))))

;; (find-loop (take 20 (apply concat (repeat [1 2 3 3 2 1]))))
;; ((0 6))

;; OK, I have a loop finder so now I need to apply it to the various coordinate numbers coming out of the orbit simulation.

(defn find-loop-multi [acc [i n]]
  (let [seen (get-in acc [:seen i] [])
        potential-loops (get-in acc [:potential-loops i] [])
        loops (get-in acc [:loops i] [])]
    (if (get loops i)
      acc
      (let [new-seen (conj seen n)
            current-index (count seen)
            previous-indices (all-indexes-of n seen)
            new-potential-loops (apply conj potential-loops (mapv #(list % current-index) previous-indices))
            new-potential-loops (vec (remove (fn [[a b]]
                                               (let [s2 (subvec seen b)
                                                     s1 (subvec seen a (+ a (count s2)))]
                                                 (or (not= s1 s2)
                                                     (< (- b a) 3))))
                                             new-potential-loops))
            new-loops (filter (fn [[a b]] (> (- (count seen) b) (inc (- b a)))) potential-loops)]
        (cond-> acc
          true (assoc-in [:seen i] new-seen)
          true (assoc-in [:potential-loops i] new-potential-loops)
          (and (empty? loops) (seq new-loops)) (assoc-in [:loops i] (first new-loops)))))))

(defn break-up-state [moons]
  (for [moon-name (keys moons)
        parameter [:position :velocity]
        coordinate [0 1 2]]
    (list (format "%s-%s-%d" moon-name parameter coordinate) (get-in moons [moon-name parameter coordinate]))))

(defn find-loops [moon-states]
  (loop [moon-states moon-states
         state {:seen {}
                :potential-loops {}
                :loops {}}
         i 0]
    (if (and (> i 0) (= 0 (mod i 100))) (println i (count (:loops state))))
    (let [{:keys [seen loops]} state]
      (if (or (empty? moon-states) (= (count loops) 24))
        {:iterations i
         :loops loops}
        (recur (rest moon-states)
               (reduce find-loop-multi state (break-up-state (first moon-states)))
               (inc i))))))

(def apply-all-3 (comp apply-velocity apply-gravity))

(defn parse-input-vector [f]
  (->> f
       (read-lines)
       (map #(rest (re-matches #".+?([-\d]+).+?([-\d]+).+?([-\d]+).+" %)))
       (map #(mapv parse-long %))
       (flatten)
       (vec)))

(defn find-orbit-loops [moons]
  (let [initial-moons (apply-all moons)]
    (iterate apply-all moons)))


;; (sort (:loops (find-loops (iterate apply-all-3 (apply-velocity (parse-input small-input))))))
;; (["Callisto-:position-0" (0 6)]
;;  ["Callisto-:position-1" (0 28)]
;;  ["Callisto-:position-2" (0 44)]
;;  ["Callisto-:velocity-0" (0 6)]
;;  ["Callisto-:velocity-1" (0 28)]
;;  ["Callisto-:velocity-2" (0 44)]
;;  ["Europa-:position-0" (0 18)]
;;  ["Europa-:position-1" (0 28)]
;;  ["Europa-:position-2" (0 44)]
;;  ["Europa-:velocity-0" (0 18)]
;;  ["Europa-:velocity-1" (0 28)]
;;  ["Europa-:velocity-2" (0 44)]
;;  ["Ganymede-:position-0" (0 18)]
;;  ["Ganymede-:position-1" (0 28)]
;;  ["Ganymede-:position-2" (0 44)]
;;  ["Ganymede-:velocity-0" (0 18)]
;;  ["Ganymede-:velocity-1" (0 28)]
;;  ["Ganymede-:velocity-2" (0 44)]
;;  ["Io-:position-0" (0 6)]
;;  ["Io-:position-1" (0 28)]
;;  ["Io-:position-2" (0 44)]
;;  ["Io-:velocity-0" (0 6)]
;;  ["Io-:velocity-1" (0 28)]
;;  ["Io-:velocity-2" (0 44)])

(defn prime-factors [n]
  (frequencies
   (loop [n n
          factors []]
     (let [new-factor (reduce (fn [acc d] (if (= 0 (mod n d)) (reduced d) 0))
                              0
                              (range 2 (inc n)))]
       (if (= new-factor n)
         (conj factors new-factor)
         (recur (quot n new-factor) (conj factors new-factor)))))))

;; TODO: for each xyz triplet, compute the least common multiple (? the product of all the prime factors
;; after factors common to all three numbers have been removed). Then, for the four numbers, compute the
;; greatest common divisor?

;; (apply * [6 28 44])
;; 7392
;; advent-12.core> (* (* 2 3) (* 2 2 7) (* 2 2 11))
;; 7392
;; advent-12.core> (* (* 3) (* 2 7) (* 2 11))
;; 924

;; (let [loops (:loops (find-loops (iterate apply-all-3 (apply-velocity (parse-input small-input)))))
;;                       k-tuples (for [n moon-names]
;;                                  (for [c [0 1 2]]
;;                                    (format "%s-:position-%d" n c)))]
;;                   (distinct (map (fn [k-tuple] (prime-factors (apply * (map (fn [k] (second (get loops k))) k-tuple)))) k-tuples)))

(defn lcm [ns]
  (reduce (fn [acc [n p]]
            (* acc (apply * (repeat p n))))
          1
          (apply merge-with max (map prime-factors ns))))

(defn find-shortest-loop [moons]
  (let [loops (:loops (find-loops (iterate apply-all-3 (apply-velocity moons))))]
    (println "loops" loops)
    (lcm (for [moon-name moon-names]
           (lcm (for [i [0 1 2]]
                  (apply - (reverse (get loops (format "%s-:position-%d" moon-name i))))))))))

;; (find-shortest-loop (parse-input large-input))
;; ...
;; 57600 22
;; 57700 23
