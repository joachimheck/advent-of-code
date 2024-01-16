(ns advent-21.core)

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
;; How many garden plots can be reached in the given number of steps?
(defn parse-pattern [input]
  (let [vectorized (mapv vec input)
        height (count vectorized)
        width (count (first vectorized))
        grid (into {}
                   (for [j (range height)
                         i (range width)
                         :let [c (get-in vectorized [j i])]]
                     [[i j] c]))
        start (get (set/map-invert grid) \S)]
    {:width width
     :height height
     :grid (assoc grid start \.)
     :start start}))

(defn parse-input [input]
  (->> (read-lines input)
       (parse-pattern)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (get grid [i j]))))))

(defn move [x y dir amount]
  (case dir
    :right [(+ x amount) y]
    :down [x (+ y amount)]
    :left [(- x amount) y]
    :up [x (- y amount)]))

(defn adjacent [[x y :as p]]
  #{[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]})

(defn in-bounds [width height points]
  (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))) points))

(defn adjacent-open [{:keys [width height grid]} p]
  (->> p
       (adjacent)
       (map #(list % (get grid %)))
       (filter #(= \. (second %)))
       (map first)
       (in-bounds width height)))

(defn find-tiles-by-steps [{:keys [width height grid start] :as pattern} steps]
  (loop [open-set #{start}
         i steps]
    (if (<= i 0)
      open-set
      (recur (set (mapcat #(adjacent-open pattern %) open-set)) (dec i)))))

(defn how-many-plots [input steps]
  (count (find-tiles-by-steps (parse-input input) steps)))

;; (time (how-many-plots small-input 6))
;; "Elapsed time: 1.0878 msecs"
;; 16

;; (time (how-many-plots large-input 64))
;; "Elapsed time: 607.8964 msecs"
;; 3751



;; Part 2
;; With infinitely repeating gardens, how many plots can be reached in a large number of steps?
(defn mark-tiles [{:keys [width height grid start] :as pattern} points]
  (assoc pattern :grid
         (reduce (fn [acc p] (assoc acc p \O))
                 grid
                 points)))

;; TODO: maybe look for repetition in the increase from one step count to the next?
(defn wrap [[x y] width height]
  [(cond (< x 0) (- width x)
         (>= x width) (- x width)
         :else x)
   (cond (< y 0) (- height y)
         (>= y height) (- y height)
         :else y)])

(defn get-wrapped [grid [x y] width height]
  (get grid [(mod x width) (mod y height)]))

(defn adjacent-open-wrap [{:keys [width height grid]} p]
  (->> p
       (adjacent)
       (map #(list % (get-wrapped grid % width height)))
       (filter #(= \. (second %)))
       (map first)))

(defn find-tiles-by-steps-wrap [{:keys [width height grid start] :as pattern} steps]
  (loop [open-set #{start}
         i steps]
    (if (<= i 0)
      open-set
      (recur (set (mapcat #(adjacent-open-wrap pattern %) open-set)) (dec i)))))

(defn how-many-plots-wrap [input steps]
  (count (find-tiles-by-steps-wrap (parse-input input) steps)))


(def reachable-counts (atom {}))

(defn count-reachable-memo [{:keys [width height grid] :as pattern} p steps]
  (if (= steps 0)
    1
    (if-let [cached (get @reachable-counts [p steps])]
      cached
      (let [adjacent (adjacent-open-wrap pattern p)
            reachable (apply + (conj (map #(count-reachable-memo pattern % (dec steps)) adjacent)
                                     ;;(count adjacent)
                                     ))]
        (swap! reachable-counts assoc [p steps] reachable)
        reachable))))

(defn how-many-plots-memo [input steps]
  (reset! reachable-counts {})
  (let [{:keys [start] :as pattern} (parse-input input)]
    (count-reachable-memo pattern start steps)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (get grid [i j]))))))

(defn count-flood-fill [{:keys [width height grid start] :as pattern} steps]
  (loop [open-set #{start}
         flag :even
         steps-left steps
         visited-odd #{}
         visited-even #{}]
    (let [new-steps-left (dec steps-left)
          new-visited-even (if (= flag :even)
                             (apply conj visited-even open-set)
                             visited-even)
          new-visited-odd (if (= flag :odd)
                            (apply conj visited-odd open-set)
                            visited-odd)
          last-visited (if (= flag :even) visited-odd visited-even)]
      (if (= steps-left 0)
        (if (even? steps) (count new-visited-even) (count new-visited-odd))
        (recur (set (remove last-visited (mapcat #(adjacent-open-wrap pattern %) open-set)))
               (if (= flag :even) :odd :even)
               (dec steps-left)
               new-visited-odd
               new-visited-even)))))

;; This generates the right answer (as did my original code for part 1), but it is very slow.
;; I don't see how to just count the available positions while also taking into account the
;; fact that paths have to go around obstacles.

(defn find-cycle [open-set-counts count-sets n]
  (if (> (count open-set-counts) n)
    (let [last-n (take-last n open-set-counts)
          multiples (set (for [i (range 1 8)]
                           (map #(* i %) last-n)))]
      (if-let [match (some multiples count-sets)]
        {:repeat-detected [match last-n]}))))

(defn count-multiple-flood-fill [{:keys [width height grid start] :as pattern} max-steps cycle-size]
  (loop [open-set #{start}
         flag :even
         steps 0
         visited-odd #{}
         visited-even #{}
         counts []
         d1s []
         d2s []
         d3s []
         d2-cycles []]
    (if (= steps (inc max-steps))
      ;; {:result (map-indexed list (map list counts d1s d2s d3s d2-cycles))}
      {:counts counts :d1s d1s :d2-cycles d2-cycles}
      (let [new-steps (inc steps)
            new-visited-even (if (= flag :even)
                               (apply conj visited-even open-set)
                               visited-even)
            new-visited-odd (if (= flag :odd)
                              (apply conj visited-odd open-set)
                              visited-odd)
            last-visited (if (= flag :even) visited-odd visited-even)
            new-count (if (even? steps) (count new-visited-even) (count new-visited-odd))
            new-d1 (- new-count (or (last counts) 0))
            new-d2 (- new-d1 (or (last d1s) 0))
            new-d3 (- new-d2 (or (last d2s) 0))
            new-d2-cycle (- new-d1 (or (get d1s (- (count d1s) cycle-size)) 0))]
        (recur (set (remove last-visited (mapcat #(adjacent-open-wrap pattern %) open-set)))
               (if (= flag :even) :odd :even)
               (inc steps)
               new-visited-odd
               new-visited-even
               (conj counts new-count)
               (conj d1s new-d1)
               (conj d2s new-d2)
               (conj d3s new-d3)
               (conj d2-cycles new-d2-cycle))))))

(defn format-for-desmos [points]
  (str/join ","
            (for [[x y] points]
              (format "(%d,%d)" x y))))

;; 109245867
;; ---> answer <---

;; TODO: I've detected a pattern in the first derivative of the number of new plots accessed
;; with every step, but I need to figure out how to use it to compute the full number of plots.

(defn compute-plots-cycles [input steps cycle-size min-step]
  (let [pattern (parse-input input)
        {:keys [counts d1s d2-cycles]} (count-multiple-flood-fill pattern (+ min-step (* 2 cycle-size)) cycle-size)
        start-v (get d1s min-step)
        start-step (min steps (+ cycle-size (first (drop-while #(> % min-step) (iterate #(- % cycle-size) steps)))))
        start-count (count-flood-fill pattern start-step)
        delta-vs (subvec d2-cycles start-step (+ start-step cycle-size))
        delta-x-per-cycle (apply + (reductions + delta-vs))
        cycles (quot (- steps start-step) cycle-size)]
    (println "steps" steps "start-step" start-step "start-count" start-count "cycles" cycles "delta-vs" delta-vs)
    (+ start-count
       (loop [i 0
              x start-count
              v start-v]
         (if (= i cycles)
           x
           (let [new-x (apply + (conj (reductions + v delta-vs) x))
                 new-v (apply + (conj delta-vs v))
                 _ (println "x" x "new-x" new-x "v" v "new-v" new-v)]
             (recur (inc i) new-x new-v)))))))

;; The code above doesn't work - neither does this code to try to move to the next step after step 56.
;; (let [start-x 1988
;;       start-v 74
;;       delta-vs [16 14 18 16 16 14 18 16 9 11 14]
;;       next-x 2072
;;       end-x 2882]
;;   (apply + (conj (reductions + 0 delta-vs) start-x)))
