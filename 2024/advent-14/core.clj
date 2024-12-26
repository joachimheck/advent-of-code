(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn parse-input [input]
  (let [robots (->> input
                    (read-lines)
                    (map #(re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+) ?" %))
                    (map rest)
                    (map #(map parse-long %))
                    (map (fn [[x y vx vy]] {:p [x y] :v [vx vy]})))
        dimensions (if (empty? (first (filter #(or (> (first (:p %)) 11) (> (second (:p %)) 7)) robots)))
                     [11 7] [101 103])]
    {:robots robots :dimensions dimensions}))

;; Part 1
;; Compute the safety factor by identifying how many robots will be in each quadrant
;; of the space after 100 seconds.
(defn compute-positions [{robots :robots dimensions :dimensions :as state} seconds]
  (map (fn [{[x y] :p [vx vy] :v :as robot}]
         ;; (println "robot" robot "dimensions" dimensions)
         [(mod (+ x (* vx seconds)) (first dimensions)) (mod (+ y (* vy seconds)) (second dimensions))])
       robots))

(defn which-quadrant [[x y] [width height]]
  (cond (and (< x (quot width 2))
             (< y (quot height 2)))
        :upper-left
        (and (< x (quot width 2))
             (> y (quot height 2)))
        :lower-left
        (and (> x (quot width 2))
             (> y (quot height 2)))
        :lower-right
        (and (> x (quot width 2))
             (< y (quot height 2)))
        :upper-right))

(defn safety-factor [input]
  (let [seconds 100
        {robots :robots dimensions :dimensions :as state} (parse-input input)
        robots-in-quadrants (dissoc (group-by #(which-quadrant % dimensions)
                                              (compute-positions state seconds))
                                    nil)]
    (apply * (map #(count (second %)) robots-in-quadrants))))


;; (time (safety-factor small-input))
;; "Elapsed time: 0.651 msecs"
;; 12
;; (time (safety-factor large-input))
;; "Elapsed time: 6.6761 msecs"
;; 226236192


;; Part 2
;; How many seconds until the robots take the shape of a christmas tree?
(defn print-robots [positions [width height]]
  (let [occupied (set positions)]
    (doall
     (for [j (range height)]
       (println
        (str/join
         (for [i (range width)]
           (if (some #{[i j]} occupied) "*" "."))))))
    nil))

(defn how-long-until-christmas-tree [input max-seconds]
  (let [{robots :robots [width height :as dimensions] :dimensions :as state} (parse-input input)]
    (loop [seconds 0]
      (if (and (> seconds 0)
               (= 0 (rem seconds 10000)))
        (println seconds "seconds"))
      ;; (Thread/sleep 1)
      (let [robot-positions (compute-positions state seconds)
            robots-by-line (group-by second robot-positions)
            matches (apply concat
                           (for [[line robots] robots-by-line
                                 :let [midpoint (quot width 2)]]
                             (for [[x y :as robot] robots]
                               (cond (= x midpoint)
                                     true
                                     :else
                                     (boolean (some #{[(- (dec width) x) line]} robots))))))]
        (cond (> seconds max-seconds)
              nil
              (every? true? matches)
              (doall (list (print-robots robot-positions dimensions)
                           seconds))
              :else
              (recur (inc seconds)))))))
