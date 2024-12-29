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

(defn step-positions [{robots :robots [width height :as dimensions] :dimensions :as state}]
  (let [{new-robots :robots new-line-points :line-points}
        (reduce (fn [acc {[x y] :p [vx vy] :v :as robot}]
                  (let [line-points (:line-points acc)
                        newx (mod (+ x vx) width)
                        newy (mod (+ y vy) height)]
                    (assoc acc
                           :robots (conj (:robots acc) {:p [newx newy] :v [vx vy]})
                           :line-points (if (<= newy 3)
                                          (assoc line-points newy (conj (get line-points newy 0) [newx newy]))
                                          line-points))))
                {:robots '()
                 :line-points {0 #{} 1 #{} 2 #{} 3 #{}}}
                robots)]
    (assoc state
          :robots new-robots
          :line-counts (into {} (map (fn [[k v]] [k (count v)]) new-line-points))
         
          ;; (map (fn [{[x y] :p [vx vy] :v :as robot}]
          ;;        {:p [(mod (+ x vx) width) (mod (+ y vy) height)] :v [vx vy]})
          ;;      robots)
          )))

(defn how-long-until-christmas-tree-compute [input max-seconds]
  (let [{robots :robots [width height :as dimensions] :dimensions :as state} (parse-input input)]
    (loop [seconds 0]
      (if (and (> seconds 0)
               (= 0 (rem seconds 10000)))
        (println seconds "seconds"))
      ;; (Thread/sleep 1)
      (let [
            robot-positions (compute-positions state seconds)
            ;; by-quadrant (group-by #(which-quadrant % dimensions) (set robot-positions))

            robots-by-line (group-by second robot-positions)
            ;; matches (apply concat
            ;;                (for [[line robots] robots-by-line
            ;;                      :let [midpoint (quot width 2)]]
            ;;                  (for [[x y :as robot] robots]
            ;;                    (cond (= x midpoint)
            ;;                          true
            ;;                          :else
            ;;                          (boolean (some #{[(- (dec width) x) line]} robots))))))

            ;; balance-score (+ (abs (- (count (:upper-left by-quadrant)) (count (:upper-right by-quadrant))))
            ;;                  (abs (- (count (:lower-left by-quadrant)) (count (:lower-right by-quadrant)))))
            ]
        (cond (> seconds max-seconds)
              nil
              ;; (< balance-score (Math/ceil (* (count robots) 0.1)))
              ;; (= balance-score 0)
              ;; (not (some #(> (count (second %)) 2) robots-by-line))
              (and (= 1 (count (get robots-by-line 0)))
                   (= (quot width 2) (first (first (get robots-by-line 0))))
                   ;; (= 2 (count (get robots-by-line 1)))
                   )
              (doall (list (print-robots robot-positions dimensions)
                           seconds
                           ;; [balance-score (count robots)]
                           ))
              :else
              (recur (inc seconds)))))))

(defn extended-euclidian [a b]
  (loop [r-last a
         s-last 1
         t-last 0
         q 0
         r b
         s 0
         t 1
         ]
    (let [q-next (quot r-last r)
          r-next (rem r-last r)
          s-next (- s-last (* q-next s))
          t-next (- t-last (* q-next t))]
      (if (= r-next 0)
        [s t]
        (recur r s t q-next r-next s-next t-next)))))

(defn time-to-zero [{[x y] :p [vx vy] :v :as robot} height]
  (loop [j y
         t 0]
    (cond (= j 0)
          t
          (and (> t 0) (= j y))
          nil
          :else
          (recur (mod (+ j vy) height) (inc t)))))

(defn time-to-loop [{[x y] :p [vx vy] :v :as robot} height]
  (loop [j y
         t 0]
    (cond (= vy 0)
          nil
          (and (> t 0) (= j y))
          t
          :else
          (recur (mod (+ j vy) height) (inc t)))))

(defn how-long-until-christmas-tree-step [input max-seconds]
  (let [{robots :robots [width height :as dimensions] :dimensions :as state} (parse-input input)
        robot-positions (map :p (:robots state))
        robots-by-line (group-by second robot-positions)
        line-counts (into {}
                          (map (fn [[line robots]]
                                 [line (count robots)])
                               (filter #(<= (first %) 3) robots-by-line)))]
    (println "line-counts" line-counts)
    (loop [seconds 0
           state (assoc state :line-counts line-counts)]
      (if (and (> seconds 0)
               (= 0 (rem seconds 10000)))
        (println seconds "seconds"))
      ;; (Thread/sleep 1)
      (let [line-counts (:line-counts state)]
        (cond (> seconds max-seconds)
              nil
              ;; (< balance-score (Math/ceil (* (count robots) 0.1)))
              ;; (= balance-score 0)
              ;; (not (some #(> (count (second %)) 2) robots-by-line))
              (and
               ;; (= 1 (count (get robots-by-line 0)))
               ;; (= (quot width 2) (first (first (get robots-by-line 0))))
               ;; (= 2 (count (get robots-by-line 1)))
               (= 1 (get line-counts 0))
               (= 2 (get line-counts 1))
               ;; (= 2 (get line-counts 2))
               ;; (= 2 (get line-counts 3))
               ;; (let [robot-positions (map :p (:robots state))
               ;;       robots-by-line (group-by second robot-positions)]
               ;;   (= (quot width 2) (first (first (get robots-by-line 0)))))
               )
              (doall (list (print-robots (map :p (:robots state)) dimensions)
                           seconds
                           ;; [balance-score (count robots)]
                           ))
              :else
              (recur (inc seconds)
                     (step-positions state)
                     ;; (compute-positions state (inc seconds))
                     ))))))


;; My hope is to compute when each robot will reach y=0 and only check the states where at least
;; (at most?) one robot is on the top line.

;; I'm not sure that will be enough. Maybe I need to compute when each robot will be in the top center position.

  ;; (let [{robots :robots [width height :as dimensions] :dimensions :as state} (parse-input large-input)
  ;;                      times (for [robot robots
  ;;                                  :let [y (second (:p robot))
  ;;                                        vy (second (:v robot))
  ;;                                        ;; _ (doall (println "robot" robot) (flush))
  ;;                                        t-z (time-to-zero robot height)
  ;;                                        t-l (time-to-loop robot height)]
  ;;                                  ]
  ;;                              {:height height
  ;;                               :robot robot
  ;;                               :t-z t-z
  ;;                               :t-l t-l})]
  ;;                  (filter #(= 1 (second %)) (frequencies (map #(vector (:t-z %) times)))))
