 (ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (->> input
       (read-lines)
       (map #(str/split % #" +"))
       (map #(remove #{""} %))
       ))
    

;; Part 1
;; How many times will a tachyon beam be split?
(defn handle-input [lines]
  (let [grid (->> lines
                  (map vec)
                  vec)
        height (count grid)
        width (count (first grid))]
    (let [grid (into {}
                     (for [j (range height)
                           i (range width)]
                       (vector [i j] (get-in grid [j i]))))]
      {:grid grid
       :bounds [width height]
       :start (first (first (filter #(#{\S} (second %)) grid)))}
      )))

(defn parse-input [input]
  (->> input
       (read-lines)
       (handle-input)))

(defn count-beam-splits [{:keys [grid bounds start] :as state}]
  (loop [currents [start]
         split-count 0
         n 0]
    (if (= (second bounds) (second (first currents)))
      split-count
      (let [[nexts split-count] (reduce (fn [[nexts c :as acc] [x y]]
                                          (if (= \^ (get grid [x (inc y)]))
                                            [(distinct (concat nexts [[(dec x) (inc y)] [(inc x) (inc y)]])) (inc c)]
                                            [(distinct (conj nexts [x (inc y)])) c]))
                                        [[] split-count]
                                        currents)]
        (recur nexts split-count (inc n))))))

;; (time (count-beam-splits (parse-input small-input)))
;; "Elapsed time: 1.3783 msecs"
;; 21
;; (time (count-beam-splits (parse-input large-input)))
;; "Elapsed time: 112.3846 msecs"
;; 1590


;; Part 2
;; How many paths are there through the beam splitters?
(defn trace-beam-paths
  ([{:keys [grid bounds start] :as state}]
   (trace-beam-paths state (dec (second bounds))))
  ([{:keys [grid bounds start] :as state} max-y]
   (loop [currents {start 1}
          n 0]
     ;; (println "currents" currents)
     (if (>= n max-y)
       currents
       (let [nexts (reduce (fn [nexts [[x y] path-count]]
                             ;; (println "nexts" nexts)
                             (if (= \^ (get grid [x (inc y)]))
                               (let [u1 (update nexts [(dec x) (inc y)] (fn [v] (+ (if (nil? v) 0 v) path-count)))
                                     u2 (update u1 [(inc x) (inc y)] (fn [v] (+ (if (nil? v) 0 v) path-count)))
                                     ;; _ (println "xy" [x y] "nexts" nexts "u2" u2)
                                     ;; _ (flush)
                                     ]
                                 ;; (update
                                 ;; (update nexts [(dec x) (inc y)] (fn [v] (if (nil? v) 1 (inc v))))
                                 ;; [(inc x) (inc y)] (fn [v] (if (nil? v) 1 (inc v))))
                                 u2)
                               (let [u3 (update nexts [x (inc y)] (fn [v] (+ (if (nil? v) 0 v) path-count)))
                                     ;; _ (println "xy" [x y] "u3" u3)
                                     ]
                                 u3)))
                           {}
                           currents)]
         (recur nexts (inc n)))))))

(defn count-beam-paths
  ([{:keys [grid bounds start] :as state} max-y]
   (apply + (map second (filter #(= max-y (second (first %))) (trace-beam-paths state max-y)))))
  ([{:keys [grid bounds start] :as state}]
   (apply + (map second (filter #(= (dec (second bounds)) (second (first %))) (trace-beam-paths state)))))
  )

(def test-input-1 ["...S..." "...^..." "......."])
(def test-input-2 ["...S..." "...^..." "..^.^.." "......."])
(def test-input-3 ["...S..." "...^..." "..^.^.." ".^.^.^." "......."])
(def test-input-4 ["....S...." "....^...." "...^.^..." "..^.^.^.." ".^.^...^." "........."])

(deftest test-inputs
  (is (= 2 (count-beam-paths (parse-input small-input) 2)))
  (is (= 4 (count-beam-paths (parse-input small-input) 4)))
  (is (= 8 (count-beam-paths (parse-input small-input) 6)))
  (is (= 13 (count-beam-paths (parse-input small-input) 8)))
  (is (= 20 (count-beam-paths (parse-input small-input) 10)))
  (is (= 26 (count-beam-paths (parse-input small-input) 12)))
  (is (= 40 (count-beam-paths (parse-input small-input)))))
;; ...S...
;; ...^...
;; .......

;; ...S...
;; ...^...
;; ..^.^..
;; .......

;; ...S...
;; ...^...
;; ..^.^..
;; .^.^.^.
;; .......

;; ....S....
;; ....^....
;; ...^.^...
;; ..^.^.^..
;; .^.^...^.
;; .........


;; (time (count-beam-paths (parse-input small-input)))
;; "Elapsed time: 2.2263 msecs"
;; 40
;; (time (count-beam-paths (parse-input large-input)))
;; "Elapsed time: 53.5705 msecs"
;; 20571740188555
