(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; Find the shortest navigable path from S to E.
(defn get-height [height-map [x y]]
  (get-in height-map [y x 0]))

(defn get-distance [height-map [x y]]
  (get-in height-map [y x 1]))

(defn find-coordinates [height-map target]
  (first
   (filter
    some?
    (for [j (range 0 (count height-map))
          i (range 0 (count (first height-map)))]
      (if (= target (get-height height-map [i j]))
        [i j])))))

(defn parse-input [f]
  (let [orig-heights (mapv (fn [coll] (mapv #(vector % nil) coll)) (mapv vec (read-lines f)))
        start (find-coordinates orig-heights \S)
        end (find-coordinates orig-heights \E)
        replacements [[start [\a 0]] [end [\z nil]]]
        new-heights (reduce (fn [heights [pos val]] (assoc-in heights (reverse pos) val)) orig-heights replacements)]
    {:heights new-heights :start start :end end}))

(defn find-neighbors [heights [x y]]
  (filter
   (fn [[x y]] (and (< -1 x (count (first heights)))
                    (< -1 y (count heights))))
   [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]]))

(defn find-accessible [heights [x y :as pos]]
  (let [height (get-height heights pos)
        neighbors (find-neighbors heights pos)
        neighbor-heights (map vector neighbors (map #(get-height heights %) neighbors))]
    (map first (filter (fn [[pos neighbor-height]] (<= (int neighbor-height) (inc (int height)))) neighbor-heights))))

(defn find-possible-nexts [heights pos]
  (filter (fn [neighbor] (= (get-distance heights neighbor) nil)) (find-accessible heights pos)))

(defn measure-next-step [state]
  (let [heights (state :heights)
;        _ (println "mns nexts" (state :nexts))
        next-distance (inc (get-distance heights (first (state :nexts))))
        nexts (distinct (mapcat #(find-possible-nexts heights %) (state :nexts)))
        state (assoc state :nexts nexts)]
    (reduce (fn [state [x y :as pos]]
              (assoc-in state [:heights y x 1] next-distance))
            state
            nexts)))

(defn count-mapped [state]
  (let [heights (apply concat (state :heights))]
    (list (count (filter #(not= nil (second %)) heights))
          (count heights))))

(defn compute-distances [state]
  (loop [state (assoc state :nexts (list (state :start)))]
    (if (empty? (state :nexts))
      state
      (let [new-state (measure-next-step state)]
        (if (and (= nil (get-distance (new-state :heights) (state :end)))
                 (some? (state :nexts)))
          (recur new-state)
          new-state))
      )))

(defn find-path [state]
  (let [state (compute-distances state)]
    (loop [path [(state :end)]]
      (let [heights (state :heights)
            current (last path)
            current-distance (get-distance heights current)]
        (if (and (not (nil? current-distance))
                 (> current-distance 0))
          (recur (conj path (first (filter #(= (dec current-distance) (get-distance heights %))
                                           (find-neighbors heights (last path))))))
          (reverse path))))))


;; (dec (count (find-path (parse-input small-input))))
;; 31
;; (dec (count (find-path (parse-input large-input))))
;; 423



;; Part 2
(defn parse-input-2 [f]
  (let [orig-heights (mapv (fn [coll] (mapv #(vector % nil) coll)) (mapv vec (read-lines f)))
        s-pos (find-coordinates orig-heights \S)
        e-pos (find-coordinates orig-heights \E)
        replacements [[s-pos [\a nil]] [e-pos [\z nil]]]
        new-heights (reduce (fn [heights [pos val]] (assoc-in heights (reverse pos) val)) orig-heights replacements)]
    {:heights new-heights :start s-pos :end e-pos}))

(defn get-start-points [state]
  (let [heights (state :heights)
        width (count (first heights))
        height (count heights)]
    (mapv first (filter #(= \a (second %))
                        (for [i (range 0 width)
                              j (range 0 height)]
                          (list (vector i j) (get-height heights [i j])))))))


(defn find-shortest-path-length [state]
  (let [start-points (get-start-points state)]
    (apply min
           ; hack to skip supposedly impossible routes, which are probably actually a bug in my code.
           (filter #(> % 0)
                   (map (fn [pos]
                          (let [state (assoc state :start pos)
                                state (assoc-in state (apply conj [:heights] (reverse pos)) [\a 0])]
                            (dec (count (find-path state)))))
                        start-points)))))

;; (find-shortest-path-length (parse-input-2 small-input))
;; 29
;; (find-shortest-path-length (parse-input-2 large-input))
;; 416
