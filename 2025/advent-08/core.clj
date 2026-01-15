 (ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
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
       (map #(str/split % #","))
       (map #(map parse-long %))
       ))
    

;; Part 1
;; After connecting the closest junction boxes, what is the
;; product of the sizes of the three largest circuits?
(defn distance [[x y z] [i j k]]
  (math/sqrt
   (+ (math/pow (- z k) 2)
      (math/pow (- y j) 2)
      (math/pow (- x i) 2))))

(defn find-distances [boxes]
  (loop [box (first boxes)
         remaining (rest boxes)
         distances {}]
    (if (empty? remaining)
      distances
      (recur (first remaining)
             (rest remaining)
             (into distances (map #(vector [box %] (distance box %)) remaining))))))
                          
(defn in-circuit? [[a b :as pair] circuits]
  (and (some #{a} circuits) (some #{b} circuits)))

(defn find-unconnected-pair [sorted-distances circuits]
  (first (filter #(not (in-circuit? (first %) circuits)) sorted-distances)))

(defn find-matching-circuits [[a b :as pair] circuits]
  (filter #(or (some #{a} %) (some #{b} %)) circuits))
    
(defn merge-circuits [[a b :as pair] circuits]
  (let [matching (filter #(or (some #{a} %) (some #{b} %)) circuits)]
    (if (empty? matching)
      (conj circuits pair)
      (conj
       (remove #(some #{%} matching) circuits)
       (distinct (concat (apply concat matching) pair))))))

(defn find-circuit [box circuits]
  (first (filter #(some #{box} %) circuits)))

(defn make-circuits [boxes distances circuit-count]
  (loop [sorted-distances (sort-by second distances)
         distances distances
         circuit-count circuit-count
         circuits (map list boxes)]
    (if (zero? circuit-count)
      circuits
      (let [pair-distance (first sorted-distances)
            pair (first pair-distance)
            pair-circuits (map #(find-circuit % circuits) pair)]
        (recur (rest sorted-distances)
               (dissoc distances pair)
               (dec circuit-count)
               (merge-circuits pair circuits))))))

(defn multiply-largest-circuit-sizes [input connections]
  (let [boxes (parse-input input)
        distances (find-distances boxes)
        circuits (make-circuits boxes distances connections)]
    (apply * (map count (take 3 (reverse (sort-by count circuits)))))))

;; 19850 - too low

;; (time (multiply-largest-circuit-sizes small-input 10))
;; "Elapsed time: 1.8112 msecs"
;; 40
;; (time (multiply-largest-circuit-sizes large-input 1000))
;; "Elapsed time: 2843.4614 msecs"
;; 68112


;; Part 2
;; Connect all the boxes into one circuit. What's the product
;; of the x coordinates of the last two connected junction boxes?

(defn make-one-circuit [boxes distances]
  (loop [sorted-distances (sort-by second distances)
         circuits (map list boxes)
         last-pair []]
    (if (= 1 (count circuits))
      last-pair
      (let [pair-distance (first sorted-distances)
            pair (first pair-distance)
            pair-circuits (map #(find-circuit % circuits) pair)]
        (recur (rest sorted-distances)
               (merge-circuits pair circuits)
               pair)))))

(defn multiply-last-connection-xs [input]
  (let [boxes (parse-input input)
        distances (find-distances boxes)
        last-pair (make-one-circuit boxes distances)]
    (apply * (map first last-pair))))

;; (time (multiply-last-connection-xs small-input))
;; "Elapsed time: 1.8644 msecs"
;; 25272
;; (time (multiply-last-connection-xs large-input))
;; "Elapsed time: 17219.5612 msecs"
;; 44543856
