(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 13: Mine Cart Madness

;; Part 1
;; What is the location of the first crash?
(defn find-neighbors [[x y] points]
  (case (get points [x y])
    \- (list [(dec x) y] [(inc x) y])
    \| (list [x (dec y)] [x (inc y)])
    \/ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])
    \\ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])
    \+ (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])))

(defn find-next-point [[x y :as p] [prev-x prev-y :as prev] points]
  (let [neighbors (find-neighbors p points)
        c (get points p)]
    (if (nil? prev)
      (first neighbors)
      (cond
        (= c \+) [(+ x (- x prev-x)) (+ y (- y prev-y))] ; The opposite side
        (= c \/) (case [(- x prev-x) (- y prev-y)]
                   [1 0] [x (dec y)]
                   [-1 0] [x (inc y)]
                   [0 1] [(dec x) y]
                   [0 -1] [(inc x) y])
        (= c \\) (case [(- x prev-x) (- y prev-y)]
                   [1 0] [x (inc y)]
                   [-1 0] [x (dec y)]
                   [0 1] [(inc x) y]
                   [0 -1] [(dec x) y])
        :else (first (remove #(= prev %) neighbors))))))

(defn find-path [points]
  (loop [path [(first (first (filter (fn [[p c]] (= c \-)) points)))]
         points points]
    (let [[x y :as p] (last path)
          c (get points p)
          [next-x next-y :as next-p] (find-next-point p (if (> (count path) 1) (last (drop-last path))) points)
          next-point [next-p (get points next-p)]
          new-points (let [prev-p (if (> (count path) 2) (last (drop-last (drop-last path))))]
                       (if (and (= \+ c) (not (empty? (remove #{prev-p next-p} (find-neighbors p points)))))
                         (assoc points [x y] (if (= x next-x) \- \|))
                         (dissoc points [x y])))]
      (if (some #{next-p} path)
        (list path new-points)
        (recur (conj path next-p) new-points)))))

(defn find-paths [points]
  (loop [points points paths '()]
    (if (empty? points)
      paths
      (let [[path new-points] (find-path points)]
        (recur new-points (conj paths path))))))

(defn parse-input [f]
  (let [vectorized (->> f
                        (read-lines)
                        (map #(str/replace % #" +$" ""))
                        (mapv vec))
        mapped (into {}
                     (apply concat
                            (for [j (range (count vectorized))
                                  :let [line (get vectorized j)]
                                  i (range (count line))
                                  :let [c (get line i)]
                                  :when (not= \space c)]
                              {[i j] c})))
        carts (filter (fn [[k v]] (or (= v \>) (= v \<) (= v \^) (= v \v))) mapped)
        without-carts (reduce (fn [acc [k v]]
                                (case v
                                  \> (assoc acc k \-)
                                  \< (assoc acc k \-)
                                  \^ (assoc acc k \|)
                                  \v (assoc acc k \|)
                                  acc))
                              mapped
                              mapped)
        ;; paths (find-paths without-carts)
        ]
    {:points without-carts
     ;; :paths paths
     :carts (map (fn [[p c]] [p c :left]) carts)}))

;; TODO: remember to turn the carts at each intersection.
(def cart-symbols [\^ \> \v \<])

(def turn-directions [:left :straight :right])

(defn turn [map-symbol cart-symbol direction]
  (case map-symbol
    \| cart-symbol
    \- cart-symbol
    \/ (case cart-symbol
         \^ \>
         \> \^
         \v \<
         \< \v)
    \\ (case cart-symbol
         \^ \<
         \> \v
         \v \>
         \< \^)
    \+ (let [diff (case direction
                    :left -1
                    :straight 0
                    :right 1)]
         (get cart-symbols (mod (+ (.indexOf cart-symbols cart-symbol) diff) (count cart-symbols))))))

(defn move-cart-once [[[x y] c t :as cart] points]
  (let [new-position (case c
                       \> [(inc x) y]
                       \< [(dec x) y]
                       \v [x (inc y)]
                       \^ [x (dec y)])
        map-symbol (get points new-position)
        new-symbol (turn map-symbol c t)
        new-next-turn (if (= \+ map-symbol) (get turn-directions (mod (inc (.indexOf turn-directions t)) (count turn-directions))) t)]
    (list new-position new-symbol new-next-turn)))

(defn order-carts [carts]
  (let [maxx (apply max (map (fn [[[x _] _ _]] x) carts))]
    ;;(sort-by (fn [[[x y] _ _]] (+ x (* (inc maxx) y))) carts)
    (sort-by (fn [[[x y] _ _]] [y x]) carts)
    ))

(defn move-carts-until-crash [initial-state]
  (let [points (:points initial-state)]
    (loop [carts (:carts initial-state)]
      (let [positions (map (fn [[[x y] _ _]] [x y]) carts)
            collisions (map first (filter (fn [[p c]] (> c 1)) (frequencies positions)))]
        (if (> (count collisions) 0)
          collisions
          (recur (map #(move-cart-once % points) (order-carts carts))))))))

;; (time (move-carts-until-crash (parse-input small-input)))
;; "Elapsed time: 2.9719 msecs"
;; ([7 3])

;; (time (move-carts-until-crash (parse-input large-input)))
;; "Elapsed time: 1739.3558 msecs"
;; ([53 133])



;; Part 2
;; Remove crashing carts.
(def small-input-2 "small-input-2.txt")

(defn remove-carts-at [carts positions]
  (remove (fn [[p c t]] (some #{p} positions)) carts))

(defn move-carts-once-removing-crashed [initial-carts points]
  (loop [i 0 carts initial-carts]
    (let [carts (vec carts)
          cart (get carts i)]
      ;; (println "loop cart" i "=" cart "carts" carts)
      (if (= i (count initial-carts))
        (remove nil? carts)
        (if (nil? cart)
          (recur (inc i) carts)
          (let [moved (move-cart-once cart points)
                with-one-moved (assoc carts i moved)
                new-pos (first moved)
                carts-with-new-pos (filter (fn [[p _ _]] (= p new-pos)) with-one-moved)]
            ;; (println "number of carts with position" new-pos (count carts-with-new-pos))
            (recur (inc i) (if (= (count carts-with-new-pos) 1)
                             with-one-moved
                             (do (map (fn [[p _ _ :as cart]] (if (not= p new-pos) cart)) with-one-moved))))))))))

(defn move-carts-removing-crashed [initial-state]
  (let [points (:points initial-state)]
    (loop [carts (:carts initial-state)]
      ;; (println "carts" (order-carts carts))
      (let [moved-carts (move-carts-once-removing-crashed (order-carts carts) points)]
        (if (= 1 (count moved-carts))
          (first moved-carts)
          (recur moved-carts))))))

;; incorrect (([110 21] \< :left)) "110,21"
;; incorrect "109,21"

(def large-input-2 "large-input-2.txt")
;; part 1: 83,49
;; part 2: 73,36


;; A reddit commenter clued me into the idea that the crashing carts need to be removed
;; just as they crash, not after all the carts have moved once, as I was originally doing.

;; (time (move-carts-removing-crashed (parse-input small-input-2)))
;; "Elapsed time: 0.933 msecs"
;; ([6 4] \^ :left)

;; (time (move-carts-removing-crashed (parse-input large-input)))
;; "Elapsed time: 1872.758 msecs"
;; ([111 68] \v :straight)
