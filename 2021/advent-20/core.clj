(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 20: Trench Map

;; Part 1
;; How many pixels are lit in the doubly-enhanced image.
(defn parse-input [f]
  (let [[[algorithm] _ image-lines] (partition-by #{""} (read-lines f))]
    {:algorithm algorithm
     :image (mapv #(into [] %) image-lines)
     :default \.
     :defaults [\. (get algorithm 0)]}))

(defn get-point [image [x y] default]
  (get-in image [y x] default))

(defn neighbors [[x y]]
  (list [(dec x) (dec y)]
        [x (dec y)]
        [(inc x) (dec y)]
        [(dec x) y]
        [x y]
        [(inc x) y]
        [(dec x) (inc y)]
        [x (inc y)]
        [(inc x) (inc y)]))

(defn draw-image [image]
  (let [width (count (first image))
        height (count image)]
   (str/join
    "\n"
    (for [j (range height)]
      (str/join
       (for [i (range width)]
         (get-in image [j i])))))))

(defn enhance-image [{algorithm :algorithm image :image default :default :as input}]
  (let [width (count (first image))
        height (count image)]
    (assoc input
           :image
           (vec
            (for [j (range -1 (+ 1 height))]
              (vec
               (for [i (range -1 (+ 1 width))]
                 (->> (neighbors [i j])
                      (map #(get-point image % default))
                      (map #(if (= % \#) 1 0))
                      (map str)
                      str/join
                      ((fn [s] (Long/parseLong s 2)))
                      (get algorithm))))))
           :default (if (= default (get (input :defaults) 0))
                      (get (input :defaults) 1)
                      (get (input :defaults) 0)))))

(defn enhance-n [f n]
  (let [{algorithm :algorithm image :image :as input} (parse-input f)]
    (:image (nth (iterate enhance-image input) n))))

(defn count-pixels [image c]
  (count (filter #{c} (apply concat image))))

;; (time (count-pixels (enhance-n small-input 2) \#))
;; "Elapsed time: 3.8525 msecs"
;; 35

;; (time (count-pixels (enhance-n large-input 2) \#))
;; "Elapsed time: 233.6117 msecs"
;; 5583



;; Part 2
;; What about if you enhanc 50 times?

;; (time (count-pixels (enhance-n small-input 50) \#))
;; "Elapsed time: 2035.0646 msecs"
;; 3351

;; (time (count-pixels (enhance-n large-input 50) \#))
;; "Elapsed time: 12048.7115 msecs"
;; 19592
