(ns advent-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 9: Smoke Basin

;; Part 1
;; Find the total risk levels of the low points on the map.
(defn parse-input [f]
  (->> (read-lines f)
       (mapv (fn [line] (mapv #(parse-long (str %)) line)))))

(defn neighbors [[x y] heights]
  ;; (println "neighbors" [x y] heights)
  (let [width (count (first heights))
        height (count heights)]
    (filter (fn [[i j]] (and (<= 0 i (dec width))
                             (<= 0 j (dec height))))
            (list [x (dec y)]
                  [(inc x) y]
                  [x (inc y)]
                  [(dec x) y]))))

(defn get-height [[x y] heights]
  (get-in heights [y x]))

(defn get-neighbor-heights [pos heights]
  (map #(get-height % heights) (neighbors pos heights)))

(defn low-point? [pos heights]
  (every? #(< (get-height pos heights) %) (get-neighbor-heights pos heights)))

(defn total-risk [f]
  (let [heights (parse-input f)]
    (apply +
           (for [j (range (count heights))
                 i (range (count (first heights)))]
             (if (low-point? [i j] heights)
               (inc (get-height [i j] heights))
               0)))))

;; (total-risk small-input)
;; 15

;; (total-risk large-input)
;; 465



;; Part 2
;; Find the three largest basins, and multiply their sizes.
(def bold-digits
  {0 (apply str (Character/toChars 0x1d7ce))
   1 (apply str (Character/toChars 0x1d7cf))
   2 (apply str (Character/toChars 0x1d7d0))
   3 (apply str (Character/toChars 0x1d7d1))
   4 (apply str (Character/toChars 0x1d7d2))
   5 (apply str (Character/toChars 0x1d7d3))
   6 (apply str (Character/toChars 0x1d7d4))
   7 (apply str (Character/toChars 0x1d7d5))
   8 (apply str (Character/toChars 0x1d7d6))
   9 (apply str (Character/toChars 0x1d7d7))})

(def double-digits
  {0 (apply str (Character/toChars 0x1d7d8))
   1 (apply str (Character/toChars 0x1d7d9))
   2 (apply str (Character/toChars 0x1d7da))
   3 (apply str (Character/toChars 0x1d7db))
   4 (apply str (Character/toChars 0x1d7dc))
   5 (apply str (Character/toChars 0x1d7dd))
   6 (apply str (Character/toChars 0x1d7de))
   7 (apply str (Character/toChars 0x1d7df))
   8 (apply str (Character/toChars 0x1d7e0))
   9 (apply str (Character/toChars 0x1d7e1))})

(def mono-digits
  {0 (apply str (Character/toChars 0x1d7f6))
   1 (apply str (Character/toChars 0x1d7f7))
   2 (apply str (Character/toChars 0x1d7f8))
   3 (apply str (Character/toChars 0x1d7f9))
   4 (apply str (Character/toChars 0x1d7fa))
   5 (apply str (Character/toChars 0x1d7fb))
   6 (apply str (Character/toChars 0x1d7fc))
   7 (apply str (Character/toChars 0x1d7fd))
   8 (apply str (Character/toChars 0x1d7fe))
   9 (apply str (Character/toChars 0x1d7ff))})

(defn draw-map
  ([heights] (draw-map heights '()))
  ([heights highlights]
   (let [width (count (first heights))
         height (count heights)]
     (str/join
      "\n"
      (for [j (range height)]
        (str/join
         (for [i (range width)]
           (if (some #{[i j]} highlights)
             (bold-digits (get-height [i j] heights))
             (mono-digits (get-height [i j] heights))))))))))

(defn low-points [heights]
  (for [j (range (count heights))
        i (range (count (first heights)))
        :when (low-point? [i j] heights)]
    [i j]))

(defn basin-neighbors [pos heights]
  (filter #(< (get-height % heights) 9) (neighbors pos heights)))

(defn find-basin [pos heights]
  (loop [open-set (apply concat (map #(basin-neighbors % heights) #{pos})) basin #{pos}]
    (if (empty? open-set)
      basin
      (recur (remove basin (apply concat (map #(basin-neighbors % heights) open-set)))
             (apply conj basin open-set)))))

(defn basin-sizes [heights]
  (->> (low-points heights)
       (map #(find-basin % heights))
       (map count)
       sort
       reverse))


;; (let [heights (parse-input small-input)]
;;                   (apply * (take 3 (basin-sizes heights))))
;; 1134

;; (let [heights (parse-input large-input)]
;;                   (apply * (take 3 (basin-sizes heights))))
;; 1269555
