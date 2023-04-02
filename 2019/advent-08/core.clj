(ns advent-08.core)

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

;; Day 8: Space Image Format

;; Part 1
;; Find the image layer with the fewest 0 digits. What's the number of 1 digits times the number of 2 digits?
(defn parse-input [f width height]
  (->> f
       (read-lines)
       (first)
       (partition 1)
       (flatten)
       (map str)
       (map parse-long)
       (partition (* width height))
       (map #(partition width %))))

(defn count-digit [layer digit]
  (count (filter #{digit} (flatten layer))))

(defn with-fewest-zeroes [layers]
  (first (first (sort-by second (map #(list % (count-digit % 0)) layers)))))

(defn compute-result [layers]
  (let [min-zeroes (with-fewest-zeroes layers)]
    (* (count-digit min-zeroes 1) (count-digit min-zeroes 2))))

;; (compute-result (parse-input small-input 3 2))
;; 1

;; (compute-result (parse-input large-input 25 6))
;; 1935



;; Part 2
;; What's the message in the image?
(def small-input-2 "small-input-2.txt")

(defn get-pixel [layer x y]
  (nth (nth layer y) x))

(defn get-pixel-color [layers x y]
  (first (filter #(not= % 2) (map #(get-pixel % x y) layers))))

(defn draw-image [layers width height]
  (str/join "\n"
   (for [j (range height)]
     (str/join
      (for [i (range width)]
        (case (get-pixel-color layers i j)
          0 " "
          1 "*"))))))

;; (println (draw-image (parse-input small-input-2 2 2) 2 2))
;;  *
;; * 
;; nil

;; (println (draw-image (parse-input large-input 25 6) 25 6))
;;  **  **** *    *  * *    
;; *  * *    *    *  * *    
;; *    ***  *    *  * *    
;; *    *    *    *  * *    
;; *  * *    *    *  * *    
;;  **  *    ****  **  **** 
;; nil
