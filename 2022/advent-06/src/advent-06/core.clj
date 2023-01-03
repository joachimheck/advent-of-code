(ns advent-06.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many characters need to be processed before the first start-of-packet marker is detected?
;; The start-of-packet marker consists of four distinct characters.
(defn n-unique? [s n i]
  (= n (count (distinct (subs s i (+ i n))))))

(defn chars-until-start-of-packet [s]
  (first (filter #(not (nil? %)) (for [i (range 0 (- (count s) 4))]
                             (if (n-unique? s 4 i) (+ i 4) nil)))))

;; (map chars-until-start-of-packet (read-lines small-input))
;; (7 5 6 10 11)
;; (map chars-until-start-of-packet (read-lines large-input))
;; (1578)


;; Part 2
;; How many characters need to be processed before the first start-of-message marker is detected?
;; The start-of-packet marker consists of fourteen distinct characters.
(defn chars-until-start-of-message [s]
  (first (filter #(not (nil? %)) (for [i (range 0 (- (count s) 14))]
                             (if (n-unique? s 14 i) (+ i 14) nil)))))

;; (map chars-until-start-of-message (read-lines small-input))
;; (19 23 23 29 26)
;; (map chars-until-start-of-message (read-lines large-input))
;; (2178)
