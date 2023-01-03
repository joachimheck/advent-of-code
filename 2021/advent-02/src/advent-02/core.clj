(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Compute final position based on course.
(defn vec+ [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn vec* [[a b] n]
  [(* a n) (* b n)])

(defn process-command [command]
  (let [[op n] (str/split command #" ")
        move-direction (cond (= op "forward") [1 0]
                             (= op "down") [0 1]
                             (= op "up") [0 -1])
        move-vector (vec* move-direction (Long/parseLong n))]
    move-vector))

(defn process-course [f]
  (reduce vec+ (map process-command (read-lines f))))

;; (apply * (process-course small-input))
;; 150
;; (apply * (process-course large-input))
;; 1815044



;; Part 2
;; Up and down affect "aim", not depth.
(defn process-aim-command [[x y aim :as state] command]
  (let [[op n-str] (str/split command #" ")
        n (Long/parseLong n-str)]
    (cond (= op "forward") [(+ x n) (+ y (* aim n)) aim]
          (= op "down") [x y (+ aim n)]
          (= op "up") [x y (- aim n)])))

(defn process-course-with-aim [f]
  (reduce process-aim-command [0 0 0] (read-lines f)))

;; (apply * (take 2 (process-course-with-aim small-input)))
;; 900
;; (apply * (take 2 (process-course-with-aim large-input)))
;; 1739283308
