(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 5: A Maze of Twisty Trampolines, All Alike

;; Part 1
;; How many jumps does it take to exit the maze?
(defn parse-input [f]
  (mapv parse-long (read-lines f)))

(defn process-jumps [maze]
  (loop [maze maze ip 0 steps 0]
    (if (or (>= ip (count maze))
            (< ip 0))
      {:steps steps}
      (recur (update maze ip inc)
             (+ ip (get maze ip))
             (inc steps)))))

;; (time (process-jumps (parse-input small-input)))
;; "Elapsed time: 0.4437 msecs"
;; {:steps 5}

;; (time (process-jumps (parse-input large-input)))
;; "Elapsed time: 294.5251 msecs"
;; {:steps 381680}



;; Part 2
;; If offset was three or more, decrease it by one.
(defn process-jumps-2 [maze]
  (loop [maze maze ip 0 steps 0]
    (if (or (>= ip (count maze))
            (< ip 0))
      {:steps steps}
      (recur (update maze ip #(if (>= % 3) (dec %) (inc %)))
             (+ ip (get maze ip))
             (inc steps)))))

;; (time (process-jumps-2 (parse-input small-input)))
;; "Elapsed time: 0.4737 msecs"
;; {:steps 10}

;; (time (process-jumps-2 (parse-input large-input)))
;; "Elapsed time: 22477.9202 msecs"
;; {:steps 29717847}
