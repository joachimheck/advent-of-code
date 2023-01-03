(ns day-24.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Visit the numbered points in the fewest steps.

(def test-input
  '("###########"
    "#0.1.....2#"
    "#.#######.#"
    "#4.......3#"
    "###########"))

(defn parse-maze [lines]
  (reduce (fn [acc [pos char]]
            (case char
              \. acc
              \# (update acc :walls #(conj % pos))
              \0 (-> acc
                     (assoc :start pos)
                     (update :targets #(assoc % pos (Long/parseLong (str char)))))
              (update acc :targets #(assoc % pos (Long/parseLong (str char))))))
          {:walls #{} :targets {} :start nil}
          (mapcat (fn [j line]
                    (map-indexed
                     (fn [i char]
                       (list [i j] char))
                     line))
                  (range 0 (count test-input))
                  lines)))

(defn neighbors [maze [x y]]
  (remove (:walls maze) (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)])))

(defn find-paths [maze start]
  (let [start-pos (get (set/map-invert (:targets maze)) start)
        target-points (dissoc (:targets maze) start-pos)]
   (loop [{paths :paths visited :visited successes :successes :as state}
          {:paths (list (list start-pos)) :visited #{start-pos} :successes '()}]
     (if (empty? paths) state
         (recur
          (let [shortest-paths (sort-by count paths)
                path (first shortest-paths)
                more-paths (rest shortest-paths)
                endpoint (last path)]
            (if (get target-points endpoint)
              (-> state
                  (update :successes
                          #(concat % (list (list
                                            (sort (list start (get target-points endpoint)))
                                            (dec (count path))
                                            path))))
                  (assoc :paths more-paths))
              (let [nexts (remove visited (neighbors maze endpoint))
                    next-paths (map #(concat path (list %)) nexts)
                    next-visited (into visited nexts)]
                (-> state
                    (assoc :visited next-visited)
                    (assoc :paths (concat more-paths next-paths)))
                ))))))))

(defn get-shortest-paths [maze]
  (let [targets (vals (:targets maze))]
    (map first
         (vals
          (group-by
           first
           (mapcat
            #(get % :successes)
            (for [target targets]
              (find-paths maze target))))))))

(get-shortest-paths (parse-maze test-input))

;; => (((0 1) 2 ([1 1] [2 1] [3 1]))
;;     ((0 4) 2 ([1 1] [1 2] [1 3]))
;;     ((1 2) 6 ([3 1] [4 1] [5 1] [6 1] [7 1] [8 1] [9 1]))
;;     ((2 3) 2 ([9 1] [9 2] [9 3]))
;;     ((3 4) 8 ([1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3] [8 3] [9 3])))

;; 0-1 2
;; 0-4 2
;; 1-2 6
;; 2-3 2
;; 3-4 8


;; 0-1 2
;; 0-1-2 8
;; 0-1-0 4
;; 0-1-2-3 10
;; 0-1-2-1 14
;; 0-1-2-3-4 18 !
;; 0-1-2-3-2 12
;; 0-1-2-1-0 16
;; 0-1-2-1-2 20 X
;; 0-1-2-3-2-3 14
;; 0-1-2-3-2-1 18 X
;; 0-1-2-1-0-1 18 X
;; 0-1-2-1-0-4 18 X
;; 0-1-2-3-2-3-2 16
;; 0-1-2-3-2-3-4 22 X
;; 0-1-2-3-2-3-2-3 18 X
;; 0-1-2-3-2-3-2-1 22 X


;; 0-4 2
