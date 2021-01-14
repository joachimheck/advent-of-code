(ns advent-23.core)

(def input-1 [3 8 9 1 2 5 4 6 7])
(def input-2 [2 1 5 6 9 4 7 8 3])

;; Part 1
;; Move cups 100 times

(defn wrap [x]
  (+ 1 (mod (- x 1) 9)))

(defn destination [start skip-set]
  (let [proposed (wrap (- start 1))]
    (if (skip-set proposed) (destination proposed skip-set)
        proposed)))

(defn move-cups [cups]
  (let [current (get cups 0)
        to-move (subvec cups 1 4)
        destination (destination current (set to-move))
        removed (subvec cups 4)
        dest-idx (.indexOf removed destination)
        until-dest (subvec removed 0 dest-idx)
        after-dest (subvec removed (inc dest-idx))
        ]
      (println cups current to-move destination removed until-dest after-dest)
      (into [] (concat (vector destination)
                       to-move
                       after-dest
                       (vector current)
                       until-dest
                       ))))

;;(nth (iterate move-cups input-1) 2)
(move-cups input-1)
(move-cups [2 8 9 1 5 4 6 7 3])
