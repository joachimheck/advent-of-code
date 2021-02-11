(ns day-22.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Count viable storage nodes

(defn parse-line [line]
  (let [[_ x y size used avail] (re-matches
                                 #".+-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.+"
                                 line)]
    (assoc {}
           [(Long/parseLong x) (Long/parseLong y)]
           (assoc {}
                  :size (Long/parseLong size)
                  :used (Long/parseLong used)
                  :avail (Long/parseLong avail)))))

(def nodes (apply merge (map parse-line (drop 2 (str/split-lines (slurp "input-22.txt"))))))

;; (count
;;  (for [[apos avals :as a] nodes
;;        [bpos bvals :as b] nodes
;;        :when (> (:used avals) 0)
;;        :when (not= a b)
;;        :when (<= (:used avals) (:avail bvals))]
;;    (list apos bpos)))
;; => 985



;; Part 2
(def test-nodes (apply merge
                       (map parse-line
                            '("/dev/grid/node-x0-y0   10T    8T     2T   80%"
                              "/dev/grid/node-x0-y1   11T    6T     5T   54%"
                              "/dev/grid/node-x0-y2   32T   28T     4T   87%"
                              "/dev/grid/node-x1-y0    9T    7T     2T   77%"
                              "/dev/grid/node-x1-y1    8T    0T     8T    0%"
                              "/dev/grid/node-x1-y2   11T    7T     4T   63%"
                              "/dev/grid/node-x2-y0   10T    6T     4T   60%"
                              "/dev/grid/node-x2-y1    9T    8T     1T   88%"
                              "/dev/grid/node-x2-y2    9T    6T     3T   66%"))))
(defn max-node-x [nodes]
  (first (last (sort-by first (keys nodes)))))

(defn max-node-y [nodes]
  (second (last (sort-by second (keys nodes)))))

(defn move-data [nodes from to]
(println from to)
  (let [maxx (max-node-x nodes)
        maxy (max-node-y nodes)
        from-node (get nodes from)
        to-node (get nodes to)]
    (if (< (:avail to-node) (:used from-node))
      (list :error-too-small nodes)
      (-> nodes
          (assoc from (-> from-node
                          (assoc :used 0)
                          (assoc :avail (:size from-node))))
          (assoc to (-> to-node
                        (assoc :used (:used from-node))
                        (assoc :avail (- (:size to-node) (:used from-node)))))))))

(defn print-grid [nodes]
  (println "")
  (map (comp println str/join)
   (for [j (range 0 (inc (max-node-y nodes)))]
     (for [i (range 0 (inc (max-node-x nodes)))
           :let [node (get nodes [i j])]]
       (format "%3d/%3d" (:used node) (:size node))))))

;; (reduce
;;  (fn [nodes [p1 p2]]
;;    ;; (concat nodes p1 p2)
;;    (let [result (move-data nodes p1 p2)]
;;      (if (= (first result) :error-too-small)
;;        (reduced (print-grid (second result)))
;;        result)))
;;  nodes
;;  (for [j (reverse (range 0 20))]
;;    (list [28 j] [28 (inc j)])))


;; Determined the solution by printing the map and counting:

;; x28-y20 is empty to start
;; goal node is x31-y0, 64T
;; destination is 0 0
;; moving the space left to x=25 takes 3 steps
;; moving the space up to y=0 takes 20 steps
;; moving to x30 takes 5 steps
;; it takes 5 steps to move the goal data one to the left
;; moving it to x=1 takes 5 * 30 = 150 steps
;; it takes one step to move it to x=0
;; (+ 3 20 5 150 1)
;; => 173 Wrong! too low. we need to avoid a line of large nodes.
;; => 179



;; Moving to the left takes 1 step, plus 4 steps
;; of recovery to the initial configuration:
;; . _ G .
;; . . . .
;; . G _ .
;; . . . .
;; . G . .
;; . . _ .
;; . G . .
;; . _ . .
;; . G . .
;; _ . . .
;; _ G . .
;; . . . .
