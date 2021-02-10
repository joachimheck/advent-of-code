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
;;    (list p1 p2)
;; ;   (move-data nodes p1 p2)
;; )
;;  nodes
;;  (for [j (reverse (range 0 20))]
;;    (list [28 j] [28 (inc j)])))

;; (move-data nodes [28 19] [28 20])

;; (print-grid (move-data test-nodes [1 0] [1 1]))

;; x28-y20 is empty to start
;; goal node is x31-y0, 64T
;; destination is 0 0
;; moving the space up to y=0 takes 20 steps
;; moving to x30 takes 2 steps
;; it takes 5 steps to move the goal data one to the left
;; moving it to x=1 takes 5 * 30 = 150 steps
;; it takes one step to move it to x=0
;; (+ 20 2 150 1)
;; => 173 Wrong! too low.

;; Moving to the left:
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


;; /dev/grid/node-x0-y0     92T   68T    24T   73%
;; /dev/grid/node-x1-y0     85T   66T    19T   77%
;; /dev/grid/node-x2-y0     94T   73T    21T   77%
;; /dev/grid/node-x3-y0     92T   65T    27T   70%
;; /dev/grid/node-x4-y0     86T   73T    13T   84%
;; /dev/grid/node-x5-y0     88T   69T    19T   78%
;; /dev/grid/node-x6-y0     90T   64T    26T   71%
;; /dev/grid/node-x7-y0     93T   70T    23T   75%
;; /dev/grid/node-x8-y0     90T   67T    23T   74%
;; /dev/grid/node-x9-y0     88T   72T    16T   81%
;; /dev/grid/node-x10-y0    94T   72T    22T   76%
;; /dev/grid/node-x11-y0    87T   67T    20T   77%
;; /dev/grid/node-x12-y0    90T   72T    18T   80%
;; /dev/grid/node-x13-y0    90T   65T    25T   72%
;; /dev/grid/node-x14-y0    89T   71T    18T   79%
;; /dev/grid/node-x15-y0    85T   71T    14T   83%
;; /dev/grid/node-x16-y0    86T   73T    13T   84%
;; /dev/grid/node-x17-y0    94T   72T    22T   76%
;; /dev/grid/node-x18-y0    89T   71T    18T   79%
;; /dev/grid/node-x19-y0    93T   72T    21T   77%
;; /dev/grid/node-x20-y0    86T   64T    22T   74%
;; /dev/grid/node-x21-y0    93T   66T    27T   70%
;; /dev/grid/node-x22-y0    94T   65T    29T   69%
;; /dev/grid/node-x23-y0    90T   68T    22T   75%
;; /dev/grid/node-x24-y0    85T   70T    15T   82%
;; /dev/grid/node-x25-y0    90T   65T    25T   72%
;; /dev/grid/node-x26-y0    85T   73T    12T   85%
;; /dev/grid/node-x27-y0    86T   67T    19T   77%
;; /dev/grid/node-x28-y0    92T   67T    25T   72%
;; /dev/grid/node-x29-y0    85T   67T    18T   78%
;; /dev/grid/node-x30-y0    92T   64T    28T   69%
;; /dev/grid/node-x31-y0    87T   64T    23T   73%

;; /dev/grid/node-x0-y1     90T   68T    22T   75%
;; /dev/grid/node-x1-y1     88T   72T    16T   81%
;; /dev/grid/node-x2-y1     85T   64T    21T   75%
;; /dev/grid/node-x3-y1     93T   66T    27T   70%
;; /dev/grid/node-x4-y1     94T   69T    25T   73%
;; /dev/grid/node-x5-y1     89T   68T    21T   76%
;; /dev/grid/node-x6-y1     89T   67T    22T   75%
;; /dev/grid/node-x7-y1     94T   65T    29T   69%
;; /dev/grid/node-x8-y1     94T   67T    27T   71%
;; /dev/grid/node-x9-y1     89T   65T    24T   73%
;; /dev/grid/node-x10-y1    87T   65T    22T   74%
;; /dev/grid/node-x11-y1    90T   70T    20T   77%
;; /dev/grid/node-x12-y1    91T   68T    23T   74%
;; /dev/grid/node-x13-y1    86T   72T    14T   83%
;; /dev/grid/node-x14-y1    92T   70T    22T   76%
;; /dev/grid/node-x15-y1    85T   67T    18T   78%
;; /dev/grid/node-x16-y1    92T   72T    20T   78%
;; /dev/grid/node-x17-y1    86T   68T    18T   79%
;; /dev/grid/node-x18-y1    87T   67T    20T   77%
;; /dev/grid/node-x19-y1    87T   70T    17T   80%
;; /dev/grid/node-x20-y1    90T   64T    26T   71%
;; /dev/grid/node-x21-y1    91T   69T    22T   75%
;; /dev/grid/node-x22-y1    91T   65T    26T   71%
;; /dev/grid/node-x23-y1    88T   65T    23T   73%
;; /dev/grid/node-x24-y1    88T   65T    23T   73%
;; /dev/grid/node-x25-y1    94T   64T    30T   68%
;; /dev/grid/node-x26-y1    92T   73T    19T   79%
;; /dev/grid/node-x27-y1    92T   73T    19T   79%
;; /dev/grid/node-x28-y1    87T   66T    21T   75%
;; /dev/grid/node-x29-y1    86T   65T    21T   75%
;; /dev/grid/node-x30-y1    88T   70T    18T   79%
;; /dev/grid/node-x31-y1    91T   72T    19T   79%
