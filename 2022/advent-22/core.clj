(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; Trace a path to determine the password.
(def directions {:east [1 0] :south [0 1] :west [-1 0] :north [0 -1]})

(defn parse-input [f]
  (let [lines (read-lines f)
        board (reduce (fn [result-j [j line]]
                        (assoc result-j j
                               (reduce (fn [result-i i]
                                         (let [c (subs line i (inc i))] (if (not= c " ") (assoc result-i i c))))
                                       {} (range (count line)))))
                      {}
                      (keep-indexed list (drop-last 2 lines)))
        path (map (fn [c]
                  (if (and (not= c "R") (not= c "L"))
                    (Long/parseLong c)
                    c))
                (let [line (last (read-lines small-input))]
                  (concat (list (second (re-matches #"(\d+).+" line)))
                          (flatten (map #(vector (rest %)) (re-seq #"([RL])(\d+)" (last lines)))))))]
    (assoc {}
           :board board
           :path path
           :start [(apply min (keys (get board 0))) 0 :east]
           :size (reduce min (map count (vals board))))))

(defn get-tile [[x y] board]
  (get (get board y) x))

(defn +vec [v1 v2]
  (mapv + v1 v2))

(defn turn [d t]
  (get (vec (keys directions)) (mod (+ (if (= t "R") 1 -1) (.indexOf (keys directions) d)) (count directions))))

(defn wrap [[x y d :as pos] board]
  (case d
    :east [(apply min (keys (get board y))) y d]
    :west [(apply max (keys (get board y))) y d]
    :south [x (some (fn [[j row]] (if (contains? row x) j)) (sort board)) d]
    :north [x (some (fn [[j row]] (if (contains? row x) j)) (reverse (sort board))) d]))

(defn find-adjacent [[x y d :as pos] board]
  (let [adjacent (+vec [x y] (get directions d))]
    (if (not (nil? (get-tile adjacent board)))
      (list (get-tile adjacent board) (conj adjacent d))
      (let [wrapped (wrap pos board)]
        (list (get-tile wrapped board ) wrapped)))))

(defn move-forward [[x y d :as pos] steps board]
  (loop [pos pos steps steps]
    (if (= steps 0)
      pos
      (let [adjacent (find-adjacent pos board)]
        (if (= "#" (first adjacent))
          pos
          (recur (second adjacent) (dec steps)))))))

(defn follow-path [f]
  (let [{board :board path :path start :start} (parse-input f)]
    (reduce (fn [[x y d :as pos] op]
              (if (or (= op "R")
                      (= op "L"))
                [x y (turn d op)]
                (move-forward pos op board)))
            start
            path)))

(defn compute-password [f]
  (let [[x y d] (follow-path f)]
    (+ (* 1000 (inc y))
       (* 4 (inc x))
       (.indexOf (keys directions) d))))


;; Part 2
;; Whoops, the map is actually a cube!
(def small-cube-faces
  {
   1 {:min [8 0] :max [11 3] :east 6 :south 4 :west 3 :north 2}
   2 {:min [0 4] :max [3 7] :east 3 :south 5 :west 6 :north 1}
   3 {:min [4 4] :max [7 7] :east 4 :south 5 :west 2 :north 1}
   4 {:min [8 4] :max [11 7] :east 6 :south 5 :west 3 :north 1}
   5 {:min [8 8] :max [11 11] :east 6 :south 2 :west 3 :north 4}
   6 {:min [12 8] :max [15 11] :east 1 :south 2 :west 5 :north 4}
   })

(def large-cube-faces
  {
   1 {:min [50 0] :max [99 49] :east 2 :south 3 :west 4 :north 6}
   2 {:min [100 0] :max [149 49] :east 5 :south 3 :west 1 :north 6}
   3 {:min [50 50] :max [99 99] :east 2 :south 5 :west 4 :north 1}
   4 {:min [0 100] :max [49 149] :east 5 :south 6 :west 1 :north 3}
   5 {:min [50 100] :max [99 149] :east 2 :south 6 :west 4 :north 3}
   6 {:min [0 150] :max [49 199] :east 5 :south 2 :west 1 :north 4}
   })

(defn get-face [[x y d :as pos] cube-faces]
  (some (fn [[i {[min-x min-y] :min [max-x max-y] :max}]]
          (if (and (<= min-x x max-x) (<= min-y y max-y)) i))
        cube-faces))

(defn face-edge [face edge board-size]
  ;; (println "face-edge" face edge board-size)
  (let [[min-x min-y] (:min face)
        [max-x max-y] (:max face)
        [start increment] (case edge
                            :east [[max-x min-y] [0 1]]
                            :south [[max-x max-y] [-1 0]]
                            :west [[min-x max-y] [0 -1]]
                            :north [[min-x min-y] [1 0]])]
    ;; (println "taking" board-size "from iterated +vec of" increment "on" start)
    (vec (take board-size (iterate #(+vec % increment) start)))))

(defn get-opposite [d]
  (get (vec (keys directions)) (mod (+ 2 (.indexOf (keys directions) d)) (count directions))))

(defn wrap-cube [[x y d :as pos] cube-faces board-size]
  (let [current-face-num (get-face pos cube-faces)
        current-face (get cube-faces current-face-num)
        current-edge (face-edge current-face d board-size)
        pos-on-face (some (fn [[p [i j]]] (if (and (= i x) (= j y)) p)) (keep-indexed list current-edge))
        wrap-face-num (get (get cube-faces (get-face pos cube-faces)) d)
        wrap-face (get cube-faces wrap-face-num)
        wrap-side (some (fn [[k v]] (if (= v current-face-num) k)) wrap-face)
        wrap-edge (vec (reverse (face-edge wrap-face wrap-side board-size)))
        new-pos (get wrap-edge pos-on-face)
        new-d (get-opposite wrap-side)]
    (conj new-pos new-d)))

(defn find-adjacent-cube [[x y d :as pos] board cube-faces board-size]
  (let [current-face (get-face pos cube-faces)
        adjacent (+vec [x y] (get directions d))
        adjacent-face (get-face adjacent cube-faces)]
    (if (= current-face adjacent-face)
      (list (get-tile adjacent board) (conj adjacent d))
      (let [wrapped (wrap-cube pos cube-faces board-size)]
        ;; (println "wrapped from" current-face pos "to" (get-face wrapped cube-faces) wrapped)
        (list (get-tile wrapped board) wrapped)))))

(defn move-forward-cube [[x y d :as pos] steps board cube-faces board-size]
  (loop [pos pos steps steps]
    (if (<= steps 0)
      pos
      (let [adjacent (find-adjacent-cube pos board cube-faces board-size)]
        (if (= "#" (first adjacent))
          pos
          (recur (second adjacent) (dec steps)))))))

(defn follow-path-cube [f cube-faces]
  (let [{board :board path :path start :start board-size :size} (parse-input f)]
    (reduce (fn [[x y d :as pos] op]
              (if (or (= op "R")
                      (= op "L"))
                [x y (turn d op)]
                (do
                  (let [steps op
                        new-pos (move-forward-cube pos op board cube-faces board-size)]
                    ;; (if (>= steps board-size)
                    ;;  (println "moving forward" steps "steps from" (get-face pos cube-faces) pos "to" (get-face new-pos cube-faces) new-pos))
                    ;; (if (and (>= steps board-size) (not= d (last new-pos))))
                    (move-forward-cube pos op board cube-faces board-size)))))
            start
            path)))

(defn compute-password-cube [f cube-faces]
  (let [[x y d] (follow-path-cube f cube-faces)]
    (+ (* 1000 (inc y))
       (* 4 (inc x))
       (.indexOf (keys directions) d))))

;; 36334
;; 72668
;; -> answer <-
;; 145336

(defn print-board [f]
  (let [{board :board path :path start :start board-size :size} (parse-input f)
        cube-faces (if (= f small-input) small-cube-faces large-cube-faces)
        width (inc (apply max (keys (apply merge (vals board)))))
        height (inc (apply max (keys board)))]
    (for [j (range height)]
      (str/join (map #(if (nil? %) " " %) (for [i (range width)] (get-face [i j :north] cube-faces)))))))


;; The problem was that I had misconfigured the manually-entered board coordinates, even though I checked
;; that more than once! The print-board function made that clear.
;;
;; (time (compute-password-cube small-input small-cube-faces))
;; "Elapsed time: 2.6726 msecs"
;; 5031
;; (time (compute-password-cube large-input large-cube-faces))
;; "Elapsed time: 382.1125 msecs"
;; 115311

