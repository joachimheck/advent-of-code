(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 23: Amphipod

;; Part 1
;; What's the least energy required to organize the amphipods?
(defn simple-neighbors [[x y]]
  (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]))

(defn find-neighbors [spaces]
  (reduce (fn [acc space]
            (let [neighbors (simple-neighbors space)]
              (assoc acc space (filter spaces neighbors))))
          {}
          spaces))

(defn parse-input [f]
  (let [grid (->> f
                  read-lines
                  (mapv vec))
        width (count (first grid))
        height (count grid)
        combo-map (apply merge-with concat (for [j (range height)
                                                 i (range width)
                                                 c (list (get-in grid [j i]))
                                                 :when (not= c \#)]
                                             (if (= c \.)
                                               {:spaces (list [i j])}
                                               (assoc {:spaces (list [i j])} [i j] c))))
        positions (dissoc combo-map :spaces)
        spaces (set (get combo-map :spaces))]
    {:spaces (find-neighbors spaces)
     :positions positions
     :rooms {\A #{[3 2] [3 3]} \B #{[5 2] [5 3]} \C #{[7 2] [7 3]} \D #{[9 2] [9 3]}}
     :room-bottoms #{[3 3] [5 3] [7 3] [9 3]}}))

(defn in-correct-room? [[pos c :as position] state]
  (boolean (some (get (:rooms state) c) (list pos))))

(defn want-to-move? [[[x y :as pos] c :as position] state]
  (not (and (in-correct-room? position state)
            (or (= y 3)
                (= c (get (:positions state) [x (inc y)]))))))

;; (let [state (parse-input small-input)]
;;   (map #(list % (want-to-move? % state)) (:positions state)))
