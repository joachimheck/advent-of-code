(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-actions
  "Returns a vector containing the seat map."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall (line-seq rdr))))

;; Part 1
;; Determine the distance to the ship's ending location.
(defrecord Position [x y facing])

(defn- get-facing [degrees]
  (case degrees
    0 "N"
    90 "E"
    180 "S"
    270 "W"))

(defn- get-heading [facing]
  (case facing
    "N" 0
    "E" 90
    "S" 180
    "W" 270))

(defn- update-facing [^Position p degrees]
  (get-facing (mod (+ degrees (get-heading (:facing p))) 360)))

(defn- move-ship [^Position p action]
  (let [letter (subs action 0 1)
        number (Integer/parseInt (subs action 1))]
    (case letter
      "N" (Position. (:x p) (+ (:y p) number) (:facing p))
      "S" (Position. (:x p) (- (:y p) number) (:facing p))
      "E" (Position. (+ (:x p) number) (:y p) (:facing p))
      "W" (Position. (- (:x p) number) (:y p) (:facing p))
      "L" (Position. (:x p) (:y p) (update-facing p (* -1 number)))
      "R" (Position. (:x p) (:y p) (update-facing p number))
      "F" (move-ship p (str (:facing p) number)))))

(defn navigate [actions]
  (let [start-p (Position. 0 0 "E")
        end-p (reduce #(move-ship %1 %2) start-p actions)]
    (println "End position" end-p)
    (+ (Math/abs (:x end-p)) (Math/abs (:y end-p)))))



;; Part 2
;; Determine the ship's location using waypoints.
(defrecord PosWay [x y w-x w-y])

(defn move-to-waypoint [^PosWay p times]
  (nth (iterate #(assoc % :x (+ (:x %) (:w-x %)) :y (+ (:y %) (:w-y %))) p) times))

(defn rotate-waypoint-90 [^PosWay p]
  (assoc p :w-x (:w-y p) :w-y (* -1 (:w-x p))))

(defn rotate-waypoint [^PosWay p dir amount]
  (let [clockwise-amount (mod (if (= dir "R") amount
                                  (- 360 amount))
                              360)
        rotations (/ clockwise-amount 90)]
    (nth (iterate rotate-waypoint-90 p) rotations)))

(defn- move-using-waypoint [^PosWay p action]
  (let [letter (subs action 0 1)
        number (Integer/parseInt (subs action 1))]
    (case letter
      "N" (assoc p :w-y (+ (:w-y p) number))
      "S" (assoc p :w-y (- (:w-y p) number))
      "E" (assoc p :w-x (+ (:w-x p) number))
      "W" (assoc p :w-x (- (:w-x p) number))
      "L" (rotate-waypoint p letter number)
      "R" (rotate-waypoint p letter number)
      "F" (move-to-waypoint p number))))

(defn navigate-with-waypoint [actions]
  (let [start-p (PosWay. 0 0 10 1)
        end-p (reduce #(move-using-waypoint %1 %2) start-p actions)]
    (println "End position" end-p)
    (+ (Math/abs (:x end-p)) (Math/abs (:y end-p)))))

