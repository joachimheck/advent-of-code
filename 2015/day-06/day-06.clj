(ns day-06.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

;; Part 1
;; Run through a lighting sequence.

(defn parse-line [l]
  (let [[_ inst minx miny maxx maxy]
        (re-matches #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)" l)]
    (list inst [(Long/parseLong minx) (Long/parseLong miny)] [(Long/parseLong maxx) (Long/parseLong maxy)])))

(defn parse-input [f]
  (map
   parse-line
   (str/split-lines (slurp f))))

(defn turn-on [lights [minx miny] [maxx maxy]]
  (apply conj lights
         (for [j (range miny (inc maxy))
               i (range minx (inc maxx))]
           [i j])))

(defn turn-off [lights [minx miny] [maxx maxy]]
  (apply disj lights
         (for [j (range miny (inc maxy))
               i (range minx (inc maxx))]
           [i j])))

(defn toggle [lights [minx miny] [maxx maxy]]
  (let [the-range (set (for [j (range miny (inc maxy))
                             i (range minx (inc maxx))]
                         [i j]))
        lit-in-range (set/intersection the-range lights)
        unlit-in-range (set/difference the-range lights)]
    (set/union unlit-in-range (set/difference lights lit-in-range))))


(parse-input "puzzle-input.txt")



(defn follow-instructions [instructions]
  (let [lights #{}]
    (reduce
     (fn [lights [instruction minpos maxpos]]
       (case instruction
         "turn on" (turn-on lights minpos maxpos)
         "turn off" (turn-off lights minpos maxpos)
         "toggle" (toggle lights minpos maxpos))
       )
     #{}
     instructions)))

;; (time (count (follow-instructions (parse-input "puzzle-input.txt"))))
;; => 400410
;; "Elapsed time: 27007.6199 msecs"




;; Part 2
;; Brightness levels

(defn get-it [lights pos]
  (let [val (get lights pos)]
    (if val val 0)))

(defn brighten [lights amount [minx miny] [maxx maxy]]
  (apply assoc lights
         (apply concat
                (for [j (range miny (inc maxy))
                      i (range minx (inc maxx))]
                  (list [i j] (max 0 (+ amount (get-it lights [i j]))))))))

(defn follow-instructions-2 [instructions]
  (let [lights {}]
    (reduce
     (fn [lights [instruction minpos maxpos]]
       (case instruction
         "turn on" (brighten lights 1 minpos maxpos)
         "turn off" (brighten lights -1 minpos maxpos)
         "toggle" (brighten lights 2 minpos maxpos))
       )
     {}
     instructions)))

;; (time (reduce + (vals (follow-instructions-2 (parse-input "puzzle-input.txt")))))
;; => 15343601
;; "Elapsed time: 41409.8019 msecs"
