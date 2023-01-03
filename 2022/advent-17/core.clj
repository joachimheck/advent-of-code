(ns advent-17.core)

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
;; How tall is the tower of 2,022 rocks?
(defn parse-input [f]
  (first (read-lines f)))

(def rocks ['([0 0] [1 0] [2 0] [3 0])
            '([1 0] [0 1] [1 1] [2 1] [1 2])
            '([0 0] [1 0] [2 0] [2 1] [2 2])
            '([0 0] [0 1] [0 2] [0 3])
            '([0 0] [1 0] [0 1] [1 1])])

(def floor [[0 -1] [1 -1] [2 -1] [3 -1] [4 -1] [5 -1] [6 -1]])

(defn min-x [points]
  (reduce (fn [min-result [x y]] (min min-result x)) Integer/MAX_VALUE points))

(defn max-x [points]
  (reduce (fn [max-result [x y]] (max max-result x)) -1 points))

(defn min-y [points]
  (reduce (fn [min-result [x y]] (min min-result y)) Integer/MAX_VALUE points))

(defn max-y [points]
  (reduce (fn [max-result [x y]] (max max-result y)) -1 points))

(defn shift-rock
  ([rock [x y] chamber]
   (let [shifted (map (fn [[rx ry]] [(+ x rx) (+ y ry)]) rock)]
     (if (or (> (max-x shifted) 6)
             (< (min-x shifted) 0)
             (some (set shifted) chamber))
       rock
       shifted)))
  ([rock [x y]]
   (let [shifted (map (fn [[rx ry]] [(+ x rx) (+ y ry)]) rock)]
     (if (or (> (max-x shifted) 6)
             (< (min-x shifted) 0))
       rock
       shifted))))

(defn move-rock-with-jet
  ([rock jet chamber]
   (if (= jet \>)
     (shift-rock rock [1 0] chamber)
     (shift-rock rock [-1 0] chamber)))
  ([rock jet]
   (if (= jet \>)
     (shift-rock rock [1 0])
     (shift-rock rock [-1 0]))))

(defn move-rock [rock i input chamber]
  ;; (println "----  " rock)
  (loop [i i rock rock remaining-input (drop i input) chamber chamber]
    (let [rock-over (move-rock-with-jet rock (first remaining-input) chamber)]
      ;; (println i "over" (first remaining-input) rock-over)
          (let [rock-down (shift-rock rock-over [0 -1] chamber)]
            ;; (println i "down  " rock-down)
            (if (= rock-over rock-down)
              (list (apply conj chamber rock-over) (inc i))
              (recur (inc i) rock-down (rest remaining-input) chamber))))))

(defn print-chamber [chamber rock]
  (doseq [j (range (max (max-y chamber) (max-y rock)) (max (min-y chamber) -1) -1)]
    (do
      (print "|")
      (doseq [i (range 0 7)]
        (if (some #{[i j]} chamber)
          (print "#")
          (if (some #{[i j]} rock)
            (print "@")
            (print "."))))
      (println "|")))
  (println "+-------+"))

(defn move-rocks [f max-n]
  (let [input (cycle (parse-input f))]
    (loop [chamber floor moves 0 num-rocks 0]
      ;; (println "rock" num-rocks "move" moves "next moves" (take 10 (drop moves input)))
      (if (= num-rocks max-n)
        (do
          (print-chamber chamber [])
          (inc (max-y chamber)))
        (let [[chamber moves] (move-rock (shift-rock (nth (cycle rocks) num-rocks) [2 (+ 4 (max-y chamber))] chamber) moves input chamber)]
          (recur chamber moves (inc num-rocks)))))))

;; (time (move-rocks small-input 2022))
;; "Elapsed time: 13307.2003 msecs"
;; 3068
;; (time (move-rocks large-input 2022))
;; "Elapsed time: 13248.9485 msecs"
;; 3114



;; Part 2
;; Handle 1,000,000,000,000 rocks.
(defn find-floor [chamber]
  (apply min
         (for [x (range 0 7)]
           (max-y (filter #(= x (first %)) chamber)))))

(defn raise-floor [chamber]
  (let [floor (find-floor chamber)]
    (remove #(< (second %) floor) chamber)))

(defn raise-floor [chamber floor]
  (remove #(< (+ (second %) 10) floor) chamber))

(defn compute-max-heights [max-heights new-points]
  (reduce (fn [max-heights [x y]]
            (if (> y (max-heights x))
              (assoc max-heights x y)
              max-heights))
          max-heights new-points))

(defn move-rock-2 [rock i input chamber]
  ;; (println "----  " rock)
  (let [rock (reduce (fn [rock jet] (shift-rock (move-rock-with-jet rock jet) [0 -1]))
                     rock
                     [(input (mod i (count input))) (input (mod (+ i 1) (count input))) (input (mod (+ i 2) (count input)))])
        i (+ i 3)]
    (loop [i i rock rock chamber chamber]
      ;; (print-chamber chamber rock)
      (let [rock-over (move-rock-with-jet rock (input (mod i (count input))) chamber)]
        ;; (println i "over" (input (mod i (count input))) rock-over)
        (let [rock-down (shift-rock rock-over [0 -1] chamber)]
          ;; (println i "down  " rock-down)
          ;; (print-chamber chamber rock-down)
          (if (= rock-over rock-down)
            (list rock-over (inc i))
            (recur (inc i) rock-down chamber)))))))

(defn move-rocks-and-floor [f max-n]
  (let [input (vec (parse-input f))]
    (loop [chamber floor move 0 rock 0 max-heights (vec (repeat 7 Integer/MIN_VALUE)) repeats #{}]
      ;; (println "rock" rock "move" move "next moves" (subvec input (mod move (count input)) (+ 10 (mod move (count input)))))
      ;; (println move [(mod rock 5) (mod move (count input))] repeats)
      (if (some repeats [[(mod rock 5) (mod move (count input))]])
        (println "repeat!" "rock" (mod rock 5) "input" move "(" (mod move (count input)) ")" (sort repeats)))
      (if (= rock max-n)
        (do
          ;; (print-chamber chamber [])
          (inc (max-y chamber)))
        (let [rock-start (shift-rock (rocks (mod rock 5)) [2 (+ 4 (max-y chamber))] chamber)
              [rock-end move] (move-rock-2 rock-start move input chamber)
              new-chamber (apply conj chamber rock-end)
              new-max-heights (compute-max-heights max-heights rock-end)
              new-chamber (if (> (apply min new-max-heights) (apply min max-heights))
                            (raise-floor new-chamber (apply min new-max-heights))
                            new-chamber)]
          (recur new-chamber move (inc rock) new-max-heights (conj repeats [(mod rock 5) (mod move (count input))])))))))

(defn next-repeat-key [[rock index :as key] input-size repeats]
  (let [the-repeat (get repeats key)]
    (if (nil? the-repeat)
      nil
      [(mod (inc rock) 5) (mod (+ index (:moves the-repeat)) input-size)])))

(defn find-cycle [[rock index :as start-key] input-size repeats]
  (loop [key (next-repeat-key start-key input-size repeats)
         cycle [(assoc (repeats start-key) :key start-key)]]
    (cond
        (nil? key) nil
        (= key start-key) cycle
        :else (recur (next-repeat-key key input-size repeats) (conj cycle (assoc (repeats key) :key key))))))

(defn cycle-data [key input-size repeats rocks-left]
  (let [found-cycle (find-cycle key input-size repeats)]
    (if (not found-cycle)
      nil
      {:rocks (count found-cycle)
       :moves (reduce (fn [total repeat] (+ total (:moves repeat))) 0 found-cycle)
       :height (- (:new-height (last found-cycle)) (:old-height (first found-cycle)))
       :skips (/ rocks-left (count found-cycle))}
      )))

(defn move-rocks-repeats [f max-n]
  (let [input (vec (parse-input f))
        input-size (count input)]
    ;; (println "input size" input-size)
    (loop [chamber floor rock-n 0 move-n 0 max-heights (vec (repeat 7 Integer/MIN_VALUE)) repeats {} additional-height 0]
      ;; (println "loop" rock-n "/" max-n move-n)
      (if (>= rock-n max-n)
        (do
          ;; (println "finished")
          ;; (print-chamber chamber [])
          (list rock-n "/" max-n (inc (max-y chamber)) additional-height (+ (inc (max-y chamber)) additional-height)))
        (let [rock-index (mod rock-n 5)
              input-index (mod move-n input-size)
              key [rock-index input-index]
              rocks-left (- max-n rock-n)]
          (let [cycle-data (cycle-data key input-size repeats rocks-left)]
            (if (and cycle-data (> (cycle-data :skips) 0) (int? (cycle-data :skips)))
              (do
                (println "repeat at" rock-n move-n "skipping" (cycle-data :skips)
                         "cycles of length" (cycle-data :rocks) "height" (cycle-data :height))
                (println "increasing height from"
                         (inc (max-y chamber))
                         "to"
                         (+ (inc (max-y chamber)) additional-height (* (cycle-data :skips) (cycle-data :height))))
                (println "possible height" (+ (inc (max-y chamber)) (* (+ 0 (cycle-data :skips)) (cycle-data :height))))
                ;; (print-chamber chamber [])
                (recur chamber
                       (+ rock-n (* (cycle-data :skips) (cycle-data :rocks)))
                       (+ move-n (* (cycle-data :skips) (cycle-data :moves)))
                       max-heights repeats
                       (+ additional-height (* (cycle-data :skips) (cycle-data :height)))))
              (let [rock-start (shift-rock (rocks rock-index) [2 (+ 4 (max-y chamber))] chamber)
                    [rock-end new-move] (move-rock-2 rock-start move-n input chamber)
                    new-chamber (apply conj chamber rock-end)
                    new-max-heights (compute-max-heights max-heights rock-end)
                    ;; new-chamber (if (> (apply min new-max-heights) (apply min max-heights))
                    ;;               (raise-floor new-chamber (apply min new-max-heights))
                    ;;               new-chamber)
                    ]
                ;; (println "writing repeat" [rock-index input-index] (list (- new-move move-n) (max-y chamber)))
                (recur new-chamber (inc rock-n) new-move new-max-heights
                       (assoc repeats key {:moves (- new-move move-n) :old-height (max-y chamber) :new-height (max-y new-chamber)})
                       additional-height)))))))))


;; (time (move-rocks-repeats small-input 1000000000000))
;; repeat at 50 282 skipping 28571428570 cycles of length 35 height 53
;; increasing height from 78 to 1514285714288
;; possible height 1514285714288
;; "Elapsed time: 10.2958 msecs"
;; (1000000000000 "/" 1000000000000 78 1514285714210 1514285714288)

;; (time (move-rocks-repeats large-input 1000000000000))
;; repeat at 2920 17060 skipping 574712642 cycles of length 1740 height 2681
;; increasing height from 4480 to 1540804597682
;; possible height 1540804597682
;; "Elapsed time: 19383.2775 msecs"
;; (1000000000000 "/" 1000000000000 4480 1540804593202 1540804597682)
