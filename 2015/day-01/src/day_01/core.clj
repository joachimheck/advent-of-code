(ns day-01.core)

(def large-input (slurp "resources/large-input.txt"))

;; Part 1
;; Go up and down floors.

(defn floor [input]
  (let [{up-count \( down-count \)} (frequencies input)]
    (-
     (if up-count up-count 0)
     (if down-count down-count 0))))

(time (floor large-input))
;; => 74
;; "Elapsed time: 2.6335 msecs"



;; Part 2
(defn moves-to-enter-basement [input]
  (reduce
   (fn [[acc-idx acc-val] c]
     (let [new-acc (if (= c \() (inc acc-val) (dec acc-val))]
       (if (= new-acc -1) (reduced acc-idx) [(inc acc-idx) new-acc])))
   [1 0]
   input))

;; (time (moves-to-enter-basement large-input))
;; => 1795
;; "Elapsed time: 0.6363 msecs"
