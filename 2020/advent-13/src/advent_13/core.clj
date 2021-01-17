(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))
  
(defn- read-bus-data
  "Returns the earliest I can catch the bus, and a vector containing the bus IDs."
  [f]
  (let [lines (read-lines f)]
    (list (Integer/parseInt (first lines))
          (->> (second lines)
               (#(str/split % #","))
               (filter #(not= "x" %))
               (map #(Integer/parseInt %))))))

(def small-example '(939 (7 13 59 31 19)))

;; Part 1
;; Which bus can I take to the airport?
(defn- get-next-bus [t schedule]
  (let [result (keep-indexed (fn [i id] (if (= 0 (mod t id)) i nil)) schedule)]
    (if (seq result) (list t (nth schedule (first result)))
        (get-next-bus (inc t) schedule))))

(defn get-answer [bus-data]
  (let [result (apply get-next-bus bus-data)]
    (println "bus" (second result) "at time" (first result))
    (* (second result) (- (first result) (first bus-data)))))



;; Part 2
;; Find a time when each bus departs one minute after the previous one.
(def small-example-1 '(7 13 \x \x 59 \x 31 19))
(def small-example-2 '(17 \x 13 19))
(def small-example-3 '(67 7 59 61))
(def small-example-4 '(67 \x 7 59 61))
(def small-example-5 '(67 7 \x 59 61))
(def small-example-6 '(1789 37 47 1889))

(defn- matches-sequence [t s]
  (every? identity (map-indexed #(or (= \x %2) (= 0 (mod (+ t %1) %2))) s)))

(defn- get-largest-and-index [s]
  (reverse (apply max-key second (filter #(not= (second %) \x) (map-indexed list s)))))

(defn get-matching-time [t s]
  (if (matches-sequence t s) t
      (get-matching-time (+ t (first s)) s)))

(defn get-matching-time-recur [t s]
  (if (matches-sequence t s) t
      (recur (+ t (first s)) s)))

  
;; get largest number and its index
;; starting at largest number,
;; (matches-sequence (- t index) s)
;; if not, recur with (+ t largest number)

(defn- get-matching-time-recur-largest [largest index t s]
  (if (matches-sequence (- t index) s) (- t index)
      (recur largest index (+ t largest) s)))

(defn get-answer-2 [s]
  (let [li (get-largest-and-index s)
        largest (first li)
        index (second li)]
    (get-matching-time-recur-largest largest index 0 s)))


;; consider numbers in order
;; for each multiple of the first number
;; find a multiple of the first of the rest of the numbers that matches
;;   mul2 - mul1 = difference in positions
;; do the same for the rest of the numbers

;x 17 y 13
(defn- get-multiples [x y]
  (map #(list % (mod (apply - (reverse %)) y))
       (let [reps (max (inc x) (inc y))]
         (map vector
              (map #(* x %) (range 1 reps))
              (map #(* y %) (range 1 reps))))))

(defn- internal-find-matching-multiples [x-mults y-mults diff]
  (println x-mults y-mults diff)
  (let [firstx (first x-mults)
        firsty (first y-mults)
        d (- firsty firstx)]
    (if (= d diff) firstx
        (if (< firstx firsty)
          (internal-find-matching-multiples (rest x-mults) y-mults diff)
          (internal-find-matching-multiples x-mults (rest y-mults) diff)))))

(defn- find-matching-multiples [x y diff]
  (let [x-mults (map #(* x %) (range 1 y))
        y-mults (map #(* y %) (range 1 x))]
    (internal-find-matching-multiples x-mults y-mults diff)))


          
;; 17-13 are off by 2 every 102/221 102+i*221
;; 13-19 are off by 1 every 208/247 208+j*247
;; 3417
;; Total cycle: 468
;;
;; t % 17 = 0
;; t+2 % 13 = 0
;; t+3 % 19 = 0 ; this is my matches-sequence method.
;;
;; i*19 = j*221 + 102 + 3
;; t = init + n*cycle
;; For individual routes, init=0 and cycle=bus id
;; For two combined routes they will have separate values
;; For '(17 \x 13) cycle is 221 (* 17 13) and init is 102
;; Bus 19 must come one cycle after n*221 + 102 + 2 (for x and 13)
;; So Bus 19 starts at t = n*221 + 105
;; Bus 19 also starts at t % 19 = 0

;; solve for t
;; cycle = 221 init = 105 (102?) desired mod = 0 bus-id = 19
(defn- read-bus-data-2
  "Returns a vector containing the bus IDs."
  [f]
  (let [lines (read-lines f)]
    (->> (second lines)
         (#(str/split % #","))
         (map #(if (= "x" %) \x (Integer/parseInt %))))))

(defn- find-cycle-intersection [init cycle bus-id diff n]
  (let [t (+ init (* cycle n))]
;;    (println t "%" bus-id "=" (mod t bus-id))
    (if (= (- bus-id (mod t bus-id)) diff)
      (list t (* cycle bus-id))
      (find-cycle-intersection init cycle bus-id diff (inc n)))))

(defn- find-cycle-intersection-recur [init cycle bus-id diff n]
  (let [t (+ init (* cycle n))]
    (println t "%" bus-id "=" (mod t bus-id) "desired" (- bus-id diff))
    (if (> t (* 3 cycle bus-id)) nil
        (if (= (- bus-id (mod t bus-id)) diff)
          ;;        (if (= (mod t bus-id) diff)
          (list t (* cycle bus-id))
          (recur init cycle bus-id diff (inc n))))))

(defn- find-cycle-intersection-recur-nomod [init cycle bus-id diff n]
  (let [t (+ init (* cycle n))]
    ;; now is t + diff an integer multiple of bus-id?
    (if (integer? (/ (+ t diff) bus-id))
      (list t (* cycle bus-id))
      (recur init cycle bus-id diff (inc n)))))

(defn- get-answer-3 [s]
  (let [indexed (keep-indexed #(when (not= \x %2) (list %1 %2)) s)]
    (first
     (reduce (fn [acc e]
               (print "adding numbers" acc e "===> ")
               (let [t (first acc)
                     cycle (second acc)
                     index (first e)
                     bus-id (second e)]
                 (time (find-cycle-intersection-recur-nomod t cycle bus-id index 0))))
             indexed))))
