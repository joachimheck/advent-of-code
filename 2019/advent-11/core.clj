(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 11: Space Police

;; Part 1
;; How many panels does the robot paint at least once?
(defn parse-line [line]
  (reduce (fn [m [k v]] (assoc m k v)) {} (map-indexed list (load-string (str/join (list "[" line "]"))))))

(defn parse-input [f]
  (parse-line (first (read-lines f))))

(defn pad-array [array]
  (let [remainder (rem (count array) 4)]
    (if (= remainder 0)
      array
      (apply conj array (vec (repeat (- 4 remainder) 0))))))

(defn digits [n]
  (if (< n 10)
    [n]
    (let [quotient (quot n 10)
          remainder (rem n 10)]
      (conj (digits quotient) remainder))))

(defn parse-operation [n]
  (let [n-digits (digits n)
        opcode (if (= (count n-digits) 1)
                 n
                 (+ (* 10 (nth n-digits (- (count n-digits) 2))) (last n-digits)))
        p-modes (vec (reverse (drop-last 2 n-digits)))]
    {:opcode opcode
     :p-modes p-modes}))

(def POSITION 0)
(def IMMEDIATE 1)
(def RELATIVE 2)

(defn get-value [n memory p-mode relative-base]
  (cond (= p-mode POSITION) (let [address n]
                              (if (< address 0)
                                "Negative address"
                                (get memory address 0)))
        (= p-mode IMMEDIATE) n
        (= p-mode RELATIVE) (let [address (+ relative-base n)]
                              (if (< address 0)
                                "Negative address"
                                (get memory address 0)))
        :else "Unknown p-mode"))

(defn get-address [n p-mode relative-base]
  (cond (= p-mode POSITION) n
        (= p-mode RELATIVE) (+ relative-base n)
        :else "Unknown p-mode"))

(defn op-add [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (+ (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-mul [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory
                        (get-address c (get p-modes 2 0) relative-base)
                        (* (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-in [a b c p-modes {:keys [ip memory inputs relative-base] :as state}]
  (assoc state
         :ip (+ ip 2)
         :memory (assoc memory (get-address a (get p-modes 0 0) relative-base) (first inputs))
         :inputs (vec (rest inputs))))

(defn op-out [a b c p-modes {:keys [ip memory outputs relative-base] :as state}]
  (assoc state
         :ip (+ ip 2)
         :outputs (conj outputs (get-value a memory (get p-modes 0 0) relative-base))))

(defn op-jt [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (if (not= 0 (get-value a memory (get p-modes 0 0) relative-base))
               (get-value b memory (get p-modes 1 0) relative-base)
               (+ ip 3))))

(defn op-jf [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (if (= 0 (get-value a memory (get p-modes 0 0) relative-base))
               (get-value b memory (get p-modes 1 0) relative-base)
               (+ ip 3))))

(defn op-lt [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (if (< (get-value a memory (get p-modes 0 0) relative-base)
                               (get-value b memory (get p-modes 1 0) relative-base))
                          1
                          0))))

(defn op-eq [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (if (= (get-value a memory (get p-modes 0 0) relative-base)
                               (get-value b memory (get p-modes 1 0) relative-base))
                          1
                          0))))

(defn op-rb [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 2)
         :relative-base (+ relative-base (get-value a memory (get p-modes 0 0) relative-base))))

(defn op-ex [a b c p-modes state]
  (assoc state :ip -1))

(def ops-by-code {1 op-add
                  2 op-mul
                  3 op-in
                  4 op-out
                  5 op-jt
                  6 op-jf
                  7 op-lt
                  8 op-eq
                  9 op-rb
                  99 op-ex
                  })

(defn do-op [opcode a b c p-modes state]
  ((get ops-by-code opcode) a b c p-modes state))

(def directions [:up :right :down :left])

(defn turn [d turn-d]
  (get directions (mod (+ (.indexOf directions d) (if (= turn-d 1) 1 3)) 4)))

(defn move [[x y] dir]
  (case dir
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

(defn paint-spaceship [program initial-panel-color]
  (loop [state {:ip 0 :memory program :inputs [initial-panel-color] :outputs [] :relative-base 0 :panels {} :position [0 0] :direction :up}]
    ;; (println "memory" (sort (:memory state)))
    ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
    ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
    (let [{:keys [ip memory]} state
          [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)
          {:keys [ip outputs] :as new-state} (do-op opcode a b c p-modes state)]
      (if (= ip -1)
        state
        (if (= (count outputs) 2)
          (let [[color turn-d] outputs
                new-direction (turn (:direction new-state) turn-d)
                new-position (move (:position new-state) new-direction)]
            (recur (-> new-state
                       (assoc :direction new-direction)
                       (assoc :position new-position)
                       (assoc-in [:panels (:position new-state)] color)
                       (assoc :outputs [])
                       (assoc :inputs [(get (:panels new-state) new-position 0)]))))
          (recur new-state))))))

;; Wrong answers: 10 3

(defn draw-panels [{:keys [panels position direction]}]
  (let [cursor (case direction :up "^" :right ">" :down "v" :left "<")
        [minx miny maxx maxy] (reduce (fn [[minx miny maxx maxy] [x y]]
                                        [(min minx x)
                                         (min miny y)
                                         (max maxx x)
                                         (max maxy y)])
                                      (concat position position)
                                      (keys panels))]
    (str/join "\n"
              (for [j (range miny (inc maxy))]
                (str/join
                 (for [i (range minx (inc maxx))]
                   (if (= [i j] position)
                     cursor
                     (case (get panels [i j])
                       0 "."
                       1 "#"
                       nil " "))))))))

;; (time (count (:panels (paint-spaceship (parse-input large-input) 0))))
;; "Elapsed time: 595.0418 msecs"
;; 2478



;; Part 2
;; Start on a white panel. What identifier is painted?

;; (time (println (draw-panels (paint-spaceship (parse-input large-input) 1))))
;; .#..#..##..####.###..#..#..##...##..####.. 
;;  #..#.#..#....#.#..#.#..#.#..#.#..#....#...
;;  ####.#......#..#..#.#..#.#....#..#...#....
;; .#..#.#.....#...###..#..#.#.##.####..#.... 
;; .#..#.#..#.#....#.#..#..#.#..#.#..#.#....> 
;;  #..#..##..####.#..#..##...###.#..#.####.  
;; "Elapsed time: 65.4612 msecs"
