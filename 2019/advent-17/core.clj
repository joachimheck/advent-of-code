(ns advent-17.core)

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

;; Day 17: Set and Forget

;; Part 1
;; What is the sum of the alignment parameters for the scaffold intersections?
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
         :memory (assoc memory (get-address a (get p-modes 0 0) relative-base) (* 1 (first inputs)))
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

(defn run-program [program inputs]
  (loop [state {:ip 0 :memory program :inputs inputs :outputs [] :relative-base 0}]
    ;; (println "memory" (sort (:memory state)))
    ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
    ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
    (let [{:keys [ip memory]} state
          [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)
          {:keys [ip outputs] :as new-state} (do-op opcode a b c p-modes state)]
      (if (= ip -1)
        outputs
        (recur new-state)))))

(defn parse-output [output]
  (str/split (str/join (map char output)) #"\n"))


(defn intersection? [[x y] grid]
  (and (= \#
          (get-in grid [y x])
          (get-in grid [(dec y) x])
          (get-in grid [y (inc x)])
          (get-in grid [(inc y) x])
          (get-in grid [y (dec x)]))))

(defn intersections [grid]
  (let [width (count (first grid))
        height (count grid)]
    (map first
         (filter #(true? (last %))
                 (for [i (range width)
                       j (range height)]
                   (list [i j] (intersection? [i j] grid)))))))

(defn sum-intersection-alignment-parameters [grid]
  (apply + (map (fn [[x y]] (* x y)) (intersections grid))))

;; (sum-intersection-alignment-parameters (parse-output (run-program (parse-input large-input) [])))
;; 6520




;; Part 2
;; After visiting every part of the scaffold at least once, how much dust does the vacuum robot report it has collected?
(defn neighbors [[x y]]
  (list [x (dec y)]
        [(inc x) y]
        [x (inc y)]
        [(dec x) y]))

(defn opposite [[x y] [a b]]
  [(- x (- a x)) (- y (- b y))])

(defn get-next-pos [pos prev grid]
  (let [available (filter #(= \# (get-in grid (reverse %)))
                          (remove #{prev} (neighbors pos)))
        opposite-pos (if prev (opposite pos prev) nil)]
    (if (some #{opposite-pos} available)
      opposite-pos
      (first available))))

(defn direction [[x1 y1] [x2 y2]]
  ;; (println "direction" [x1 y1] [x2 y2])
  (cond (< y2 y1) :north
        (> x2 x1) :east
        (> y2 y1) :south
        (< x2 x1) :west))

(defn get-turn [dir new-dir]
  (let [directions [:north :east :south :west :north :east :south :west]
        dir-index (.indexOf directions dir)
        clockwise (.indexOf (drop dir-index directions) new-dir)]
    (case clockwise
      0 :straight
      1 :right
      2 :right
      3 :left)))

(defn move [[x y] dir]
  (case dir
    :north [x (dec y)]
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]))

(defn turn [dir turn]
  (let [directions [:north :east :south :west]
        turns [:left :straight :right]]
    (get directions (mod (+ (.indexOf directions dir) (dec (.indexOf turns turn))) 4))))

(defn create-path [grid]
  (let [start-pos (reverse (first (filter #(> (second %) 0) (map #(list (first %) (.indexOf (second %) "^")) (map-indexed list grid)))))
        start-dir :north]
    (loop [pos start-pos dir start-dir prev-pos nil path []]
      ;; (println "loop" pos dir prev-pos path)
      (let [next-pos (get-next-pos pos prev-pos grid)]
        (if (or (nil? next-pos) (> (count path) 200))
          path
          (let [next-dir (direction pos next-pos)]
            (if (= dir (direction pos next-pos))
              (recur next-pos dir pos (conj (vec (drop-last path)) (inc (last path))))
              (let [next-turn (get-turn dir next-dir)]
                (recur pos (turn dir next-turn) prev-pos (apply conj path [(case next-turn :left \L :right \R) 0]))))))))))

;; (println (create-path (parse-output (run-program (parse-input large-input) []))))
;; [L 12 L 8 R 10 R 10 L 6 L 4 L 12 L 12 L 8 R 10 R 10 L 6 L 4 L 12 R 10 L 8 L 4 R 10 L 6 L 4 L 12 L 12 L 8 R 10 R 10 R 10 L 8 L 4 R 10 L 6 L 4 L 12 R 10 L 8 L 4 R 10]

;; I manually figured this out:
;; A = R 10 L 8 L 4 R 10
;; B = L 6 L 4 L 12
;; C = L 12 L 8 R 10 R 10
;; [C B C B A B C A B A]

(defn asciify [v]
  (concat (map int v) (list 10)))

(defn visit-scaffold [program]
  (let [;; grid (parse-output (run-program (parse-input large-input) []))
        main-routine "C,B,C,B,A,B,C,A,B,A"
        fn-a "R,10,L,8,L,4,R,10"
        fn-b "L,6,L,4,L,12"
        fn-c "L,12,L,8,R,10,R,10"
        video "n"
        inputs (apply concat (map asciify (list main-routine fn-a fn-b fn-c video)))]
    (last (run-program (assoc program 0 2) inputs))))
