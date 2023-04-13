(ns advent-13.core)

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

;; Day 13: Care Package

;; Part 1
;; How many block tiles are on the screen when the game exits?
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

(defn build-grid [outputs]
  (into {} (map (fn [[a b c]] [[a b] c]) (partition 3 outputs))))

(defn count-block-tiles [program]
  (count (filter #(= 2 (second %)) (build-grid (run-program program [])))))

;; ---> answer <---
;; 840
;; 2520


;; Duh, count the block tiles, not all the tiles.
;; (count-block-tiles (parse-input large-input))
;; 242



;; Part 2
;; Beat the game by breaking all the blocks. What is your score after the last block is broken?
(defn draw-grid [grid]
  (println "Score" (get grid [-1 0]))
  (let [positions (keys grid)
        maxx (apply max (map first positions))
        maxy (apply max (map second positions))]
    (str/join "\n"
              (for [y (range (inc maxy))]
                (str/join
                 (for [x (range (inc maxx))]
                   (case (get grid [x y])
                     0 " "
                     1 "W"
                     2 "B"
                     3 "P"
                     4 "O")))))))

(defn run-game [program inputs]
  (let [outputs (run-program (assoc program 0 2) (apply conj (vec inputs) (repeat 100 0)))
        grid (build-grid outputs)]
    (println (filter #(= 4 (last %)) (partition 3 outputs)))
    (println (draw-grid grid))))


;; Score nil
;; WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
;; W                                      W
;; W B BB  B  BBBBBB   BBBBBB BBB B   BBB W
;; W B   B BB B BB B  B  BB BBB BB  B BBB W
;; W BBB   B   B BB BBB B BB    BBBB B B  W
;; W  BB    BBB BBB   BBBBB     BBBB B  B W
;; W B BB   B  B BBBB B   B  B   BBBBB    W
;; W   B BBB BB   B BB   BBBB BBB  BB B   W
;; W BB B B B  B   BB  B   B B    BBB BB  W
;; W      B B BB      B BBB  B B     BB B W
;; W B  BB B BBBB BB  B  B BBB  B BBBB B  W
;; W B B   B B   BBBB BBB   B   B         W
;; W  BBBBBB B  BBB  B    BB B  B  BB BBB W
;; W   B BBB B  B BBBBBBBB  B   BB BBB    W
;; W B BB BB    B B BBBBB B B B BBB BBBB  W
;; W                                      W
;; W                 O                    W
;; W                                      W
;; W                                      W
;; W                   P                  W
;; W                                      W

;; (run-game (parse-input large-input) [0 0 0 1 1 1 1 1 1])
;; TODO: why is the score still zero after this?
;; TODO: predict where the ball will intersect the x axis and get the paddle there.
