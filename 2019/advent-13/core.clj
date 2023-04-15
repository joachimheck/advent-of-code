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
    (println "ball" (filter #(= 4 (last %)) (partition 3 outputs)))
    (println "paddle" (filter #(= 3 (last %)) (partition 3 outputs)))
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
;; paddle starts at 20,19
;; ball starts at 18,16, going SE
(defn move [[x y] dir]
  (case dir
    :n [x (dec y)]
    :ne [(inc x) (dec y)]
    :e [(inc x) y]
    :se [(inc x) (inc y)]
    :s [x  (inc y)]
    :sw [(dec x) (inc y)]
    :w [(dec x) y]
    :nw [(dec x) (dec y)]))

(defn predict-next-ball-state [[[x y] dir grid]]
  ;; (println "p-n-b-s" [x y] dir)
  (let [to-n (not= 0 (get grid (move [x y] :n) 0))
        to-ne (not= 0 (get grid (move [x y] :ne) 0))
        to-e (not= 0 (get grid (move [x y] :e) 0))
        to-se (not= 0 (get grid (move [x y] :se) 0))
        to-s (not= 0 (get grid (move [x y] :s) 0))
        to-sw (not= 0 (get grid (move [x y] :sw) 0))
        to-w (not= 0 (get grid (move [x y] :w) 0))
        to-nw (not= 0 (get grid (move [x y] :nw) 0))]
    (case dir
      :ne (cond to-n (predict-next-ball-state [[x y] :se (assoc grid (move [x y] :n) 0)])
                to-e (predict-next-ball-state [[x y] :nw (assoc grid (move [x y] :e) 0)])
                to-ne (predict-next-ball-state [[x y] :sw (assoc grid (move [x y] :ne) 0)])
                :else [(move [x y] :ne) :ne grid])
      :se (cond to-s (predict-next-ball-state [[x y] :ne (assoc grid (move [x y] :s) 0)])
                to-e (predict-next-ball-state [[x y] :sw (assoc grid (move [x y] :e) 0)])
                to-se (predict-next-ball-state [[x y] :nw (assoc grid (move [x y] :se) 0)])
                :else [(move [x y] :se) :se grid])
      :sw (cond to-s (predict-next-ball-state [[x y] :nw (assoc grid (move [x y] :s) 0)])
                to-w (predict-next-ball-state [[x y] :se (assoc grid (move [x y] :w) 0)])
                to-sw (predict-next-ball-state [[x y] :ne (assoc grid (move [x y] :sw) 0)])
                :else [(move [x y] :sw) :sw grid])
      :nw (cond to-n (predict-next-ball-state [[x y] :sw (assoc grid (move [x y] :n) 0)])
                to-w (predict-next-ball-state [[x y] :ne (assoc grid (move [x y] :w) 0)])
                to-nw (predict-next-ball-state [[x y] :se (assoc grid (move [x y] :nw) 0)])
                :else [(move [x y] :nw) :nw grid]))))

(defn predict-x-intercept [[[x y] dir] grid]
  (let [[[ix iy] idir :as intercept] (first (drop-while (fn [[[x y] dir grid]] (or (< y 18) (and (>= y 18) (or (= dir :ne) (= dir :nw)))))
                                                        (take 100 (iterate predict-next-ball-state [[x y] dir grid]))
                                                        ;; (take 100 (iterate (fn [[[x y] dir]] (predict-next-ball-state [x y] dir grid)) [[x y] dir grid]))
))]
    ;; (println "predict-x-intercept" intercept)
    (if ix ix 20)))

(defn advance-program [state]
  (loop [state state]
    ;; (println "memory" (sort (:memory state)))
    ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
    ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
    (let [{:keys [ip memory inputs]} state
          [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)]
      (if (and (= opcode 3) (empty? inputs))
        {:state state :input-required true}
        (let [{:keys [ip outputs] :as new-state} (do-op opcode a b c p-modes state)]
          (cond (= ip -1)
                {:finished true}
                (= (count outputs) 3)
                {:state new-state :outputs outputs}
                :else
                (recur new-state)))))))

(defn add-to-grid [grid [x y b]]
  (assoc grid [x y] b))

(defn determine-ball-state [[[x y] dir] [newx newy]]
  ;; (println "d-b-s" [x y] dir [newx newy])
  [[newx newy] (cond (and (> newx x) (> newy y)) :se
                     (and (> newx x) (< newy y)) :ne
                     (and (< newx x) (> newy y)) :sw
                     (and (< newx x) (< newy y)) :nw)])

(defn play-game [program pause-after-steps]
  (loop [state {:ip 0 :memory (assoc program 0 2) :inputs [] :outputs [] :relative-base 0}
         grid {}
         ball-state [[17 15] :se]
         paddle-pos 20
         score 0
         blocks-left nil
         steps 0]
    (let [result (advance-program state)
          result-state (:state result)
          outputs (:outputs result)
          new-blocks-left (if (= -1 (first outputs)) (count (filter #(= 2 %) (vals grid))) blocks-left)]
      ;; (println "play-game" ball-state "prev-outputs" (:outputs state) "outputs" outputs "state outputs" (:outputs result-state))
      (cond
        ;; (and (= score 856) (= blocks-left 223)) (do (println "Ball" ball-state "intercept" (predict-x-intercept ball-state grid))
        ;;                                             (println (draw-grid grid)))
        (or (:finished result) (and (> score 0) (= new-blocks-left 0))) (do (println (draw-grid grid))
                                                                            {:final-score score
                                                                             :blocks-left blocks-left
                                                                             :ball-state ball-state
                                                                             :steps steps})
        outputs (let [new-state (assoc result-state :outputs [])
                      new-grid (add-to-grid grid outputs)
                      new-ball-state (if (= 4 (last outputs)) (determine-ball-state ball-state outputs) ball-state)
                      new-score (if (= -1 (first outputs)) (last outputs) score)]
                  (if (not= blocks-left new-blocks-left) (println "score" new-score "blocks" new-blocks-left "steps" steps))
                  (recur new-state new-grid new-ball-state paddle-pos new-score new-blocks-left steps))
        (:input-required result) (do (if (>= steps pause-after-steps)
                                       (do
                                         (newline)
                                         (println "Ball state" (drop-last ball-state)
                                                  "next" (drop-last (predict-next-ball-state [(first ball-state) (second ball-state) grid])))
                                         (println "x-intercept" (predict-x-intercept ball-state grid))
                                         (println (draw-grid grid))))
                                     (if (or (< steps pause-after-steps) (= (read-line) " "))
                                       (let [x-intercept (predict-x-intercept ball-state grid)
                                             direction (compare x-intercept paddle-pos)]
                                         (recur (assoc result-state :inputs [direction])
                                                grid
                                                ball-state
                                                (+ paddle-pos direction)
                                                score
                                                new-blocks-left
                                                (inc steps)))
                                       :finished))))))

;; TODO: game fails at step 250.
;; (play-game (parse-input large-input) 240)

