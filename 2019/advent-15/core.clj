(ns advent-15.core)

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

;; Day 15: Oxygen System

;; Part 1
;; What is the fewest number of movement commands required to move the repair droid
;; from its starting position to the location of the oxygen system?
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
                (seq outputs)
                (do
                  ;; (println "output!" outputs)
                  {:state new-state :outputs outputs})
                :else
                (recur new-state)))))))

(def directions [:north :south :west :east])
(def statuses {0 :wall 1 :moved 2 :oxygen-system})
(def direction-codes {1 :north 2 :south 3 :west 4 :east})
(def code-directions {:north 1 :south 2 :west 3 :east 4})

(defn move [[x y] direction]
  (case direction
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]
    :north [x (dec y)]))

(defn draw-grid [grid]
  (let [{minx :minx miny :miny maxx :maxx maxy :maxy}
        (reduce (fn [result [[x y] _]]
                  {:minx (min (result :minx) x) :miny (min (result :miny) y) :maxx (max (result :maxx) x) :maxy (max (result :maxy) y)})
                {:minx Long/MAX_VALUE :miny Long/MAX_VALUE :maxx Long/MIN_VALUE :maxy Long/MIN_VALUE}
                grid)]
    (str/join
     "\n"
     (for [j (range miny (inc maxy))]
       (str/join
        (for [i (range minx (inc maxx))]
          (let [v (get grid [i j])]
            (case v
              :empty "."
              :wall "#"
              :oxygen-system "O"
              nil " "))))))))

(defn process-status [grid pos direction status]
  (let [move-pos (move pos direction)
        new-grid (assoc grid move-pos (if (= status :moved) :empty status))
        new-pos (if (= status :wall) pos move-pos)]
    [new-grid new-pos]))

(defn get-direction [[fx fy] [tx ty]]
  (cond (> tx fx) :east
        (> ty fy) :south
        (< tx fx) :west
        (< ty fy) :north))

(defn choose-direction [path grid]
  (let [pos (last path)
        unmapped (first (filter #(nil? (second %)) (map #(list % (get grid (move pos %))) directions)))]
    (if unmapped
      (first unmapped)
      (get-direction (last path) (last (drop-last path))))))

(defn update-path [path new-pos]
  (cond (= new-pos (last path)) path
        (= new-pos (last (drop-last path))) (vec (drop-last path))
        :else (conj path new-pos)))

(defn find-oxygen-system [program pause-after-steps]
  (loop [state {:ip 0 :memory program :inputs [] :outputs [] :relative-base 0}
         grid {[0 0] :empty}
         path [[0 0]]
         direction :north
         steps 0]
    (let [result (advance-program state)
          result-state (:state result)
          status (get statuses (first (:outputs result)))]
      (cond
        status (let [new-state (assoc result-state :outputs [])
                     [new-grid new-pos] (process-status grid (last path) direction status)]
                 (if (= status :oxygen-system)
                   (do (println (draw-grid new-grid))
                       {:path-length (count path)})
                   (recur new-state new-grid (update-path path new-pos) nil steps)))
        (:input-required result) (let [new-direction (choose-direction path grid)]
                                   (if (>= steps pause-after-steps)
                                         (println (draw-grid grid)))
                                       (if (or (< steps pause-after-steps) (= (read-line) " "))
                                         (recur (assoc result-state :inputs [(get code-directions new-direction)])
                                                grid
                                                path
                                                new-direction
                                                (inc steps))
                                         :finished))
        :else (do (println (draw-grid grid))
                  (list :finished-else status grid ))))))

;; (time (find-oxygen-system (parse-input large-input) 5000))
;;      ###         ###             ###
;;      ...        #...             ...
;;    ##.#.         ##.             .#.
;;    ... .           .             . .
;;  ##.## .###########.             . .
;;  ...#.#.............             . .
;;  .###.##############       ##### . .
;; #...#...#.......           ..... . .
;;  ##. .#.#.#####.           .###.#. .
;;  ... .#...#...#.          #... ... .
;;  .## .#####.###.           ##. ### .
;;  .   ... ... ...           ...#... .
;;  .#####. .#. .##   ####### .###.#. .
;; #..... . . . .    #.......#..... O .
;;  ####.#. . .#. ####.#####.###### ##.
;;     #... . ...#.....#...#...#...#...
;;      ####. ### .#####.#.###.#.#. .##
;;        ...#... .....#.#...#... . ...
;;        .###.#.#####.#.###.#####.###.
;;       #.....#.......#...#...........
;;        ####  ######  ### ########## 
;; "Elapsed time: 93.5877 msecs"
;; {:path-length 220}



;; Part 2
;; How many minutes will it take to fill with oxygen?
