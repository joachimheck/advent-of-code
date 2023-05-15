(ns advent-19.core)

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

;; Day 19: Tractor Beam

;; Part 1
;; How many points are affected by the tractor beam in the 50x50 area closest to the emitter?
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

(defn affected-points [program maxx maxy]
  (for [i (range maxx)
        j (range maxy)
        :when (= 1 (first (run-program program [i j])))]
    [i j]))

;; (time (count (affected-points (parse-input large-input) 50 50)))
;; "Elapsed time: 3525.9961 msecs"
;; 169



;; Part 2
;; Find the closest 100x100 square that fits in the beam.

(defn draw-beam [points]
  (let [[maxx maxy] (reduce (fn [[maxx maxy] [x y]]
                              [(max maxx x) (max maxy y)])
                            [0 0]
                            points)]
    (str/join "\n"
              (for [j (range (inc maxy))]
                (str/join
                 (for [i (range (inc maxx))]
                   (if (some #{[i j]} points) "#" ".")))))))


(defn affected-points-optimized [program maxx maxy]
  (loop [y 0
         prevx 0
         points []]
    (if (= y maxy)
      points
      (let [[line-points newx] (loop [x prevx
                                      newx nil
                                      line-points []
                                      started false]
                                 (let [point (first (run-program program [x y]))]
                                   (if (or (and started (= point 0))
                                           (and (not started) (= x (+ prevx 5))))
                                     [line-points newx]
                                     (recur (inc x)
                                            (if (and (nil? newx) (= point 1)) x newx)
                                            (if (= point 1) (conj line-points [x y]) line-points)
                                            (if (and (not started) (= point 1)) true started)))))]
           (recur (inc y) (if (nil? newx) prevx newx) (apply conj points line-points))))))

;; (time (count (affected-points-optimized (parse-input large-input) 50 50)))
;; "Elapsed time: 392.9647 msecs"
;; 169

(defn beam-line [program y prev-start]
  (loop [x prev-start
         start nil
         end nil]
    (if (or end (and (nil? start) (= x (+ prev-start 5))))
      [y [start end]]
      (let [point (first (run-program program [x y]))
            new-start (if (and (nil? start) (= point 1)) x start)
            new-end (if (and start (nil? end) (= point 0)) (dec x) end)]
        (recur (inc x) new-start new-end)))))

(defn beam-lines [program maxx maxy]
  (into {}
        (loop [y 0
               prev-start 0
               lines []]
          (if (= y maxy)
            lines
            (let [[line-y [start end] :as line] (beam-line program y prev-start)]
              (recur (inc y) (if start start prev-start) (conj lines line)))))))

(defn count-affected-points [lines]
  (reduce (fn [total [_ [start end]]] (+ total (if (nil? start) 0 (inc (- end start)))))
          0
          lines))

;; (time (count-affected-points (beam-lines (parse-input large-input) 50 50)))
;; "Elapsed time: 386.4639 msecs"
;; 169

(defn draw-beam-lines [lines]
  (let [[maxx maxy] (reduce (fn [[maxx maxy] [y [_ x]]] [(max maxx (if (nil? x) 0 x)) (max maxy y)])
                            [0 0]
                            lines)]
    (str/join "\n"
              (for [j (range (inc maxy))]
                (let [[start end] (get lines j)
                      start (if (nil? start) -1 start)
                      end (if (nil? end) -1 end)]
                 (str/join
                  (for [i (range (inc maxx))]
                    (if (<= start i end) "#" "."))))))))

;; Map the beam by lines: [y [x-start x-end]]. Find x,y such that line y-100 has x-end >= x+100.
(defn find-square [program dimension]
  (loop [y 0
         lines {}
         prev-start 0]
    (let [[_ [start end]] (beam-line program y prev-start)
          top-y (inc (- y dimension))
          [top-start top-end] (get lines top-y)]
      ;; (if (= y 29) (println "y=" y "top-end" top-end "start" start "diff+" (inc (- top-end start)) "dimension" dimension))
      (if (and top-end start (>= (inc (- top-end start)) dimension))
        [start top-y]
        (recur (inc y)
               (assoc lines y [start end])
               (if start start prev-start))))))

(defn compute-result [[x y]]
  (+ y (* 10000 x)))

;; (time (compute-result (find-square (parse-input large-input) 100)))
;; "Elapsed time: 157037.2678 msecs"
;; 7001134
