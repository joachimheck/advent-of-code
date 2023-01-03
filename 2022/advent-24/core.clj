(ns advent-24.core)

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
;; Find a path through a valley filled with moving blizzards.
(defn parse-input [f]
  (let [lines (read-lines f)
        width (- (count (first lines)) 2)
        height (- (count lines) 2)]
    {:width width
     :height height
     :board (as-> lines ls
              (subvec ls 1 (inc height))
              (map vec ls)
              (mapv #(subvec % 1 (inc width)) ls)
              (map #(keep-indexed list %) ls)
              (keep-indexed list ls)
              (map (fn [line] (map (fn [x] (list (first x) (first line) (second x))) (second line))) ls)
              (apply concat ls)
              (reduce (fn [result [x y c]] (if (= c \.) result (assoc result [x y] (list c)))) {} ls))}))


(defn draw-board
  ([state] (draw-board state nil))
  ([state pos]
   (let [board (state :board)]
     (str/join
      "\n"
      (concat
       (list (str/join (concat (if (= pos [0 -1]) '("#E") '("#.")) (repeat (state :width) "#"))))
       (for [j (range (state :height))]
         (str/join
          (concat
           '("#")
           (for [i (range (state :width))]
             (if (= [i j] pos)
               "E"
               (let [contents (get board [i j])]
                 (cond (= contents nil) "."
                       (= contents '()) "."
                       (= contents \#) "#"
                       (> (count contents) 1) (count contents)
                       :else (str (first contents))))))
           '("#"))))
       (list (str/join (concat (repeat (state :width) "#") '(".#")))))))))

(defn get-destination [[x y] width height blizzard]
  (let [[raw-x raw-y] (cond (= blizzard \^) [x (dec y)]
                            (= blizzard \>) [(inc x) y]
                            (= blizzard \v) [x (inc y)]
                            (= blizzard \<) [(dec x) y])]
    [(cond (= raw-x -1) (dec width)
           (= raw-x width) 0
           :else raw-x)
     (cond (= raw-y -1) (dec height)
           (= raw-y height) 0
           :else raw-y)]))

(defn move-board [state]
  (let [board (state :board)
        width (state :width)
        height (state :height)]
    (assoc state
           :board (reduce
                   (fn [result [k v]]
                     (merge-with concat result {(get-destination k width height v) (list v)}))
                   {}
                   (apply concat (map (fn [[k v]] (map #(list k %) v)) board))))))

(defn neighbors [[x y] width height]
  (remove (fn [[x y]] (and (or (< x 0) (< y 0) (>= x width) (>= y height))
                           (not= [x y] [(dec width) height])
                           (not= [x y] [0 -1])))
          (list [x (dec y)]
                [(inc x) y]
                [x (inc y)]
                [(dec x) y]
                [x y])))

(defn free-neighbors [[x y :as pos] {board :board width :width height :height}]
  (remove (fn [pos] (get board pos)) (neighbors pos width height)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn find-path [froms [start-minute start :as start-pair] goal]
  ;; (println "find-path" start goal "froms" (sort-by (fn [[[minute pos] _]] (manhattan-distance pos start)) froms))
  (loop [path (list (first (first (filter (fn [[[minute pos] _]] (= pos goal)) froms))))]
    (let [[minute pos :as head] (first path)]
      (if (or (nil? (first path))
              (= head start-pair))
        path
        (recur (conj path (get froms head)))))))

(def state-cache (atom {}))

(defn get-state-at-minute-memo [initial-state minute]
  (let [product (* (initial-state :width) (initial-state :height))
        k (list product minute)]
    (if-let [v (get @state-cache k)]
      v
      (if (= minute 0)
        initial-state
        (let [ret (move-board (get-state-at-minute-memo initial-state (dec minute)))]
          (swap! state-cache assoc k ret)
          ret)))))

(defn verify-path [initial-state path]
  (map (fn [[minute pos :as node]]
         (list node (get (get (get-state-at-minute-memo initial-state minute) :board) pos))) path))

(defn h-sort [[minute pos] goal]
  (manhattan-distance pos goal))

(defn f-sort [[minute pos] goal]
  (+ minute (manhattan-distance pos goal)))

(defn mostly-h-sort [[minute pos] goal]
  (+ minute (* 50 (manhattan-distance pos goal))))

(defn move-expedition [initial-state max-iterations sort-fn]
  (reset! state-cache {})
  (let [width (initial-state :width)
        height (initial-state :height)
        goal [(dec width) height]
        start (list 0 [0 -1])]
    (loop [open-set #{start}
           froms {}
           f-scores {start (sort-fn start goal)}
           g-scores {start 0}
           iterations 0]
      (cond (empty? open-set)
            :failure-no-open-nodes
            (= iterations max-iterations)
            (list :fail-over-time iterations)
            :else
            ;; (println "loop" open-set froms g-scores iterations)
            ;; (if (not= (count open-set) (count (distinct open-set))) (println "not distinct"))
            ;; (if (= 0 (mod iterations 50)) (println "iteration" iterations))
            (let [[current-minute current-pos :as current] (first (first (sort-by second (map #(list % (get f-scores %)) open-set))))]
              (if (= current-pos goal)
                (let [path (find-path froms start goal)]
                  (list :steps (dec (count path)) :iterations iterations))
                (let [open-set (into #{} (remove #{current} open-set))
                      neighbors (map #(list (inc current-minute) %)
                                     (free-neighbors current-pos (get-state-at-minute-memo initial-state (inc current-minute))))
                      tentative-g-score (inc (get g-scores current))]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                        (reduce (fn [result neighbor]
                                  (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                    {
                                     :froms (assoc (get result :froms) neighbor current)
                                     :g-scores (assoc (get result :g-scores) neighbor tentative-g-score)
                                     :f-scores (assoc (get result :f-scores) neighbor (+ tentative-g-score (sort-fn neighbor goal)))
                                     :open-set (conj (get result :open-set) neighbor)
                                     }
                                    result))
                                {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                                neighbors)]
                    (recur new-open-set new-froms new-f-scores new-g-scores (inc iterations))))))))))

;; Incorrect answers
;; (:finished 29 :minutes)
;; -> answer <-
;; 370
;; 371

;: I rewrote my code to carefully follow the A* algorithm in wikipedia. Now it works! Don't know exactly what was wrong before.
;; (time (let [initial-state (parse-input small-input)] (move-expedition initial-state 50000 h-sort)))
;; "Elapsed time: 4.6915 msecs"
;; (:steps 18 :iterations 31)
;; (time (let [initial-state (parse-input large-input)] (move-expedition initial-state 50000 h-sort)))
;; "Elapsed time: 72975.8411 msecs"
;; (:steps 232 :iterations 38245)



;; Part 2
;; An elf forgot his snacks. Find the quickest path from the start to the goal, then back.

(defn move-expedition-2 [initial-state start goal max-iterations sort-fn]
  (reset! state-cache {})
  (let []
    (loop [open-set #{start}
           froms {}
           f-scores {start (sort-fn start goal)}
           g-scores {start 0}
           iterations 0]
      (cond (empty? open-set)
            :failure-no-open-nodes
            (= iterations max-iterations)
            (list :fail-over-time iterations)
            :else
            ;; (println "loop" open-set froms g-scores iterations)
            ;; (if (not= (count open-set) (count (distinct open-set))) (println "not distinct"))
            ;; (if (= 0 (mod iterations 50)) (println "iteration" iterations))
            (let [[current-minute current-pos :as current] (first (first (sort-by second (map #(list % (get f-scores %)) open-set))))]
              (if (= current-pos goal)
                (let [path (find-path froms start goal)]
                  {:steps (dec (count path)) :iterations iterations :path path})
                (let [open-set (into #{} (remove #{current} open-set))
                      neighbors (map #(list (inc current-minute) %)
                                     (free-neighbors current-pos (get-state-at-minute-memo initial-state (inc current-minute))))
                      tentative-g-score (inc (get g-scores current Integer/MAX_VALUE))]
                  (let [{new-open-set :open-set new-froms :froms new-f-scores :f-scores new-g-scores :g-scores :as reduce-result}
                        (reduce (fn [result neighbor]
                                  (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
                                    {
                                     :froms (assoc (get result :froms) neighbor current)
                                     :g-scores (assoc (get result :g-scores) neighbor tentative-g-score)
                                     :f-scores (assoc (get result :f-scores) neighbor (+ tentative-g-score (sort-fn neighbor goal)))
                                     :open-set (conj (get result :open-set) neighbor)
                                     }
                                    result))
                                {:froms froms :g-scores g-scores :f-scores f-scores :open-set open-set}
                                neighbors)]
                    (recur new-open-set new-froms new-f-scores new-g-scores (inc iterations))))))))))

(defn move-back-and-forth [initial-state max-iterations sort-fn]
  (let [width (initial-state :width)
        height (initial-state :height)
        point-a [0 -1]
        point-b [(dec width) height]]
    (let [first (move-expedition-2 initial-state (list 0 point-a) point-b max-iterations h-sort)
          first-steps (get first :steps)]
      (let [second (move-expedition-2 initial-state (list first-steps point-b) point-a max-iterations h-sort)
            second-steps (get second :steps)]
        (let [third (move-expedition-2 initial-state (list (+ first-steps second-steps) point-a) point-b max-iterations h-sort)
              third-steps (get third :steps)
              steps (list first-steps second-steps third-steps)]
          {:sub-steps steps :total-steps (apply + steps)})))))


;; (time (move-back-and-forth (parse-input small-input) 50000 h-sort))
;; "Elapsed time: 14.5487 msecs"
;; {:sub-steps (18 23 13), :total-steps 54}

;; (time (move-back-and-forth (parse-input large-input) 50000 h-sort))
;; "Elapsed time: 294895.6975 msecs"
;; {:sub-steps (232 255 228), :total-steps 715}
