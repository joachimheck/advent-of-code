(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])
(require '[clojure.math.numeric-tower :as math :exclude [abs]])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 11: Radioisotope Thermoelectric Generators

;; Part 1
;; Move microchips to the fourth floor.

(def test-input
  {4 #{}
   3 #{"LG"}
   2 #{"HG"}
   1 #{"E" "HM" "LM"}})

(def real-input
  {4 #{}
   3 #{"CoM" "CmM" "RuM" "PuM"}
   2 #{"CoG" "CmG" "RuG" "PuG"}
   1 #{"E" "PmG" "PmM"}})

(defn print-state [state]
  (println "")
  (for [i (reverse (range 1 5))]
    (let [floor (get state i)]
     (apply println
            (concat (list (str/join (list "F" (str i))))
                    (if (some #{"E"} floor) "E" " ")
                    (remove #{"E"} floor))))))

(defn elevator-floor [state]
  (first
   (for [i (range 1 5)
         :when (some #{"E"} (get state i))]
     i)))

(defn adjacent-floors [floor]
  (cond (= floor 1) '(2)
        (= floor 2) '(1 3)
        (= floor 3) '(2 4)
        (= floor 4) '(3)))

(defn contents [state floor]
  (remove #{"E"} (get state floor)))

(defn microchip? [item] (str/ends-with? item "M"))

(defn generator? [item] (str/ends-with? item "G"))

(defn microchip-pairs [contents]
  (let [microchips (filter microchip? contents)]
    (distinct
     (for [m1 microchips
           m2 microchips
           :when (not= m1 m2)]
       (sort (list m1 m2))))))

(defn generator-pairs [contents]
  (let [generators (filter generator? contents)]
    (distinct
     (for [g1 generators
           g2 generators
           :when (not= g1 g2)]
       (sort (list g1 g2))))))

(defn matching-item [item]
  (let [[_ element type] (re-matches #"(\w+)([G|M])" item)]
    (if (= type "G")
      (str/join (list element "M"))
      (str/join (list element "G")))))

(defn matching-items [contents]
  (distinct
   (for [item (map matching-item contents)
         :when (some #{item} contents)]
     (sort (list item (matching-item item))))))

(defn moveables [contents]
  (concat
   (map list contents)
   (matching-items contents)
   (microchip-pairs contents)
   (generator-pairs contents)))

(defn move [state items start-floor end-floor]
  (-> state
      (update start-floor (fn [floor] (set (remove (set (concat items '("E"))) floor))))
      (update end-floor (fn [floor] (set (concat '("E") (sort (concat items floor))))))))

(defn valid? [state]
  (reduce #(and %1 %2)
          true
          (map
           (fn [floor]
             (or (empty? (filter generator? floor))
                 (every? 
                  (set (filter generator? floor))
                  (map matching-item (filter microchip? floor)))))
           (vals state))))

(defn next-states [state]
 (let [start-floor (elevator-floor state)
       end-floors (adjacent-floors start-floor)
       contents (contents state start-floor)]
   (for [f end-floors
         items (moveables contents)
         :let [new-state (move state items start-floor f)]
         :when (valid? new-state)]
     new-state)))

(defn finished? [state]
  (every? empty? (for [i (range 1 4)] (get state i))))

(defn find-path [[end-node nodes]]
 (loop [path [end-node]]
;; (println path)
   (if (nil? (:parent (last path)))
     (reverse (map :state path))
     (recur (conj path (first (filter #(= (:state %) (:parent (last path))) nodes)))))))

(defn score [{state :state dist :dist}]
  (- dist (reduce (fn [acc [k v]] (+ acc (* k (count v)))) 0 state))
  ;; (+ dist (reduce (fn [acc [k v]] (+ acc (* 10 (inc k) (count v)))) 0 state))
  )

(defn min-scoring-node [nodes]
  (reduce
   (fn [best node]
     (if (< (score node) (score best)) node best))
   nodes))

(defn split-item [item]
  (if (= item "E")
    ["E" "E"]
    (let [[_ atom type] (re-matches #"(.+)([G|M])" item)]
      [atom type])))

;; (defn state-key [state]
;;   (->> state
;;        (mapcat (fn [[floor items]]
;;                  (map (fn [item] [floor (split-item item)]) items)))
;;        (reduce
;;         (fn [[atom-numbers item-floors] [floor [atom type]]]
;;           (let [next-num (if (vals atom-numbers) (inc (apply max (vals atom-numbers))) 0)
;;                 atom-num (if (get atom-numbers atom) (get atom-numbers atom) next-num)
;;                 floors (if (get item-floors atom-num) (get item-floors atom-num) {})]
;;             (list
;;              (assoc atom-numbers atom atom-num)
;;              (update item-floors atom-num #(assoc % type floor)))))
;;         ({} {}))
;;        second))

(defn state-key [state]
  (+
   (apply + (for [i (range 4)]
              (* (math/expt 10 i) (count (remove #{"E"} (get state (inc i)))))))
   (* 10000 (first (for [[k v] state
                         :when (contains? v "E")]
                     k)))))

(deftest test-state-key
  (is (= (state-key {4 #{"HM"}, 3 #{"HG" "E" "LM" "LG"}, 2 #{}, 1 #{}}) (state-key {4 #{"LM"}, 3 #{"HG" "E" "LG" "HM"}, 2 #{}, 1 #{}}))))


(defn ->node [state parent]
  (assoc {} :state state :parent (:state parent) :dist (inc (:dist parent)) :key (state-key state)))

(defn search-with-a* [start-state]
  (loop [unvisited (list {:state start-state :parent nil :dist 0})
         visited '()]
    (if (empty? unvisited)
      (list :gave-up (count visited))
      (let [node (min-scoring-node unvisited)
            state (:state node)
            unvisited-next-states (remove (set (map :state unvisited))
                                          (remove (set (map :state visited)) (next-states state)))
            next-nodes (map #(assoc {} :state % :parent state :dist (inc (:dist node))) unvisited-next-states)
            new-visited (concat visited (list node))
            new-unvisited (concat (remove #{node} unvisited) next-nodes)]
        (if (finished? state)
          (list node visited)
          (recur new-unvisited new-visited))))))

(defn next-nodes [node known-nodes]
  (let [state (:state node)
        raw-nexts (map #(->node % node) (next-states state))
        known-keys (set (map :key known-nodes))
        unvisited-next-nodes (remove (fn [node] (known-keys (:key node))) raw-nexts)]
    unvisited-next-nodes))

(defn search-with-a*-2 [start-state]
  (loop [unvisited (list {:state start-state :parent nil :dist 0 :key (state-key start-state)})
         visited '()]
    (if (empty? unvisited)
      (list :gave-up (count visited))
      (let [node (min-scoring-node unvisited)
            state (:state node)
            unvisited-next-nodes (next-nodes node (concat visited unvisited))
            ;; state-key (state-key state)
            ;; next-nodes (map #(->node % node) (next-states state))
            ;; known-keys (set (map :key (concat visited unvisited)))
            ;; unvisited-next-nodes (remove #(some known-keys %) next-nodes)
            new-visited (concat visited (list node))
            new-unvisited (concat (remove #{node} unvisited) unvisited-next-nodes)]
;(println "next-nodes" next-nodes)
        (if (finished? state)
          (list node visited)
          (recur new-unvisited new-visited))))))

(defn shortest-path-length [fn input]
  (dec (count (find-path (fn input)))))

;; (time (shortest-path-length search-with-a* test-input))
;;  => 11
;; "Elapsed time: 16.1503 msecs"

;; (time (shortest-path-length search-with-a* real-input))
;; "Elapsed time: 5338147.5629 msecs"
;; => 88.96912 minutes
;; 34 lines = 33 steps.

;; I reworked my A* algorithm to skip similar cases, following the hint in
;; https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/db1v1ws/
;; This massively reduced the time, from 90 minutes to 1.

;; (time (shortest-path-length search-with-a*-2 test-input))
;; => 11
;; "Elapsed time: 28.9973 msecs"

;; (time (shortest-path-length search-with-a*-2 real-input))
;; => 33
;; "Elapsed time: 54808.1543 msecs"


;; Part 2
;; More items
(def test-input-2
  {4 #{}
   3 #{"LG"}
   2 #{"HG"}
   1 #{"E" "HM" "LM" "XM" "XG"}})

(def real-input-2
  {4 #{}
   3 #{"CoM" "CmM" "RuM" "PuM"}
   2 #{"CoG" "CmG" "RuG" "PuG"}
   1 #{"E" "PmG" "PmM" "ElG" "ElM" "DiG" "DiM"}})

;; (time (shortest-path-length search-with-a*-2 real-input-2))

;; The previous solution isn't fast enough, and it seems kind of hokey now that I come back to it,
;; so I think I'll try to start partly from scratch and re-use my (real) A* code from other problems.
(defn compute-goal [state]
  {4 (set (apply concat (vals state)))
   3 #{}
   2 #{}
   1 #{}})

(defn reconstruct-path [froms start goal]
  (loop [path (list goal)]
    (let [head (first path)]
      (if (nil? head)
        {:error :could-not-reconstruct-path
         :froms froms
         :path path}
        (if (= head start)
          {:steps (dec (count path))
           :path path}
          (let [prev (get froms head)]
            (recur (conj path prev))))))))

(defn uniquify [coll]
  (map first (vals (group-by state-key coll))))

(defn h-score-diff [state goal]
  (let [score-fn (fn [s] (reduce (fn [acc [k v]] (+ acc (* k (count v)))) 0 s))]
    (- (score-fn goal) (score-fn state))))

(defn h-count [state goal]
  (count (vals (dissoc state 4))))

(defn process-node [state open-set f-scores g-scores froms seen-keys goal h-fn]
  ;; (println "process-node" "state" state "open-set" open-set)
  ;; (println "neighbors" (next-states state))
  (loop [
         neighbors (map first (vals (group-by state-key (next-states state))))
         ;; neighbors (next-states state)
         open-set (disj open-set state)
         f-scores f-scores
         g-scores g-scores
         froms froms
         seen-keys seen-keys]
    ;; (println "loop" "seen-keys" seen-keys)
    (if (empty? neighbors)
      [open-set f-scores g-scores froms seen-keys]
      (let [neighbor (first neighbors)
            neighbor-key (state-key neighbor)
            tentative-g-score (inc (get g-scores state))]
        (if (and ;; (< tentative-g-score (get seen-keys neighbor-key Integer/MAX_VALUE))
             (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE)))
          (let [new-f-score (+ tentative-g-score (h-fn neighbor goal))]
            (recur (rest neighbors)
                   (conj open-set neighbor)
                   (assoc f-scores neighbor new-f-score)
                   (assoc g-scores neighbor tentative-g-score)
                   (assoc froms neighbor state)
                   (assoc seen-keys neighbor-key tentative-g-score)
                   ;; seen-keys
                   ))
          (recur (rest neighbors) open-set f-scores g-scores froms seen-keys))))))

(defn search-with-real-a* [start h-fn]
  (let [goal (compute-goal start)]
   (loop [open-set #{start}
          f-scores {start (h-fn start goal)}
          g-scores {start 0}
          froms {}
          seen-keys {(state-key start) 0}
          steps 0]
     ;; (println "loop" "open-set" open-set)
     (cond (empty? open-set)
           {:error :no-open-nodes}
           ;; (> steps 100000)
           ;; {:error :max-iterations}
           :else
           (let [current (first (sort-by #(get f-scores %) open-set))]
             (if (= goal current)
               (reconstruct-path froms start current)
               (let [[open-set f-scores g-scores froms seen-keys] (process-node current open-set f-scores g-scores froms seen-keys goal h-fn)]
                 ;; (println "new open-set" open-set)
                 (recur open-set f-scores g-scores froms seen-keys (inc steps)))))))))


;; (time (:steps (search-with-real-a* test-input h-count)))
;; "Elapsed time: 44.456799 msecs"
;; 11

;; (time (:steps (search-with-real-a* real-input h-count)))
;; "Elapsed time: 12577.974501 msecs"
;; 33

;; (time (:steps (search-with-real-a* real-input-2 h-count)))
;; "Elapsed time: 763945.0256 msecs"
;; nil

(defn state-key-2 [state]
  (let [items-and-floors (for [[k v] state
                               x v]
                           (list x k))
        state-map (reduce (fn [acc [item floor]]
                            (let [[atom type] (split-item item)]
                              (assoc-in acc [atom type] floor)))
                          {}
                          items-and-floors)]
    (sort (map (fn [[k v]]
                 (if (= k "E")
                   [(get v "E")]
                   [(get v "G")
                    (get v "M")]))
               state-map))))

(defn search-with-bfs [start]
  (let [goal (compute-goal start)]
   (loop [open-set (list start)
          steps 0
          visited-keys #{}]
     ;; (println "steps" steps "open set size" (count open-set))
     (if (> (count (filter #(= % goal) open-set)) 0)
       steps
       (let [new-states (mapcat (fn [s]
                                  (map (fn [ns] [(state-key-2 ns) ns]) (next-states s)))
                                open-set)
             grouped (into {} new-states)]
         (recur (map second (remove (fn [[k v]] (contains? visited-keys k)) grouped))
                (inc steps)
                (apply conj visited-keys (map first grouped))))))))

;; After all that, all I needed was to get my state-key right and do a breadth-first search.

;; (time (search-with-bfs test-input))
;; "Elapsed time: 21.07 msecs"
;; 11

;; (time (search-with-bfs real-input))
;; "Elapsed time: 1506.3912 msecs"
;; 33

;; (time (search-with-bfs real-input-2))
;; "Elapsed time: 10191.2221 msecs"
;; 57
