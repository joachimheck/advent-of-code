(ns day-11.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Move microchips to the fourth floor.

(def test-input
  {4 '()
   3 '("LG")
   2 '("HG")
   1 '("E" "HM" "LM")})

(def real-input
  {4 '()
   3 '("CoM" "CmM" "RuM" "PuM")
   2 '("CoG" "CmG" "RuG" "PuG")
   1 '("E" "PmG" "PmM")})

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
      (update start-floor (fn [floor] (remove (set (concat items '("E"))) floor)))
      (update end-floor (fn [floor] (concat '("E") (sort (concat items floor)))))))

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
  (every? #(= '() %) (for [i (range 1 4)] (get state i))))

(defn find-path [[end-node nodes]]
 (loop [path [end-node]]
;; (println path)
   (if (nil? (:parent (last path)))
     (reverse (map :state path))
     (recur (conj path (first (filter #(= (:state %) (:parent (last path))) nodes)))))))

(defn score [{state :state dist :dist}]
  (- dist (reduce (fn [acc [k v]] (+ acc (* k (count v)))) 0 state)))

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

(defn state-key [state]
  (->> state
       (mapcat (fn [[floor items]]
                 (map (fn [item] [floor (split-item item)]) items)))
       (reduce
        (fn [[atom-numbers item-floors] [floor [atom type]]]
          (let [next-num (if (vals atom-numbers) (inc (apply max (vals atom-numbers))) 0)
                atom-num (if (get atom-numbers atom) (get atom-numbers atom) next-num)
                floors (if (get item-floors atom-num) (get item-floors atom-num) {})]
            (list
             (assoc atom-numbers atom atom-num)
             (update item-floors atom-num #(assoc % type floor)))))
        ({} {}))
       second))

(defn search-with-a* [start-state]
  (loop [unvisited (list {:state start-state :parent nil :dist 0 :key (state-key start-state)}) visited '()]
    (if (empty? unvisited)
      (list :gave-up (count visited))
      (let [node (min-scoring-node unvisited)
            state (:state node)
            state-key (state-key state)
            next-nodes (map #(assoc {} :state % :parent state :dist (inc (:dist node)) :key (state-key %))
                            (next-states state))
            unvisited-keys (set (map :key unvisited))
            unvisited-next-nodes (remove #(some unvisited-keys %) next-nodes)
            new-visited (concat visited (list node))
            new-unvisited (concat (remove #{node} unvisited) unvisited-next-nodes)]
        (if (finished? state)
          (list node visited)
          (recur new-unvisited new-visited))))))

(defn shortest-path-length [input]
  (dec (count (find-path (search-with-a* input)))))

;; (time (shortest-path-length test-input))
;;  => 11
;; "Elapsed time: 16.1503 msecs"

;; (time (shortest-path-length real-input))
;; "Elapsed time: 5338147.5629 msecs"
;; => 88.96912 minutes
;; 34 lines = 33 steps.

(time (shortest-path-length real-input))




;; Part 2
;; More items
(def real-input-2
  {4 '()
   3 '("CoM" "CmM" "RuM" "PuM")
   2 '("CoG" "CmG" "RuG" "PuG")
   1 '("E" "PmG" "PmM" "EG" "EM" "DG" "DM")})
