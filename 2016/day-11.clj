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

(defn leaves [tree]
  (keep (fn [[k v]] (when (empty? v) k)) tree))


;; (defn build-tree [start-state]
;;   (loop [state-tree {start-state '()}]
;;     (if (empty? (leaves state-tree))
;;       (list :gave-up state-tree)
;;       (let [node (first (leaves state-tree))
;;             nexts (remove (set (keys state-tree)) (next-states node))
;;             new-tree (-> state-tree
;;                           (update node #(concat % nexts '(:visited)))
;;                           ((fn [state-tree]
;;                              (reduce (fn [acc state] (assoc acc state '())) state-tree nexts))))]
;;         (if (finished? node)
;;           new-tree
;;           (recur new-tree))))))

(defn build-tree [start-state]
  (loop [unvisited (list {:node start-state :parent nil}) visited '()]
    (if (empty? unvisited)
      :gave-up
      (let [node (:node (first unvisited))
            nexts (remove (set visited) (map #(assoc {} :node % :parent node) (next-states node)))
            new-visited (concat visited (list (first unvisited)))
            new-unvisited (concat (rest unvisited) nexts)]
;; (println node)
;; (println nexts)
;; (println new-visited)
;; (println new-unvisited)
        (if (finished? node)
          (list (first unvisited) visited)
          (recur new-unvisited new-visited)
)))))

;;(build-tree test-input)


;; (defn children [node] (rest node))

;; (defn replace-node [tree old new]
;;  (if (= (first tree) old)
;;    (concat (list new) (rest tree))
;;    (concat (list (first tree))
;;            (map #(replace-node % old new) (rest tree)))))

;; (defn add-nodes [tree tgt nodes]
;;   (if (= (first tree) tgt)
;;     (concat (list (first tree)) 
;;             (list (concat (rest tree) nodes)))
;;     (concat (list (first tree))
;;             (map #(add-nodes % tgt nodes) (rest tree)))))

;; (defn leaves-2 [tree]
;;   )
;; (tree-seq identity rest '(:A (:B (:D) (:E)) (:C (:F))))

;; (defn build-tree-2 [start-state]
;;   (loop [state-tree '(start-state)]

;;     )

;;   )

;; (defn dfs [tree node]
;;   (cond
;;     (= :visited node)
;;     '()

;;     (finished? node)
;;     node

;;     :else
;;     (let [children (get tree node)]
;;       (if (or (empty? children))
;;        '()
;;        (let [search-result (filter (comp not empty?)
;;                                    (map (partial dfs tree) children))]
;;          (if (not (empty? search-result))
;;            (list node search-result)))))))


;; (let [tree (build-tree test-input)
;;                 node test-input]
;;             (dfs tree node))

;; (filter finished? (remove #{:visited} (flatten (distinct (vals (build-tree test-input))))))
;; (flatten (build-tree test-input))


;; (map print-state
;;      (flatten (let [tree (build-tree test-input)
;;                 node test-input]
;;             (dfs tree node)
;;             )))

