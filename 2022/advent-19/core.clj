(ns advent-19.core)

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
;; What is the sum of the quality levels of the blueprints?
(defn parse-input [f]
  (let [lines (read-lines f)]
    (reduce
     (fn [result line]
       (let [obsidian-robot-values (map #(Long/parseLong %) (rest (first (re-seq #"obsidian robot costs (\d+) ore and (\d+) clay" line))))
             geode-robot-values (map #(Long/parseLong %) (rest (first (re-seq #"geode robot costs (\d+) ore and (\d+) obsidian" line))))
             ore-robot-cost {:ore (Long/parseLong (second (re-find #"ore robot costs (\d+) ore" line)))}
             clay-robot-cost {:ore (Long/parseLong (second (re-find #"clay robot costs (\d+) ore" line)))}
             obsidian-robot-cost (assoc {} :ore (first obsidian-robot-values) :clay (second obsidian-robot-values))
             geode-robot-cost (assoc {} :ore (first geode-robot-values) :obsidian (second geode-robot-values))
             robot-costs (list ore-robot-cost clay-robot-cost obsidian-robot-cost geode-robot-cost)]
         (assoc result (Long/parseLong (second (re-find #"Blueprint (\d+):" line)))
                {
                 :ore-robot ore-robot-cost
                 :clay-robot clay-robot-cost
                 :obsidian-robot obsidian-robot-cost
                 :geode-robot geode-robot-cost
                 :max-ore-robots (apply max (map #(get % :ore) robot-costs))
                 :max-clay-robots (get obsidian-robot-cost :clay)
                 :max-obsidian-robots (get geode-robot-cost :obsidian)
                 }
                )))
     {}
     lines)))

(defn can-build? [materials robot blueprint]
  (let [robot-cost (get blueprint robot)]
    (reduce (fn [result [k v]] (and result (>= (materials k) v))) true robot-cost)))

(def robots (list :ore-robot :clay-robot :obsidian-robot :geode-robot))
(def robot-incomes {:ore-robot :ore :clay-robot :clay :obsidian-robot :obsidian :geode-robot :geodes})

(defn get-buildable [materials blueprint]
  (filter (fn [robot] (can-build? materials robot blueprint)) robots))

(defn build-robot [robot {income :income materials :materials :as node} blueprint]
  (assoc node
         :income (update income (robot-incomes robot) inc)
         :materials (reduce (fn [result [material cost]]
                              (update result material #(- % cost)))
                            materials
                            (get blueprint robot))))

(def start-node {:income {:ore 1 :clay 0 :obsidian 0 :geodes 0} :materials {:ore 0 :clay 0 :obsidian 0 :geodes 0}})

(defn produce-geodes-dfs [blueprint minutes max-minutes node]
  ;; (println minutes "/" max-minutes "produce-geodes-dfs" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (= minutes max-minutes)
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            no-action-node (assoc node :materials new-materials)
            minutes-left (- max-minutes minutes)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   (get-buildable materials blueprint))
                             no-action-node)
            sub-results (map (fn [next-node] (produce-geodes-dfs blueprint (inc minutes) max-minutes next-node)) next-nodes)]
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))

;; This doesn't seem to prune anything. I guess the only repeated incomes are from the no-action branch.
(defn produce-geodes-explored-incomes [blueprint minutes max-minutes node explored-incomes]
  ;; (println minutes "/" max-minutes "produce-geodes-explored-incomes" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (= minutes max-minutes)
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            no-action-node (assoc node :materials new-materials)
            minutes-left (- max-minutes minutes)
            next-nodes (conj (reverse (remove #(contains? explored-incomes (:income %))
                                      (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                            (get-buildable materials blueprint))))
                             no-action-node)
            new-explored-incomes (conj explored-incomes (get node :income))
            sub-results (map (fn [next-node]
                               (produce-geodes-explored-incomes blueprint (inc minutes) max-minutes next-node new-explored-incomes))
                             next-nodes)]
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))

(defn produce-geodes-dfs-pmap [blueprint minutes max-minutes node]
  ;; (println minutes "/" max-minutes "produce-geodes-dfs-pmap" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (= minutes max-minutes)
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            no-action-node (assoc node :materials new-materials)
            minutes-left (- max-minutes minutes)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   (get-buildable materials blueprint))
                             no-action-node)
            sub-results (if (< 3 minutes-left 8)
                          (pmap (fn [next-node] (produce-geodes-dfs-pmap blueprint (inc minutes) max-minutes next-node)) next-nodes)
                          (mapv (fn [next-node] (produce-geodes-dfs-pmap blueprint (inc minutes) max-minutes next-node)) next-nodes))]
        ;; (println "next-nodes" next-nodes)
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))


;; ideas
;; keep all successor nodes as we work through the tree, prepend new ones and recurse from the start of the list.
;; make a blueprint that generates geodes more quickly, for testing

;; ore clay obsidian geodes
(def start-node-vector [[1 0 0 0] [0 0 0 0] []])

(defn parse-input-vector [f]
  (let [lines (read-lines f)]
    (reduce
     (fn [result line]
       (let [obsidian-robot-values (map #(Long/parseLong %) (rest (first (re-seq #"obsidian robot costs (\d+) ore and (\d+) clay" line))))
             geode-robot-values (map #(Long/parseLong %) (rest (first (re-seq #"geode robot costs (\d+) ore and (\d+) obsidian" line))))
             ore-robot-cost (Long/parseLong (second (re-find #"ore robot costs (\d+) ore" line)))
             clay-robot-cost (Long/parseLong (second (re-find #"clay robot costs (\d+) ore" line)))
             obsidian-robot-costs (map #(Long/parseLong %) (rest (first (re-seq #"obsidian robot costs (\d+) ore and (\d+) clay" line))))
             geode-robot-costs (map #(Long/parseLong %) (rest (first (re-seq #"geode robot costs (\d+) ore and (\d+) obsidian" line))))
             robot-costs [[ore-robot-cost 0 0 0]
                          [clay-robot-cost 0 0 0]
                          [(first obsidian-robot-costs) (second obsidian-robot-costs) 0 0]
                          [(first geode-robot-costs) 0 (second geode-robot-costs) 0]]]
         (assoc result (Long/parseLong (second (re-find #"Blueprint (\d+):" line)))
                {:robot-costs robot-costs
                 :max-robots (apply map max robot-costs)})))
     {}
     lines)))

(defn can-build-vector? [materials robot blueprint]
  (reduce #(and %1 %2) (map >= materials (get blueprint robot))))

(defn get-buildable-vector [materials blueprint]
  ;;(println "get-buildable-vector" materials blueprint)
  (filter (fn [robot] (can-build-vector? materials robot blueprint)) (range 4)))

(defn build-robot-vector [robot [income materials do-not-build :as node] blueprint]
  [(update income robot inc) (mapv - materials (get blueprint robot)) do-not-build])

(defn produce-geodes-vectors [blueprint minutes max-minutes [income materials :as node]]
  ;; (println minutes "/" max-minutes "produce-geodes-vectors" node)
  (let [minutes-left (- max-minutes minutes)]
    (if (= minutes max-minutes)
      (list (materials 3) 1)
      (let [no-action-node (assoc node 1 (mapv + materials income))
            minutes-left (- max-minutes minutes)
            next-nodes (conj (mapv (fn [robot] (build-robot-vector robot no-action-node blueprint))
                                   (get-buildable-vector materials blueprint))
                             no-action-node)
            sub-results (if (< 3 minutes-left 8) ;;(< minutes-left -1) ;; (< 3 minutes-left 8)
                          (pmap (fn [next-node] (produce-geodes-vectors blueprint (inc minutes) max-minutes next-node)) next-nodes)
                          (mapv (fn [next-node] (produce-geodes-vectors blueprint (inc minutes) max-minutes next-node)) next-nodes))]
        ;; (println "next-nodes" next-nodes)
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))



;; Seems to work but is slower than the recursive technique.
;; (time (produce-geodes-iterative (get (parse-input small-input) 1) 19))
;; "Elapsed time: 88917.3609 msecs"
;; (1 8595059 33)
(defn produce-geodes-iterative [blueprint max-minutes]
  (loop [i 0 successors (list (list start-node 0)) max-geodes 0 max-successors 0]
    ;; (println "-iterative loop" successors)
    (if (empty? successors)
      (list max-geodes i max-successors)
      (let [current (first successors)
            [node minutes] current] 
        (if (= minutes max-minutes)
          (recur (inc i)
                 (rest successors)
                 (max max-geodes (get-in node [:materials :geodes]))
                 max-successors)
          (let [income (:income node)
                materials (:materials node)
                new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
                no-action-node (assoc node :materials new-materials)
                next-nodes (conj (mapv (fn [robot] (list (build-robot robot no-action-node blueprint) (inc minutes)))
                                       (get-buildable materials blueprint))
                                 (list no-action-node (inc minutes)))]
            ;; (println "removing" (first next-nodes) "from" (apply conj successors (rest next-nodes)) "result" (remove #{(first next-nodes)} (apply conj successors (rest next-nodes))))
            ;; (println "recurring" (remove #{current} (apply conj successors next-nodes)))
            (recur i
                   (remove #{current} (apply conj successors next-nodes))
                   max-geodes
                   (max max-successors (count successors)))))))))

;; (produce-geodes-iterative (get (parse-input small-input) 1) 4)

(defn produce-geodes-max-robots [blueprint minutes max-minutes node]
  ;;(println minutes "/" max-minutes "produce-geodes-max-robots" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (or (= minutes max-minutes)
            (or (> (income :ore) (blueprint :max-ore-robots))
                (> (income :clay) (blueprint :max-clay-robots))
                (> (income :obsidian) (blueprint :max-obsidian-robots))))
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            no-action-node (assoc node :materials new-materials)
            minutes-left (- max-minutes minutes)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   (get-buildable materials blueprint))
                             no-action-node)
            sub-results (map (fn [next-node] (produce-geodes-max-robots blueprint (inc minutes) max-minutes next-node)) next-nodes)]
        ;; (println "next-nodes" next-nodes)
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))


;; (time (produce-geodes-dfs (get (parse-input small-input) 1) 0 15 start-node))
;; "Elapsed time: 343.4889 msecs"
;; (0 58722)
;; (time (produce-geodes-explored-incomes (get (parse-input small-input) 1) 0 15 start-node #{}))
;; "Elapsed time: 479.5755 msecs"
;; (0 58722)
;; (time (produce-geodes-dfs-pmap (get (parse-input small-input) 1) 0 15 start-node))
;; "Elapsed time: 231.3209 msecs"
;; (0 58722)
;; (time (produce-geodes-vectors (get (parse-input-vector small-input) 1) 0 15 start-node-vector))
;; "Elapsed time: 252.0894 msecs"
;; (0 58722)
;; (time (produce-geodes-iterative (get (parse-input small-input) 1) 15))
;; "Elapsed time: 677.9165 msecs"
;; (0 58722 23)
;; (time (produce-geodes-max-robots (get (parse-input small-input) 1) 0 15 start-node))
;; "Elapsed time: 333.1921 msecs"
;; (0 54295)
;; (time (produce-geodes-multi (get (parse-input small-input) 1) 0 15 start-node))
;; "Elapsed time: 24.3328 msecs"
;; (0 3453)

;; The secret was pruning branches in which we built something we could have already built.
(defn produce-geodes-multi [blueprint minutes max-minutes node]
  ;;(println minutes "/" max-minutes "produce-geodes-max-robots" node)
  (let [income (:income node)
        materials (:materials node)]
    (if (or (= minutes max-minutes)
            (or (> (income :ore) (blueprint :max-ore-robots))
                (> (income :clay) (blueprint :max-clay-robots))
                (> (income :obsidian) (blueprint :max-obsidian-robots))))
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            minutes-left (- max-minutes minutes)
            buildable-robots (remove (into #{} (node :do-not-build)) (get-buildable materials blueprint))
            no-action-node (assoc node :materials new-materials)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   buildable-robots)
                             (assoc no-action-node :do-not-build buildable-robots))
            sub-results (if (< 3 minutes-left 8)
                          (pmap (fn [next-node] (produce-geodes-multi blueprint (inc minutes) max-minutes next-node)) next-nodes)
                          (mapv (fn [next-node] (produce-geodes-multi blueprint (inc minutes) max-minutes next-node)) next-nodes))]
        ;; (println "next-nodes" next-nodes)
        (list (apply max (map first sub-results)) (apply + (map second sub-results)))))))

(defn produce-geodes-multi-vector [blueprint minutes max-minutes [income materials do-not-build :as node]]
  ;; (println "produce-geodes-multi-vector" node)
  (if (or (= minutes max-minutes)
          (some true? (apply map > [income (:max-robots blueprint)])))
    (list (materials 3) 1)
    (let [no-action-node (assoc node 1 (mapv + materials income))
          minutes-left (- max-minutes minutes)
          buildable-robots (remove (into #{} do-not-build) (get-buildable-vector materials (:robot-costs blueprint)))
          next-nodes (conj (mapv (fn [robot] (build-robot-vector robot no-action-node (:robot-costs blueprint)))
                                 buildable-robots)
                           (assoc no-action-node 2 buildable-robots))
          sub-results (if (< 3 minutes-left 8) ;;(< minutes-left -1) ;; (< 3 minutes-left 8)
                        (pmap (fn [next-node] (produce-geodes-multi-vector blueprint (inc minutes) max-minutes next-node)) next-nodes)
                        (mapv (fn [next-node] (produce-geodes-multi-vector blueprint (inc minutes) max-minutes next-node)) next-nodes))]
      ;; (println "next-nodes" next-nodes)
      (list (apply max (map first sub-results)) (apply + (map second sub-results))))))

(defn sum-quality-levels [f]
  (let [blueprints (parse-input f)]
    (apply +
     (doall (map (fn [[blueprint-id blueprint]]
                   (print blueprint-id "")
                   (time (* blueprint-id (first (produce-geodes-multi blueprint 0 24 start-node)))))
                 blueprints)))))



;; (sum-quality-levels small-input)
;; 1 "Elapsed time: 47966.832 msecs"
;; 2 "Elapsed time: 53045.7299 msecs"
;; 33

;; (sum-quality-levels large-input)
;; 7 "Elapsed time: 7468.5036 msecs"
;; 20 "Elapsed time: 20508.289 msecs"
;; 27 "Elapsed time: 4571.8669 msecs"
;; 1 "Elapsed time: 4599.5236 msecs"
;; 24 "Elapsed time: 8292.6635 msecs"
;; 4 "Elapsed time: 39420.1223 msecs"
;; 15 "Elapsed time: 5944.3868 msecs"
;; 21 "Elapsed time: 10214.8382 msecs"
;; 13 "Elapsed time: 6586.0245 msecs"
;; 22 "Elapsed time: 5924.8762 msecs"
;; 29 "Elapsed time: 14055.5755 msecs"
;; 6 "Elapsed time: 29977.3122 msecs"
;; 28 "Elapsed time: 7841.1056 msecs"
;; 25 "Elapsed time: 2760.9282 msecs"
;; 17 "Elapsed time: 5390.2364 msecs"
;; 3 "Elapsed time: 4106.7694 msecs"
;; 12 "Elapsed time: 17545.7611 msecs"
;; 2 "Elapsed time: 4882.6694 msecs"
;; 23 "Elapsed time: 6786.7644 msecs"
;; 19 "Elapsed time: 12545.8731 msecs"
;; 11 "Elapsed time: 3971.7985 msecs"
;; 9 "Elapsed time: 2704.5192 msecs"
;; 5 "Elapsed time: 3967.6792 msecs"
;; 14 "Elapsed time: 5455.5573 msecs"
;; 26 "Elapsed time: 7381.2638 msecs"
;; 16 "Elapsed time: 5173.1126 msecs"
;; 30 "Elapsed time: 21533.4876 msecs"
;; 10 "Elapsed time: 9050.443 msecs"
;; 18 "Elapsed time: 34145.124 msecs"
;; 8 "Elapsed time: 7436.4695 msecs"
;; 988




;; Part 2
;; How many geodes can be made with the first three blueprints in 32 minutes?
(defn produce-geodes-optimized [blueprint minutes max-minutes node]
  ;;(println minutes "/" max-minutes "produce-geodes-max-robots" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (or (= minutes max-minutes)
            (or (> (income :ore) (blueprint :max-ore-robots))
                (> (income :clay) (blueprint :max-clay-robots))
                (> (income :obsidian) (blueprint :max-obsidian-robots)))
            (or (and (<= (income :geodes) 4)
                     (< minutes-left (int (* max-minutes (/ 2 32)))))
                (and (= (income :geodes) 0)
                     (< minutes-left (int (* max-minutes (/ 4 32)))))
                (and (= (income :obsidian) 0)
                     (< minutes-left (int (* max-minutes (/ 16 32)))))
                (and (= (income :clay) 0)
                     (< minutes-left (int (* max-minutes (/ 8 32)))))))
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            buildable-robots (remove (into #{} (node :do-not-build)) (get-buildable materials blueprint))
            no-action-node (assoc node :materials new-materials)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   buildable-robots)
                             (assoc no-action-node :do-not-build buildable-robots))
            sub-results (map (fn [next-node] (produce-geodes-optimized blueprint (inc minutes) max-minutes next-node)) next-nodes)]
        ;; (println "next-nodes" next-nodes)
        (if (empty? sub-results)
          (list 0 1)
          (list (apply max (map first sub-results)) (apply + (map second sub-results))))))))


;; (* 24 21 6) 3024
;; (* 30 26 8) 6240
;; -> answer <-

(defn estimated-geodes [blueprint node minutes-left]
  (if (> minutes-left 8)
    Integer/MAX_VALUE
    (if (= 0 (get-in node [:income :obsidian]))
      0
      (loop [l-node node l-minutes-left (inc minutes-left)]
        (let [income (l-node :income)
              materials (l-node :materials)
              new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
              no-action-node (assoc l-node :materials new-materials)]
          ;;(println "l-node" l-node "l-minutes-left" l-minutes-left "buildable" (get-buildable materials blueprint))
          (if (= l-minutes-left 0)
            (get-in l-node [:materials :geodes])
            (cond
              (some #{:geode-robot} (get-buildable materials blueprint))
              (recur (build-robot :geode-robot no-action-node blueprint) (dec l-minutes-left))
              (some #{:obsidian-robot} (get-buildable materials blueprint))
              (recur (build-robot :obsidian-robot no-action-node blueprint) (dec l-minutes-left))
              :else
              (recur no-action-node (dec l-minutes-left)))))))))

(defn prune? [blueprint node minutes max-minutes minutes-left max-geodes]
  (let [income (node :income)
        materials (node :materials)]
   (or (= minutes max-minutes)
       (or (> (income :ore) (blueprint :max-ore-robots))
           (> (income :clay) (blueprint :max-clay-robots))
           (> (income :obsidian) (blueprint :max-obsidian-robots))
           (< (estimated-geodes blueprint node minutes-left) max-geodes)))))

(defn produce-geodes-optimized-2 [blueprint minutes max-minutes node max-geodes]
  ;;(println minutes "/" max-minutes "produce-geodes-optimized-2" node)
  (let [income (:income node)
        materials (:materials node)
        minutes-left (- max-minutes minutes)]
    (if (prune? blueprint node minutes max-minutes minutes-left max-geodes)
      (list (get-in node [:materials :geodes]) 1)
      (let [new-materials (reduce (fn [result [k v]] (update result k (fn [i] (+ v i)))) materials income)
            buildable-robots (remove (into #{} (node :do-not-build)) (get-buildable materials blueprint))
            no-action-node (assoc node :materials new-materials)
            next-nodes (conj (mapv (fn [robot] (build-robot robot no-action-node blueprint))
                                   buildable-robots)
                             (assoc no-action-node :do-not-build buildable-robots))
            sub-results (reduce (fn [result next-node]
                                  (let [next-result
                                        (produce-geodes-optimized-2 blueprint (inc minutes) max-minutes next-node (result :max-geodes))]
                                    {:results (conj (result :results) next-result) :max-geodes (max (result :max-geodes) (first next-result))}))
                                {:results '() :max-geodes max-geodes}
                                next-nodes)]
        ;; (println "next-nodes" next-nodes)
        (list (sub-results :max-geodes) (apply + (map second (sub-results :results))))))))



;; (time (produce-geodes-optimized-2 (get (parse-input large-input) 1) 0 32 start-node 0))
;; "Elapsed time: 60098.0462 msecs"
;; (30 1429499)
;; (time (produce-geodes-optimized-2 (get (parse-input large-input) 2) 0 32 start-node 0))
;; "Elapsed time: 65374.2839 msecs"
;; (26 1559752)
;; (time (produce-geodes-optimized-2 (get (parse-input large-input) 3) 0 32 start-node 0))
;; "Elapsed time: 69138.4027 msecs"
;; (11 1793310)
;;
;; (* 30 26 11)
;; 8580
