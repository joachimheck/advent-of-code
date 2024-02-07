(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

;; Part 1
;; What is the product of the sizes of the two groups of components
;; that can be created by separating three wires.
(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #": "))
       (map (fn [[a b]] [a (str/split b #" ")]))
       (mapcat (fn [[a bs]] (map #(set (list a %)) bs)))
       (set)))

(defn at-a-time [n coll]
  (cond (or (= n 0) (< (count coll) n))
        '()
        (= n 1)
        (map list coll)
        :else
        (concat (map #(conj % (first coll)) (at-a-time (dec n) (rest coll)))
                (at-a-time n (rest coll)))))

(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
            (pairs (rest coll)))))

(defn find-paths [nodes connections]
  (for [start nodes
        end nodes
        :when (not= start end)]
    (loop [paths [[start]]]
      (let [finished (first (filter #(= (last %) end) paths))]
        (if (some? finished)
          (list start end finished)
          (recur
           (for [path paths
                 :let [p-last (last path)
                       p-pre-last (last (butlast path))]
                 next (map first (map #(remove #{p-last} %) (filter #(contains? % p-last) connections)))
                 :when (not= next p-pre-last)]
             (vec (conj path next)))))))))

(defn find-paths-pairs [nodes connections]
  (apply concat
         (for [[start end] (pairs nodes)]
           (loop [paths [[start]]]
             (let [finished (first (filter #(= (last %) end) paths))]
               (if (some? finished)
                 (list [start end finished] [end start (reverse finished)])
                 (recur
                  (for [path paths
                        :let [p-last (last path)
                              p-pre-last (last (butlast path))]
                        next (map first (map #(remove #{p-last} %) (filter #(contains? % p-last) connections)))
                        :when (not= next p-pre-last)]
                    (vec (conj path next))))))))))

(defn find-paths-reduce [nodes connections]
  (let [pairs (profile "find pairs" (pairs nodes))
        pair-count (count pairs)]
    (println "found" pair-count "pairs")
    (:reduced-paths
     (reduce (fn [{:keys [reduced-paths path-count] :as acc} [start end]]
               (println "Finding path from" start "to" end)
               (if (and (> (mod (- path-count 2) (quot pair-count 10)) (mod path-count (quot pair-count 10))))
                 (printf "%d%%\n" (int (* 100 (/ path-count (* 2 pair-count))))))
               (loop [open-paths [[start]]
                      sub-paths []]
                 (let [finished (first (filter #(= (last %) end) open-paths))]
                   (if finished
                     (let [new-paths (conj sub-paths [[start end] finished])
                           new-plus-reversed (apply concat (map (fn [[[s e] p]] [[[s e] p] [[e s] (reverse p)]]) new-paths))]
                       (println "new-plus-reversed" new-plus-reversed)
                       (assoc acc
                              :reduced-paths (into reduced-paths new-plus-reversed)
                              :path-count (+ (:path-count acc) 2)))
                     (let [extendeds (for [path open-paths
                                           :let [p-last (last path)
                                                 p-pre-last (last (butlast path))]
                                           next (map first (map #(remove #{p-last} %) (filter #(contains? % p-last) connections)))
                                           :when (not= next p-pre-last)]
                                       (if-let [sub-path (get acc [p-last end])]
                                         (do (println "Found sub-path from" p-last "to" end)
                                             (apply conj path (rest sub-path)))
                                         (vec (conj path next))))
                           new-sub-paths (map (fn [p] [[(first p) (last p)] p]) extendeds)]
                       (recur extendeds (apply conj sub-paths new-sub-paths)))))))
             {:reduced-paths {} :path-count 0}
             pairs))))

(defn find-paths-flood [nodes connections]
  (loop [start-nodes nodes
         path-map {}]
    (if (empty? start-nodes)
      path-map
      (let [start (first start-nodes)
            new-path-map (profile (str "paths for " start)
                                  (loop [open-nodes [(first start-nodes)]
                                         path-map' (assoc path-map [start start] [start])
                                         visited #{}]
                                    (if (empty? open-nodes)
                                      (dissoc path-map' [start start])
                                      (let [current (first open-nodes)
                                            current-path (get path-map' [start current])
                                            neighbors (map first (map #(remove #{current} %) (filter #(contains? % current) connections)))
                                            unvisited (remove visited neighbors)]
                                        ;; (if (= "hfx" start)
                                        ;;   (println "open-nodes" open-nodes "current" current "unvisited" unvisited "path-map'" path-map'))
                                        (recur (distinct (apply conj (vec (rest open-nodes)) unvisited))
                                               (reduce (fn [acc n]
                                                         (if-not (get path-map' [start n])
                                                           (do
                                                             ;; (println "adding path" [start n] (conj current-path n))
                                                             (let [new-path (conj current-path n)]
                                                               (assoc acc [start n] new-path)
                                                               ))
                                                           acc))
                                                       path-map'
                                                       neighbors)
                                               (conj visited current))))))]
        (recur (rest start-nodes) new-path-map)))))

(defn get-connection-frequencies [connections]
  (let [nodes (profile "get nodes" (set (apply concat connections)))
        paths (profile "find paths" (find-paths-flood nodes connections))]
    (as-> paths x
         (profile "map last" (map last x))
         (profile "partition" (map #(partition 2 1 %) x))
         (profile "map set" (map (fn [p] (map #(set %) p)) x))
         (profile "concat" (apply concat x))
         (profile "frequencies" (frequencies x)))))

(defn find-connected [node connections]
  (loop [open-set #{node}
         connected #{}]
    (if (empty? open-set)
      connected
      (let [new-connected (apply conj connected open-set)
            ;; _ (println "open-set" open-set)
            ;; _ (println "connected" (filter #(some open-set %) connections))
            new-open-set (set (remove new-connected (apply concat (filter #(some open-set %) connections))))]
        (recur new-open-set new-connected)))))

(defn group-nodes [connections]
  (let [nodes (set (apply concat connections))]
    (loop [open-set nodes
           groups []]
      (if (empty? open-set)
        groups
        (let [group (find-connected (first open-set) connections)]
          (recur (remove group open-set) (conj groups group)))))))

(defn multiply-split-group-sizes [input]
  (reset! profile-times {})
  (let [connections (profile "parse input" (parse-input input))
        freqs (profile "get frequencies" (get-connection-frequencies connections))
        top-3 (profile "find top 3" (set (map first (take 3 (reverse (sort-by second freqs))))))
        new-connections (profile "get new connections" (remove top-3 connections))
        new-groups (profile "get new groups" (group-nodes new-connections))]
    (apply * (map count new-groups))))


;; 1557
;; ---> answer <---


(defn find-non-triangle-connections [input]
  (let [connections (parse-input input)
        nodes (set (apply concat connections))
        neighbor-map (into {}
                           (for [node nodes]
                             [node (set (map first (map #(remove #{node} %)
                                                        (filter #(contains? % node) connections))))]))]
    (distinct
     (for [node nodes
           neighbor (get neighbor-map node)
           :let [n2s (get neighbor-map neighbor)
                 triangle-node (first (set/intersection n2s (get neighbor-map node)))]
           :when (nil? triangle-node)]
       #{node neighbor}))))

(defn find-shortest-loops
  ([input] (find-shortest-loops input nil))
  ([input start]
   (let [connections (parse-input input)
         nodes (set (apply concat connections))
         neighbor-map (into {}
                            (for [node nodes]
                              [node (set (map first (map #(remove #{node} %) (filter #(contains? % node) connections))))]))
         make-pathoid (fn [start node] {:start start :n2 node :end node :path [#{start node}]})
         extend-pathoid (fn [{:keys [start n2 end path] :as p} cnx]
                          (let [next-node (first (remove #{end} cnx))]
                            {:start start :n2 n2 :end next-node :path (conj path cnx)}))]
     (loop [open-pathoids (if (nil? start)
                            (for [n nodes
                                  nbr (get neighbor-map n)]
                              (make-pathoid n nbr))
                            (map #(make-pathoid start %) (get neighbor-map start)))
            shortest-paths {}]
       (let [spc (count shortest-paths)]
         (if (and (not= 0 spc) (= 0 (mod spc 100)))
           (println "Shortest path count" spc "pathoid count" (count open-pathoids))))
       (if (empty? open-pathoids)
         shortest-paths
         (let [{:keys [start n2 end path] :as current} (first (sort-by #(count (:path %)) open-pathoids))
               next-connections (remove (set path) (filter #(contains? % end) connections))]
           ;; (println "current" [(:start current) (:end current)] "open-pathoids" (map :path open-pathoids))
           (if (some #{start} (apply concat next-connections))
             (recur (remove (fn [{p-start :start p-n2 :n2}] (and (= p-start start) (= p-n2 n2))) open-pathoids)
                    (assoc shortest-paths [start n2] (inc (count path))))
             (recur (apply conj (remove #{current} open-pathoids) (map #(extend-pathoid current %) next-connections))
                    shortest-paths))))))))

(defn make-pathoid [start node]
  {:start start :n2 node :end node :path [node]})

(defn extend-pathoid [{:keys [start n2 end path] :as p} next-node]
  {:start start :n2 n2 :end next-node :path (conj path next-node)})

(defn compare-pathoids [a b]
  (compare [(count (:path a)) [(:start a) (:path a)]] [(count (:path b)) [(:start b) (:path b)]]))

(defn find-starting-pathoids [nodes neighbor-map start]
  (apply sorted-set-by compare-pathoids
         (if (nil? start)
           (for [n nodes
                 nbr (get neighbor-map n)]
             (make-pathoid n nbr))
           (map #(make-pathoid start %) (get neighbor-map start)))))

(defn make-neighbor-map [nodes connections]
  (into {}
        (for [node nodes]
          [node (set (map first
                          (map #(remove #{node} %)
                               (filter #(contains? % node) connections))))])))

(defn find-shortest-loops-nodes
  ([input] (find-shortest-loops-nodes input nil))
  ([input start]
   (let [connections (parse-input input)
         nodes (set (apply concat connections))
         neighbor-map (make-neighbor-map nodes connections)
         starting-pathoids (find-starting-pathoids nodes neighbor-map start)]
     ;; (println "start" start)
     ;; (println "nodes" nodes)
     ;; (println "nbrs" (for [n nodes] (list n (get neighbor-map n))))
     ;; (println "starting-pathoids" starting-pathoids)
     (loop [open-pathoids starting-pathoids
            shortest-paths {}
            max-short-paths 0]
       (let [spc (count shortest-paths)
             new-max-length? (>= (- spc max-short-paths) 100)
             new-max-short-paths (if new-max-length? spc max-short-paths)]
         (if new-max-length?
           (println "shortest path count" spc "pathoid count" (count open-pathoids)))
         (if (empty? open-pathoids)
           shortest-paths
           (let [{:keys [start n2 end path] :as current} (first open-pathoids)
                 neighbors (remove (set path) (get neighbor-map end))]
             ;; (println "current" [(:start current) (:n2 current) (:end current) (count (:path current))]
             ;;          "open-pathoids" (map (fn [p] [(:start p) (:path p)]) open-pathoids)
             ;;          "type" (type open-pathoids))
             (if (and (> (count path) 1) (some #{start} neighbors))
               (recur (reduce (fn [acc p] (disj acc p))
                              open-pathoids
                              (filter (fn [{p-start :start p-n2 :n2}] (and (= p-start start) (= p-n2 n2))) open-pathoids))
                      (assoc shortest-paths [start n2] (inc (count path)))
                      new-max-short-paths)
               (recur (apply conj (disj open-pathoids current) (map #(extend-pathoid current %) (remove #{start} neighbors)))
                      shortest-paths
                      new-max-short-paths)))))))))

(defn find-path [start end neighbor-map]
  (loop [paths [[start]]]
    (let [finished (first (filter #(= (last %) end) paths))]
      (if (some? finished)
        finished
        (recur
         (for [path paths
               :let [p-last (last path)
                     p-pre-last (last (butlast path))]
               next (get neighbor-map p-last)
               :when (not= next p-pre-last)]
           (vec (conj path next))))))))

(defn approximate-heat-map [input n-count]
  (let [connections (parse-input input)
        nodes (set (apply concat connections))
        neighbor-map (make-neighbor-map nodes connections)
        all-pairs (pairs nodes)
        pairs (take n-count (shuffle all-pairs))
        paths (for [[start end] pairs]
                (find-path start end neighbor-map))]
    (println "Testing" n-count "of" (count all-pairs) "pairs.")
    (reduce (fn [acc path]
              (reduce (fn [acc pair] (update acc pair (fnil inc 0)))
                      acc
                      (map set (partition 2 1 path))))
            {}
            paths)))

(defn find-groups [connections]
  (let [nodes (set (apply concat connections))
        neighbor-map (make-neighbor-map nodes connections)]
    (reduce (fn [acc node]
              (let [neighbors (get neighbor-map node)
                    neighbor-groups (filter #(some neighbors %) acc)]
                (case (count neighbor-groups)
                  0 (conj acc #{node})
                  1 (let [group (first neighbor-groups)]
                      (conj (disj acc group) (conj group node)))
                  (let [merged (conj (apply set/union neighbor-groups) node)]
                    (conj (apply disj acc neighbor-groups) merged)))))
            #{}
            nodes)))

(defn compute-group-size-product [input n-count]
  (let [heat-map (approximate-heat-map input n-count)
        hottest (map first (take 3 (reverse (sort-by second heat-map))))
        connections (parse-input input)
        remaining-connections (apply disj connections hottest)
        groups (find-groups remaining-connections)
        group-count (count groups)]
    (println "Group sizes:" (for [group groups] (count group)))
    (if (= 2 group-count)
      (apply * (for [group groups] (count group))))))


;; (time (compute-group-size-product large-input 100))
;; Testing 100 of 1211346 pairs.
;; Group sizes: (1557)
;; "Elapsed time: 45155.406 msecs"
;; nil

;; (time (compute-group-size-product large-input 105))
;; Testing 105 of 1211346 pairs.
;; Group sizes: (778 779)
;; "Elapsed time: 71146.2442 msecs"
;; 606062

