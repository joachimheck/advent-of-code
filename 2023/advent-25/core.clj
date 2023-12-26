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

(defn get-connection-frequencies [connections]
  (let [nodes (set (apply concat connections))
        paths (find-paths nodes connections)]
    (->> paths
         (map last)
         (map #(partition 2 1 %))
         (map (fn [p] (map #(set %) p)))
         (apply concat)
         (frequencies))))

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
  (let [connections (parse-input input)
        freqs (get-connection-frequencies connections)
        top-3 (set (map first (take 3 (reverse (sort-by second freqs)))))
        new-connections (remove top-3 connections)
        new-groups (group-nodes new-connections)]
    (apply * (map count new-groups))))
