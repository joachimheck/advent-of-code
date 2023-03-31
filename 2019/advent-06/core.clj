(ns advent-06.core)

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

;; Day 6: Universal Orbit Map

;; Part 1
;; What is the total number of direct and indirect orbits in your map data?
(defn tree-from-pairs [pairs]
  (let [root (first (filter #(= "COM" (first %)) pairs))]
    (loop [tree (list (first root) (list (second root)))
           pairs (vec (remove #{root} pairs))
           i 0]
      ;; (if (and (> i 0) (= 0 (mod i 100)))
      ;;   (println i (count pairs) (.indexOf (map first pairs) "47X") (first pairs)))
      (if (empty? pairs)
        tree
        (let [[a b :as current] (first pairs)
              new-tree (walk/postwalk (fn [node]
                                        ;; (println "walk-fn" node pairs)
                                        (if (= (first node) a)
                                          (seq (conj (vec node) (list b)))
                                          node))
                                      tree)]
          (recur new-tree
                 (if (= new-tree tree)
                   (vec (conj (vec (drop 1 pairs)) (first pairs)))
                   (vec (rest pairs)))
                 (inc i)))))))

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/split % #"\)"))
       (tree-from-pairs)))

(defn path-to-node [tree target]
  (if (= target (first tree))
      (list target)
      (let [sub-paths (filter some? (map #(path-to-node % target) (rest tree)))]
        (if (empty? sub-paths)
          nil
          (concat (list (first tree)) (first (filter some? sub-paths)))))))

(defn count-orbits [tree]
  (reduce (fn [acc node]
            (+ acc (dec (count (path-to-node tree node)))))
          0
          (remove #{"COM"} (flatten tree))))

;; (time (count-orbits (parse-input small-input)))
;; "Elapsed time: 0.9522 msecs"
;; 42

;; (time (count-orbits (parse-input large-input)))
;; "Elapsed time: 28551.2425 msecs"
;; 142915



;; Part 2
;; How many steps are there between me ("YOU") and Santa ("SAN")?
(defn map-from-pairs [pairs]
  (let [[ra rb :as root] (first (filter #(= "COM" (first %)) pairs))]
    (loop [pair-map {ra (list rb) rb (list ra)}
           pairs (vec (remove #{root} pairs))]
      (if (empty? pairs)
        pair-map
        (let [[a b :as current] (first pairs)
              new-pair-map (if (get pair-map a)
                             (update pair-map a #(conj % b))
                             (assoc pair-map a (list b)))
              new-pair-map (if (get new-pair-map b)
                             (update new-pair-map b #(conj % a))
                             (assoc new-pair-map b (list a)))]
          (recur new-pair-map (rest pairs)))))))

(defn parse-input-2 [f]
  (->> f
       (read-lines)
       (map #(str/split % #"\)"))
       (map-from-pairs)))

(defn path [a b pair-map]
  (loop [paths [[a]]]
    (let [current-path (first paths)
          end (last current-path)
          previous (last (drop-last current-path))
          next-nodes (remove #{previous} (get pair-map end))]
      (if (some #{b} next-nodes)
        (conj current-path b)
        (recur (apply conj (rest paths) (map #(conj current-path %) next-nodes)))))))

;; (let [pair-map (-> (parse-input-2 small-input)
;;                                    (update "I" #(conj % "SAN"))
;;                                    (update "K" #(conj % "YOU"))
;;                                    (assoc "SAN" '("I"))
;;                                    (assoc "YOU" '("K")))]
;;                   (- (count (path "YOU" "SAN" pair-map)) 3))
;; 4

;; (- (count (path "YOU" "SAN" (parse-input-2 large-input))) 3)
;; 283
