(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 12: Passage Pathing

;; Part 1
;; Count the number of paths through the cave system that visit small caves at most once.
(defn parse-input-lines [lines]
  (->> lines
       (map (fn [s] (re-find #"([A-z]+)-([A-z]+)" s)))
       (map rest)
       (map vec)
       (mapcat (fn [[a b]] (list [a b] [b a])))
       (reduce (fn [result [k v]]
                 (if (= v "start")
                   result
                   (update result k #(if (nil? %) (list v) (conj % v)))))
               {})))

(defn parse-input [f]
  (parse-input-lines (read-lines f)))

(def micro-caves (parse-input-lines '("start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end")))
(def mini-caves (parse-input-lines '("dc-end"
                                    "HN-start"
                                    "start-kj"
                                    "dc-start"
                                    "dc-HN"
                                    "LN-dc"
                                    "HN-end"
                                    "kj-sa"
                                    "kj-HN"
                                    "kj-dc")))

(defn lowercase? [s]
  (re-matches #"[a-z]+" s))

(defn uppercase? [s]
  (re-matches #"[A-Z]+" s))

(defn count-paths-recursive [caves path]
  ;; (println "count-paths-recursive" path)
  (let [current (last path)]
    (if (= current "end")
      1
      (apply +
             (for [neighbor (caves current)
                   :when (or (uppercase? neighbor) (not-any? #{neighbor} path))]
               (count-paths-recursive caves (conj path neighbor)))))))

;; (time (count-paths-recursive (parse-input small-input) ["start"]))
;; "Elapsed time: 6.4967 msecs"
;; 226

;; (time (count-paths-recursive (parse-input large-input) ["start"]))
;; "Elapsed time: 75.7306 msecs"
;; 4167



;; Part 2
;; There is time to visit a single small cave twice.
(defn small-caves [caves]
  (remove #{"start" "end"} (filter lowercase? (keys caves))))

(defn find-paths-recursive-2 [caves path revisit-cave]
  ;; (println "count-paths-recursive" path)
  (let [current (last path)]
    (if (= current "end")
      (list path)
      (apply concat (for [neighbor (caves current)
                          :when (or
                                 (uppercase? neighbor)
                                 (not-any? #{neighbor} path)
                                 (and (= neighbor revisit-cave) (= 1 (count (filter #{revisit-cave} path))))
                                 )]
                      (count-paths-recursive-2 caves (conj path neighbor) revisit-cave))))))

(defn count-paths-2 [caves]
  (count
   (distinct
    (apply concat
           (for [small-cave (small-caves caves)]
             (find-paths-recursive-2 caves ["start"] small-cave))))))

;; (time (count-paths-2 (parse-input small-input)))
;; "Elapsed time: 161.5464 msecs"
;; 3509

;; (time (count-paths-2 (parse-input large-input)))
;; "Elapsed time: 3941.3886 msecs"
;; 98441
