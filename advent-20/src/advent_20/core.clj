(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def small-input-2 "resources/small-input-2.txt")
(def large-input "resources/large-input.txt")
(def large-input-2 "resources/large-input-2.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall
     (let [tiles {}]
       (map
        (fn [tile]
          (assoc tiles
                 :id (first tile)
                 :lines (rest tile)))
        (remove #(= 1 (count %)) (partition-by #(empty? %) (line-seq rdr))))))))
