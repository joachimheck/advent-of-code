(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall
     (let [tiles {}]
       (reduce
        #(assoc %1 (:id %2) %2)
        tiles
        (map
         (fn [tile-lines]
           (assoc tiles
                  :id (Long/parseLong (second (re-matches #"Tile (\d+):" (first tile-lines))))
                  :lines (rest tile-lines)))
         (remove #(= 1 (count %)) (partition-by #(empty? %) (line-seq rdr)))))))))

;; Part 1
;; Match tiles up by their borders. The borders may need to be reversed to match.
(defn borders
  ([tile] (borders tile false))
  ([tile rev?]
   (let [lines (:lines tile)
         borders (list
                  (first lines)
                  (last lines)
                  (reduce #(str %1 (first %2)) "" lines)
                  (reduce #(str %1 (last %2)) "" lines))]
     (if rev?
       (concat borders (map str/reverse borders))
       borders))))

(defn find-matching-borders [t ts]
  (list (:id t)
        (for [t2 (vals ts)
              :when (not= t2 t)
              b1 (borders t true)
              b2 (borders t2)
              :when (= b1 b2)]
          (list (:id t2) b2))))

(defn find-neighbor-ids [t ts]
  (map first (second (find-matching-borders t ts))))

(defn multiply-corners [ts]
  (->> (vals ts)
       (map #(list (:id %) (find-neighbor-ids % ts))) ; tiles/neighbors
       (filter #(= 2 (count (second %)))) ; corners (2 neighbors)
       (map first) ; id strings
       (reduce * 1)))

;; (multiply-corners (read-input small-input))
;; 20899048083289



;; Part 2
;; I didn't actually assemble the image in part 1 so I have to do that now
;; Then look for sea monsters!
(defn find-neighbors [t ts]
  (map #(get ts %) (find-neighbor-ids t ts)))

(defn with-neighbors [ts n]
  (->> (vals ts)
       (map #(list (:id %) (find-neighbors % ts))) ; tiles/neighbors
       (filter #(= n (count (second %))))
       (map #(get ts (first %)))))

(defn find-corners [ts] (with-neighbors ts 2))
(defn find-sides [ts] (with-neighbors ts 3))

;; To map the edges:
;; start at a corner
;; take a neighbor
;; find its other neighbor, etc
;; until we get back to the start tile

(defn find-next-border-tile [t prev ts]
  (first (remove #(= % prev) (find-neighbors t ts))))

(defn border-tiles-map [ts]
  (reduce
   #(assoc %1 (:id %2) %2)
   {}
   (concat (find-corners ts) (find-sides ts))))

(defn assemble-tiles [ts]
  (let [border-tiles (border-tiles-map ts)
        start-corner (first (find-corners ts))
        neighbor (first (find-neighbors start-corner border-tiles))]
    (let [next-neighbor (find-next-border-tile neighbor start-corner border-tiles)]
      (list start-corner neighbor next-neighbor))
    ))
;; TODO call this recursively, until reaching the start, to get all the tiles
    
