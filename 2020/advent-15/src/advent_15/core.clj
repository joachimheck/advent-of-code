(ns advent-15.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))


;; Part 1
;; In the elves' memory game what is the 2020th number?
(defn- last-index-of [x ns]
  (last (keep-indexed (fn [index item] (when (= item x) index)) ns)))

(defn- next-number [ns]
  (let [n (last ns)
        prev (butlast ns)]
    (if (some #{n} prev)
      (conj ns (- (count prev) (last-index-of n prev)))
      (conj ns 0))))

(defn nth-number [ns n]
  (let [l (nth (iterate next-number ns) (- n (count ns)))]
    (last l)))

;; (time (nth-number [14 1 17 0 3 20] 2020))
;; "Elapsed time: 338.4856 msecs"
;; 387



;; Part 2
;; Work with much larger numbers.
(defn nth-number-map
  ([ns end-turn]
   (let [map (reduce #(assoc %1 %2 (count %1)) {} (butlast ns))]
     (nth-number-map map (last ns) (count ns) (- end-turn 1))))
  ([map n turn end-turn]
   (let [last-spoken (get map n)
         prev-turn (dec turn)
         new-n (if last-spoken (- prev-turn last-spoken) 0)]
     (if (= turn end-turn) new-n
         (recur (assoc map n prev-turn) new-n (inc turn) end-turn)))))


;; (time (nth-number-map [14 1 17 0 3 20] 2020))
;; "Elapsed time: 1.644 msecs"
;; 387
;; (time (nth-number-map [14 1 17 0 3 20] 30000000))
;; "Elapsed time: 20785.0838 msecs"
;; 6428
