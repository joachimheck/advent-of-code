(ns advent-09.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn print-disk [disk]
  (str/join
   (map #(if (nil? %) "." (format "%d" %)) disk)))

(defn parse-input [input]
  (->> input
       (read-lines)
       (first)
       (vec)
       (map str)
       (map parse-long)
       (reduce (fn [acc v]
                 (-> acc
                     (assoc-in [:space] (not (:space acc)))
                     (update-in [:n] (if (:space acc) identity inc))
                     (assoc-in [:disk] (apply conj (:disk acc) (repeat v (if (:space acc) nil (:n acc)))))
                     ))
               {:space false
                :n 0
                :disk []})
       (:disk)))

;; Part 1
;; Compact the disk by moving file blocks from the end into the empty spaces at the beginning.
(defn compact [disk]
  (let [size (count disk)]
    (loop [disk disk reversed (vec (reverse disk))]
      ;; (Thread/sleep 500)
      ;; (println "disk" (print-disk disk) "reversed" (print-disk reversed))
      (if (not (some nil? (drop-while nil? reversed)))
        disk
        (let [open-index (count (take-while #(some? %) disk))
              empty-end (take-while nil? reversed)
              last-index (dec (- (count disk) (count empty-end)))]
          (recur (-> disk
                     (assoc open-index (get disk last-index))
                     (assoc last-index nil))
                 (-> reversed
                     (assoc (dec (- size open-index)) (get disk last-index))
                     (assoc (dec (- size last-index)) nil))))))))


(defn compute-checksum [disk]
  (apply +
         (for [i (range (count disk))]
           (* i (or (get disk i) 0)))))

(defn compact-and-compute-checksum [input]
  (compute-checksum (compact (parse-input input))))


;; (time (compact-and-compute-checksum small-input))
;; "Elapsed time: 0.8518 msecs"
;; 1928
;; (time (compact-and-compute-checksum large-input))
;; "Elapsed time: 226700.9467 msecs"
;; 6288707484810


;; Part 2
;; Try to move whole files when there is space.
(defn to-disk [filemap]
  (vec
   (apply concat
          (for [[idx [n l] :as file] (sort filemap)]
            (repeat l n)))))

(defn print-filemap [filemap]
  (print-disk (to-disk filemap)))

(defn compact-and-compute-checksum-2 [input]
  (let [disk (parse-input input)
        parts (partition-by identity disk)
        filemap (dissoc
                  (reduce (fn [acc v]
                            (-> acc
                                (update :idx #(+ % (count v)))
                                (assoc (:idx acc) [(first v) (count v)])))
                          {:idx 0}
                          parts)
                  :idx)
        file-locations (filter (fn [[_ [n _]]] (some? n)) (reverse (sort filemap)))
        compacted (reduce (fn [acc [idx [n len] :as v]]
                            ;; (println "fn" (print-filemap acc) v)
                            (let [[s-i [s-n s-l] :as space] (first (filter (fn [[s-i [s-n s-l]]]
                                                                             (and (nil? s-n)
                                                                                  (< s-i idx)
                                                                                  (>= s-l len))) (sort acc)))]
                              (if space
                                (let [acc (assoc acc s-i [n len])
                                      acc (assoc acc idx [nil len])
                                      acc (if (< len s-l)
                                            (assoc acc (dec (+ s-i len)) [nil (- s-l len)])
                                            acc)]
                                  acc)
                                acc)))
                          filemap
                          file-locations)]
    (compute-checksum (to-disk compacted))))

;; (time (compact-and-compute-checksum-2 small-input))
;; "Elapsed time: 0.9504 msecs"
;; 2858
;; (time (compact-and-compute-checksum-2 large-input))
;; "Elapsed time: 187997.0945 msecs"
;; 6311837662089
