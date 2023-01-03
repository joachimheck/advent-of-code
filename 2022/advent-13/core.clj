(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; Which pairs of packets are in the right order?
(defn read-packets [f]
  (map (fn [[a b]] (list (read-string (str/replace a "," " ")) (read-string (str/replace b "," " "))))
       (remove #{'("")} (partition-by #(= "" %) (read-lines f)))))

(defn compare-values [left right]
  (if (and (int? left) (int? right))
    (cond (< left right) :correct
          (> left right) :incorrect
          :else :continue)
    (if (and (not (int? left)) (not (int? right)))
      (cond (and (empty? left) (empty? right))
            :continue
            (and (empty? left) (not (empty? right)))
            :correct
            (and (empty? right) (not (empty? left)))
            :incorrect
            :else
            (let [comparison (compare-values (first left) (first right))]
              (if (= comparison :continue)
                (compare-values (rest left) (rest right))
                comparison)))
      (if (and (int? left) (not (int? right)))
        (compare-values [left] right)
        (compare-values left [right])))))

(defn enumerate [coll]
  (map-indexed (fn [i x] (list (inc i) x)) coll))

(defn check-packet-orders [packets]
  (enumerate (map (fn [[a b]] (compare-values a b)) packets)))

;; (apply + (map first (filter #(= :correct (second %)) (check-packet-orders (read-packets small-input)))))
;; 13
;; (apply + (map first (filter #(= :correct (second %)) (check-packet-orders (read-packets large-input)))))
;; 5882



;; Part 2
;; Sort all the packets.
(defn compare-packets [left right]
  (let [result (compare-values left right)]
    (cond (= result :correct) -1
          (= result :incorrect) 1)))

(defn order-packets [f]
  (let [packets (reduce (fn [result pair] (apply conj result pair)) [] (read-packets f))
        packets (apply conj packets [[[2]] [[6]]])]
    (enumerate (sort compare-packets packets))))

;; (apply * (map first (filter (fn [[i p]] (or (= p [[2]]) (= p [[6]]))) (order-packets small-input))))
;; 140
;; (apply * (map first (filter (fn [[i p]] (or (= p [[2]]) (= p [[6]]))) (order-packets large-input))))
;; 24948
