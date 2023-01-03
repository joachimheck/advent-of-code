(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What are the top crates after moving crates around?

(defn filter-spaces [c]
  (vec (filter #(not (= \space %)) c)))

(defn parse-crates [crate-lines]
  (mapv filter-spaces
       (apply mapv vector ; Mystery code to transpose an array.
              (map #(take-nth 4 (subs % 1)) (drop 1 (reverse crate-lines))))))

(defn parse-moves [move-lines]
  (let [move-regex #"move (\d+) from (\d+) to (\d+)"]
    (map (fn [line]
           (let [match (re-matcher move-regex line)]
             (re-find match)
             (map #(Long/parseLong %) (drop 1 (re-groups match))))) move-lines)))

(defn parse-input [f]
  (let [input-parts (partition-by empty? (read-lines f))]
    (list
     (parse-crates (first input-parts))
     (parse-moves (last input-parts)))))

(defn move-once [crates from-index to-index]
  (let [from-index (- from-index 1)
        to-index (- to-index 1)
        from (get crates from-index)
        removed (last from)
        new-from (vec (drop-last from))
        to (get crates to-index)
        new-to (conj to removed)]
    (assoc crates from-index new-from to-index new-to)))

(defn move [crates number from-index to-index]
  (if (= number 0) crates
      (move (move-once crates from-index to-index) (- number 1) from-index to-index)))

(defn move-crates [f]
  (let [[crates moves] (parse-input f)]
    (reduce (fn [crates [number from-index to-index]]
              (move crates number from-index to-index))
            crates moves)))

;; (apply str (map last (move-crates small-input)))
;; "CMZ"
;; (apply str (map last (move-crates large-input)))
;; "NTWZZWHFV"


;; Part 2
;; The crane is a CraneMover 9001! It can move stacks of crates at once.
(defn move-9001 [crates number from-index to-index]
  (let [from-index (- from-index 1)
        to-index (- to-index 1)
        from (get crates from-index)
        removed (subvec from (- (count from) number))
        new-from (subvec from 0 (- (count from) number))
        to (get crates to-index)
        new-to (apply conj to removed)]
    (assoc crates from-index new-from to-index new-to)))

(defn move-crates-9001 [f]
  (let [[crates moves] (parse-input f)]
    (reduce (fn [crates [number from-index to-index]]
              (move-9001 crates number from-index to-index))
            crates moves)))

;; (apply str (map last (move-crates-9001 small-input)))
;; "MCD"
;; (apply str (map last (move-crates-9001 large-input)))
;; "BRZGFVBTJ"
