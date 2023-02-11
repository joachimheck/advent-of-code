(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 14: Disk Defragmentation

;; Part 1
;; How many squares are used in the grid created from a given knot hash?

;; From day 10 ---v
(defn twist-numbers
  ([numbers lengths]
   (twist-numbers numbers lengths 0 0))
  ([numbers lengths position skip]
   (loop [numbers numbers lengths lengths position position skip skip]
     (if (empty? lengths)
       {:numbers numbers :position position :skip skip}
       (let [length (first lengths)
             shifted (vec (concat (subvec numbers position) (subvec numbers 0 position)))
             to-reverse (subvec shifted 0 length)
             reversed (vec (reverse to-reverse))
             replaced (apply conj reversed (subvec shifted length))
             reshifted (vec (concat (subvec replaced (- (count numbers) position)) (subvec replaced 0 (- (count numbers) position))))]
         ;;(println "shifted" shifted "replaced" replaced "reshifted" reshifted)
         (recur reshifted (rest lengths) (mod (+ position length skip) (count numbers)) (inc skip)))))))

(defn to-ascii [s]
  (map (fn [c] (int c)) s))

(defn to-lengths [s]
  (concat (to-ascii s) '(17 31 73 47 23)))

(defn twist-multiple [numbers lengths]
  (loop [i 0 position 0 skip 0 numbers numbers]
    (if (= i 64)
      numbers
      (let [{:keys [numbers position skip]} (twist-numbers numbers lengths position skip)]
        (recur (inc i) position skip numbers)))))

(defn densify-hash [sparse-hash]
  (let [blocks (partition 16 sparse-hash)]
    (map (fn [numbers] (apply bit-xor numbers)) blocks)))

(defn to-hexadecimal [numbers]
  (str/join (map #(format "%02x" %) numbers)))

(defn knot-hash [s]
  (let [lengths (to-lengths s)
        sparse-hash (twist-multiple (vec (range 256)) lengths)
        dense-hash (densify-hash sparse-hash)]
    (to-hexadecimal dense-hash)))
;; From day 10 ---^

(def small-input "flqrgnkx")
(def large-input "ugkiagan")

(def hex-to-binary {\0 "0000"
                    \1 "0001"
                    \2 "0010"
                    \3 "0011"
                    \4 "0100"
                    \5 "0101"
                    \6 "0110"
                    \7 "0111"
                    \8 "1000"
                    \9 "1001"
                    \a "1010"
                    \b "1011"
                    \c "1100"
                    \d "1101"
                    \e "1110"
                    \f "1111"})

(defn to-binary [hex-n]
  (reduce (fn [acc d] (apply conj acc (get hex-to-binary d))) [] hex-n))

(defn build-grid [hash]
  (vec
   (for [j (range 128)]
     (to-binary (knot-hash (str/join (list hash "-" (str j))))))))

(defn draw-grid [grid]
  (str/join "\n"
            (for [j (range 128)]
              (str/join (map #(if (= % \1) "#" ".") (get grid j))))))

;; (time (count (filter #(= % \1) (flatten (build-grid small-input)))))
;; "Elapsed time: 12858.249 msecs"
;; 8108

;; (time (count (filter #(= % \1) (flatten (build-grid large-input)))))
;; "Elapsed time: 12703.965601 msecs"
;; 8292



;; Part 2
;; Count the contiguous regions.
(defn grid-to-map [grid]
  (let [grid-size 128
        ones (for [j (range grid-size)
                   i (range grid-size)
                   :when (= \1 (get-in grid [j i]))]
               #{[i j]})]
    (reduce #(apply conj %1 %2) ones)))

(defn neighbors [[i j]]
  #{[(dec i) j]
    [(inc i) j]
    [i (dec j)]
    [i (inc j)]})

(defn region-neighbors [region]
  (reduce #(apply conj %1 %2) #{} (map neighbors region)))

(defn build-region [points]
  (if (empty? points)
    '()
    (loop [region #{(first points)}
           points (set points)]
      (let [region-plus (set (filter points (apply conj region (region-neighbors region))))]
        (if (= region-plus region)
          region
          (recur region-plus points))))))

(defn find-regions [hash]
  (let [start-set (grid-to-map (build-grid hash))]
    (loop [open-set start-set regions #{}]
      (if (empty? open-set)
        regions
        (let [region (build-region open-set)]
         (recur (set (remove region open-set))
                (conj regions region)))))))

;; (time (count (find-regions small-input)))
;; "Elapsed time: 15253.8745 msecs"
;; 1242

;; (time (count (find-regions large-input)))
;; "Elapsed time: 15065.9306 msecs"
;; 1069
