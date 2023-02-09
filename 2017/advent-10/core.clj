(ns advent-10.core)

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

;; Day 10: Knot Hash

;; Part 1
;; After knotting a list of numbers, what is the product of the first two numbers?

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

(defn knot-product [numbers lengths]
  (let [twisted (:numbers (twist-numbers numbers lengths))]
    (* (first twisted) (second twisted))))

;; (time (knot-product [0 1 2 3 4] '(3 4 1 5)))
;; "Elapsed time: 0.1767 msecs"
;; 12

;; (time (knot-product (vec (range 256)) '(18 1 0 161 255 137 254 252 14 95 165 33 181 168 2 188)))
;; "Elapsed time: 5.073 msecs"
;; 46600



;; Part 2
;; Compute the knot hash of ASCII input.
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

(deftest test-knot-hash
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (knot-hash "")))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (knot-hash "AoC 2017")))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (knot-hash "1,2,3")))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (knot-hash "1,2,4"))))

;; (knot-hash "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188")
;; "23234babdc6afa036749cfa9b597de1b"
