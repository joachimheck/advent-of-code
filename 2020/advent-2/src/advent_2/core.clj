(ns advent-2.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defrecord Entry [x1 x2 ltr pwd])

(defn make-entry [[s1 s2 ltr pwd]]
  (->Entry (Long/parseLong s1) (Long/parseLong s2) (first (seq ltr)) pwd))

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (map (fn [line]
           (let [matches (re-matches #"(\d+)-(\d+) (.): (.+)" line)]
             (make-entry (rest matches))))
         (doall (line-seq rdr)))))

;; Part 1
;; Count passwords in which the number of occurrances of the letter
;; is between the two numbers.
(defn validate-1 [^Entry e]
  (<= (:x1 e) (count (filter #{(:ltr e)} (:pwd e))) (:x2 e)))

;; (count (filter validate-1 (read-input large-input)))
;; 560



;; Part 2
;; Count passwords in which the letter appears at one of the given positions.
(defn xor [a b]
  (or (and a (not b)) (and (not a) b)))

(defn validate-2 [^Entry e]
  (xor
   (= (:ltr e) (get (:pwd e) (dec (:x1 e))))
   (= (:ltr e) (get (:pwd e) (dec (:x2 e))))))
;;  (= 1 (count (keep #(when (= (:ltr e) (subs (:pwd e) (dec %) %)) %)
;;                    (list (:x1 e) (:x2 e))))))

;; (count (filter validate-2 (read-input large-input)))
;; 303
   
