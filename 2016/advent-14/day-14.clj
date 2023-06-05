(ns day-14.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;; Part 1
;; Generate keys for a one-time pad.
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn contains-triplet? [s]
 (some (fn [[a b c]] (when (= a b c) (str a))) (partition 3 1 s)))

(defn hash [salt i] (md5 (str/join (list salt (str i)))))

(defn get-nth-key [salt hash-fn n]
  (nth
   (filter
    #(not (nil? %))
    (map-indexed
     (fn [index [hash & hashes]]
       (let [triplet (contains-triplet? hash)
             quin-hash (and triplet
                            (some (fn [h] (when (str/includes? h (str/join (repeat 5 triplet))) h))
                                  hashes))
             ]
         (when quin-hash (list index hash quin-hash))))
     (partition 1001 1
                (map (partial hash-fn salt) (iterate inc 0)))))
   (dec n)))

;; (time (get-nth-key "abc" hash 64))
;; => (22728
;;     "26ccc731a8706e0c4f979aeb341871f0"
;;     "1e15d83ba7591b79ccccc2e9a22f78b8")
;; "Elapsed time: 5451.33 msecs"

;; (time (get-nth-key "ngcjuoqr" hash 64))
;; => (18626
;;     "3b9f53b34ddd6dd60113f44a9bafca29"
;;     "036b0266f5eb3ddddd1005842818cd96")
;; "Elapsed time: 4512.2786 msecs"



;; Part 2
;; Stretch the hashes.
(defn stretched-hash [salt i]
  (nth (iterate md5 (hash salt i)) 2016))

;; (time (get-nth-key "abc" (memoize stretched-hash) 64))
;; => (22551
;;     "2df6e9378c3c53abed6d3508b6285fff"
;;     "2e559978fffff9ac9c9012eb764c6391")
;; "Elapsed time: 148400.0298 msecs"

;; (time (get-nth-key "ngcjuoqr" (memoize stretched-hash) 64))
;; => (20092
;;     "0192059360a06c82e5444db43320d17a"
;;     "64c6044444d7a72d8c37d8bb2d4a90fc")
;; "Elapsed time: 135473.7492 msecs"
