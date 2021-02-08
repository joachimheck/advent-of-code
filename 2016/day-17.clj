(ns day-17.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;; Part 1
;; Get to the vault
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def test-inputs '("hijkl" "ihgpwlah" "kglvqrro" "ulqzkmiv"))

(defn open? [s] (#{\b \c \d \e \f} s))

(defn dirs [[x y] [u d l r :as hash]]
  (keep
   (fn [[dir [x y] open]]
     (when (and (<= 0 x 3)
                (<= 0 y 3)
                open)
       (list dir [x y])))
   (list
    (list "U" [x (dec y)] (open? u))
    (list "D" [x (inc y)] (open? d))
    (list "L" [(dec x) y] (open? l))
    (list "R" [(inc x) y] (open? r)))))

(defn find-path [passcode]
  (loop [path "" pos [0 0] paths '()]
    (if (= pos [3 3]) path
        (let [hash (subs (md5 (str/join (list passcode path))) 0 4)
              new-paths (concat paths (map (fn [[dir pos]]
                                             (list (str/join (list path dir)) pos))
                                           (dirs pos hash)))
              [new-path & more-paths] new-paths]
          (if (empty? new-paths) :no-path
              (recur (first new-path) (second new-path) more-paths))))))

(def real-input "vwbaicqe")

;; (find-path real-input)
;; => "DRDRULRDRD"



;; Part 2
;; Find the longest path
(defn find-longest-path [passcode]
  (loop [path "" pos [0 0] paths '() longest 0]
    (if (= pos [3 3])
      (let [[new-path & more-paths] paths]
        (recur (first new-path) (second new-path) more-paths (max longest (count path))))
      (let [hash (subs (md5 (str/join (list passcode path))) 0 4)
              new-paths (concat paths (map (fn [[dir pos]]
                                             (list (str/join (list path dir)) pos))
                                           (dirs pos hash)))
              [new-path & more-paths] new-paths]
          (if (empty? new-paths)
            longest
            (recur (first new-path) (second new-path) more-paths longest))))))

;;(find-longest-path real-input)
;; => 384
