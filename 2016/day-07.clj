(ns day-07.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Which ip addresses support TLS?
(def test-input '("abba[mnop]qrst"
                  "abcd[bddb]xyyx"
                  "aaaa[qwer]tyui"
                  "ioxxoj[asdfgh]zxcvbn"
                  "wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt"))

(defn find-abbas [s]
  (filter
   identity
   (map-indexed
    (fn [i [a b c d]]
      (when (and (= a d) (= b c) (not= a b)) (list i [a b c d])))
    (partition 4 1 s))))

(defn find-bracket-ranges [s]
  (partition 2
             (keep (fn [[i c]] (when (or (= c \[) (= c \])) i))
                   (map-indexed list s))))

(defn supports-tls? [s]
  (let [abbas (find-abbas s)
        bracket-ranges (find-bracket-ranges s)]
    (boolean (and (seq abbas)
                  (empty? (for [[pos abba] abbas
                                [low high] bracket-ranges
                                :when (< low pos high)]
                            (list abba low high)
                            ))))))

;; (count (filter true? (map supports-tls? test-input)))
;; => 2

;; (count (filter true? (map supports-tls? (str/split-lines (slurp "input-07.txt")))))
;; => 118



;; Part 2
;; Which IPs support SSL?
(def test-input-2 '("aba[bab]xyz"
                    "xyx[xyx]xyx"
                    "aaa[kek]eke"
                    "zazbz[bzb]cdb"))

(defn find-triplets [s]
  (filter identity
          (map-indexed
           (fn [i [a b c]]
             (when (and (= a c) (not= a b)) (list i [a b c])))
           (partition 3 1 s))))

(defn reverse-aba [[a b _]] [b a b])

(defn supports-ssl? [ip]
  (seq
   (let [triplets (find-triplets ip)
         bracket-ranges (find-bracket-ranges ip)
         babs (for [[pos s] triplets
                    [low high] bracket-ranges
                    :when (< low pos high)]
                [pos s])
         abas (remove (set babs) triplets)
         ]
     (for [[_ in] babs
           [_ out] abas
           :when (= in (reverse-aba out))]
       in))))

;; (count (filter identity (map supports-ssl? test-input-2)))
;; => 3

;; (count (filter identity (map supports-ssl? (str/split-lines (slurp "input-07.txt")))))
;; => 378
;; Was comparing babs with all triplets, not just abas.
;; => 260
