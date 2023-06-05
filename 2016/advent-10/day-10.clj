(ns day-10.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Bots handling microchips

(def test-input '("value 5 goes to bot 2"
                  "bot 2 gives low to bot 1 and high to bot 0"
                  "value 3 goes to bot 1"
                  "bot 1 gives low to output 1 and high to bot 0"
                  "bot 0 gives low to output 2 and high to output 0"
                  "value 2 goes to bot 2"))

(def real-input (str/split-lines (slurp "input-10.txt")))

(def value-re #"value (\d+) goes to bot (\d+)")

(def give-re #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")
;; Bot 2 (2,5)
;; Bot 1 (3)
;;      Bot 2
;;   Bot 1 Bot 0
;; Out 1 Bot 0
;;     Out 2 Out 0
;;
;; Bot 2 compares 2 and 5
;; Bot 1 compares 2 and 3
;; Bot 0 compare 3 and 5
(defn parse-values [input]
  (reduce
   (fn [bots line]
     (let [[_ val bot] (re-matches value-re line)]
       (if val
         (update bots (Long/parseLong bot) (fn [v] (conj v (Long/parseLong val))))
         bots)))
   {}
   input))

(defn make-dest [type dest]
  (if (= type "bot") (Long/parseLong dest) (str/join (list "out-" dest))))

(defn parse-connections [input]
  (reduce
   (fn [cnxns line]
     (let [[_ bot low-type low-dest high-type high-dest] (re-matches give-re line)]
       (if bot
         (assoc cnxns
                (Long/parseLong bot)
                (list (make-dest low-type low-dest) (make-dest high-type high-dest)))
         cnxns)))
   {}
   input))

(defn process [input]
 (loop [connections (parse-connections input)
        bots (parse-values input)
        bot-jobs '()]
   (if (empty? (remove #(and (string? %) (str/starts-with? % "out")) (keys bots)))
     (list (sort bots) (sort bot-jobs))
     (let [with-two (filter (fn [[k v]] (= 2 (count v))) bots)
           distributed (reduce
                        (fn [bots [bot vals]]
                          (let [v1 (apply min vals)
                                v2 (apply max vals)
                                [dest1 dest2] (get connections bot)]
                            (-> bots
                                (update dest1 #(conj % v1))
                                (update dest2 #(conj % v2))
                                )))
                        bots
                        with-two)
           bots-remaining (apply dissoc distributed (keys with-two))]
       (recur connections bots-remaining (concat bot-jobs with-two))))))

;; (process test-input)
;; => ((["out-0" (5)] ["out-1" (2)] ["out-2" (3)])
;;     ([0 (3 5)] [1 (2 3)] [2 (2 5)]))


;; (process real-input)
;; => (( ... [101 (61 17)] ... ))



;; Part 2
;; Multiply the values of outputs 0, 1, and 2
;; (apply *
;;  (map (comp first second)
;;       (filter (fn [[name [number]]]
;;                 (or (= name "out-0") (= name "out-1") (= name "out-2")))
;;               (first (process real-input)))))
;; => 37789
