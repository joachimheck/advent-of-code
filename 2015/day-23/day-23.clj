(ns day-22.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(defn read-input [f]
  (vec
   (map
    (fn [line]
      (let [[_ inst reg dist] (re-matches #"(\w+) ([ab])?,? ?([+-]\d+)?" line)]
        (cond-> {:inst inst}
          (not (nil? reg)) (assoc :reg reg)
          (not (nil? dist))
          (assoc :dist (* (Long/parseLong (subs dist 1)) (if (str/starts-with? dist "-") -1 1))))))
    (str/split-lines (slurp f)))))

;;(read-input "input-23.txt")

(defn run-program [program]
  (loop [{ip "ip" a "a" b "b" :as state} {"ip" 0 "a" 1 "b" 0}]
    (if (>= ip (count program))
      (list :done state)
      (let [{inst :inst reg :reg dist :dist} (nth program ip)]
        (recur
         (case inst
           "hlf" (-> state
                     (update reg #(quot % 2))
                     (update "ip" inc))
           "tpl" (-> state
                     (update reg #(* % 3))
                     (update "ip" inc))
           "inc" (-> state
                     (update reg inc)
                     (update "ip" inc))
           "jmp" (update state "ip" #(+ % dist))
           "jie" (if (even? (get state reg))
                   (update state "ip" #(+ % dist))
                   (update state "ip" inc))
           "jio" (if (= 1 (get state reg))
                   (update state "ip" #(+ % dist))
                   (update state "ip" inc))))
        ))))

;; (run-program (read-input "input-23.txt"))
;; => (:done {"ip" 46, "a" 1, "b" 170})


;; Part 2
;; Register a starts set to 1.
;; (run-program (read-input "input-23.txt"))
;; => (:done {"ip" 46, "a" 1, "b" 247})

