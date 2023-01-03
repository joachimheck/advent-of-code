(ns day-21.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Beat the boss as cheaply as possible.

(def test-player {:hp 8 :damage 5 :armor 5})
(def test-boss {:hp 12 :damage 7 :armor 2})

(defn combat [player boss]
 (let [new-boss (update boss
                        :hp
                        #(- % (max 1 (- (get player :damage) (get boss :armor)))))
       new-player (update player
                          :hp
                          #(- % (max 1 (- (get boss :damage) (get player :armor)))))]
   (cond (<= (get new-boss :hp) 0) (list :player-win player new-boss)
         (<= (get new-player :hp) 0) (list :boss-win new-player boss)
         :else (recur new-player new-boss))))

;;; (combat test-player test-boss)
;; => (:player-win
;;     {:hp 2, :damage 5, :armor 5}
;;     {:hp 0, :damage 7, :armor 2})

(def real-boss {:hp 109 :damage 8 :armor 2})

(defn parse-section [section]
  (assoc {}
         (first (str/split (first section) #":"))
         (reduce
          (fn [acc line]
            (let [[_ n c d a] (re-matches #"([+\w ]+?) +(\d+) +(\d+) +(\d+)" line)] 
              (assoc acc n {:cost (Long/parseLong c) :damage (Long/parseLong d) :armor (Long/parseLong a)})))
          {}
          (rest section))))

(defn parse-input [f]
  (-> (slurp f)
      (str/split #"\R\R")
      (->>
       (map str/split-lines)
       (map parse-section)
       (reduce merge {}))))

(defn total-property [coll prop]
;; (println "total-property" coll prop)
  (->> coll
       (map second)
       (map #(get % prop))
       (reduce +)))

(defn combos [f]
 (let [input (parse-input f)]
   (for [weapon (get input "Weapons")
         armor (get input "Armor")
         left-ring (get input "Left Ring")
         right-ring (get input "Right Ring")
         :when (or (= (first left-ring) "Nothing") (not (= left-ring right-ring)))]
     (assoc {}
            :equipment (str/join " " (map first (list weapon armor left-ring right-ring)))
            :cost (total-property (list weapon armor left-ring right-ring) :cost)
            :damage (total-property (list weapon armor left-ring right-ring) :damage)
            :armor (total-property (list weapon armor left-ring right-ring) :armor)))))

;; (first
;;  (sort-by (fn [m] (get m :cost))
;;           (filter (fn [m] (= (first (get m :result)) :player-win))
;;                   (map (fn [combo]
;;                          (assoc {}
;;                                 :cost (get combo :cost)
;;                                 :result (combat {:hp 100
;;                                                  :damage (get combo :damage)
;;                                                  :armor (get combo :armor)}
;;                                                 real-boss)))
;;                        (combos "input-21.txt")))))
;; => {:cost 111,
;;     :result
;;     (:player-win
;;      {:hp 16, :damage 7, :armor 4}
;;      {:hp -1, :damage 8, :armor 2})}




;; Part 2
;; How much can I spend and still lose?

;; (first
;;  (reverse
;;   (sort-by (fn [m] (get m :cost))
;;            (filter (fn [m] (= (first (get m :result)) :boss-win))
;;                    (map (fn [combo]
;;                           (assoc {}
;;                                  :cost (get combo :cost)
;;                                  :result (combat {:hp 100
;;                                                   :damage (get combo :damage)
;;                                                   :armor (get combo :armor)}
;;                                                  real-boss)))
;;                         (combos "input-21.txt"))))))
;; => {:cost 188,
;;     :result
;;     (:boss-win
;;      {:hp 0, :damage 7, :armor 3}
;;      {:hp 14, :damage 8, :armor 2})}
