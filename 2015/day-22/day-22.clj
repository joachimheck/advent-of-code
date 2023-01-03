(ns day-22.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Magic RPG combat.

;; At each turn, branch into the five (or fewer) possible paths
;; by adding them to the end of a queue.
(def initial-game-state {:hp 50 :mana 500 :armor 0 :boss-hp 55 :boss-damage 8
                         :shield-timer 0 :poison-timer 0 :recharge-timer 0
                         :mana-expended 0})

(defn shield-active? [state]

  (when (nil? (:shield-timer state))
    (doall (println "broken state" state)))

 (> (:shield-timer state) 0))

(defn poison-active? [state] (> (:poison-timer state) 0))

(defn recharge-active? [state] (> (:recharge-timer state) 0))

(def spells (list
             {:name "magic missile" :cost 53
              :cast-fn (fn [state] (update state :boss-hp #(- % 4)))
              :is-active? (fn [state] false)
              :effect-fn identity}
             {:name "drain" :cost 73
              :cast-fn (fn [state] (-> state
                                       (update :boss-hp #(- % 2))
                                       (update :hp #(+ % 2))))
              :is-active? (fn [state] false)
              :effect-fn identity}
             {:name "shield" :cost 113
              :cast-fn (fn [state] (assoc state :shield-timer 6))
              :is-active? shield-active?
              :effect-fn (fn [state] (if (not (shield-active? state))
                                       (assoc state :armor 0)
                                       (-> state
                                        (assoc :armor 7)
                                        (update :shield-timer dec))))}
             {:name "poison" :cost 173
              :cast-fn (fn [state] (assoc state :poison-timer 6))
              :is-active? poison-active?
              :effect-fn (fn [state] (if (not (poison-active? state)) state
                                         (-> state
                                               (update :boss-hp #(- % 3))
                                               (update :poison-timer dec))))}
             {:name "recharge" :cost 229
              :cast-fn (fn [state] (assoc state :recharge-timer 5))
              :is-active? recharge-active?
              :effect-fn (fn [state] (if (not (recharge-active? state)) state
                                         (-> state
                                             (update :mana #(+ % 101))
                                             (update :recharge-timer dec))))}))

(defn spell-options [game-state]
  (->> spells
       (filter (fn [spell] (<= (:cost spell) (:mana game-state))))
       (remove (fn [spell] ((:is-active? spell) game-state)))))

(defn process-effects [state]
  (reduce (fn [acc spell]
            ((:effect-fn spell) acc))
          state
          spells))

(defn boss-turn [state]
  (let [damage (max 1 (- (:boss-damage state) (:armor state)))]
    (update state :hp #(- % damage))))

(defn player-turn [state]
  (let [options (spell-options state)]
    (if (empty? options)
      (list (assoc state :winner :boss))
      (for [opt options]
        (-> state
            ((:cast-fn opt))
            (update :mana #(- % (:cost opt)))
            (update :mana-expended #(+ % (:cost opt))))))))

(defn process-turn [state]
  (-> state
      ;; Hard mode
      (update :hp dec)
      ((fn [state]
         (if (<= (:hp state) 0)
           (list (assoc state :winner :boss))
           (-> state
            process-effects
            player-turn
            (->>
             (map process-effects)
             (map boss-turn)
             (map (fn [state]
                    (cond (<= (:boss-hp state) 0) (assoc state :winner :player)
                          (<= (:hp state) 0) (assoc state :winner :boss)
                          :else state))))))))))


(defn play-game [start-state]
  (loop [game-states (list start-state)]
    (println (count game-states) "game-states"
             "finished" (count (filter :winner game-states))
             "unfinished" (count (remove :winner game-states)))
    (let [finished (filter :winner game-states)
          unfinished (remove :winner game-states)
          won-by-player (filter #(= (:winner %) :player) finished)]
      (if (not (empty? won-by-player))
        (list :done (sort #(- (:mana-expended %1) (:mana-expended %2)) won-by-player))
        (if (not (empty? unfinished))
          (recur (concat finished (mapcat process-turn unfinished)))
          (list :done finished))))))


;; (time (apply min (map :mana-expended (second (play-game initial-game-state)))))
;; ;; => 953
;; "Elapsed time: 809.0714 msecs"


;; Part 2
;; Hard mode - lose 1 hp each player turn, before effects.
;; (time (apply min (map :mana-expended (second (play-game initial-game-state)))))
;; => 1289
