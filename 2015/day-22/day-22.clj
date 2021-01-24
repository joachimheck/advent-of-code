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
                         :shield-timer 0 :poison-timer 0 :recharge-timer 0})

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
  (reduce (fn [state spell] ((:effect-fn spell) state))
          state
          spells))

(defn boss-turn [state]
  (let [damage (max 1 (- (:boss-damage state) (:armor state)))]
    (update state :hp #(- % damage))))

(defn player-turn [state]
  (let [options (spell-options state)]
    (if (empty? options)
      (assoc state :winner :boss)
      (for [opt options]
        (-> state
            ((:cast-fn opt))
            (update :mana #(- % (:cost opt))))))))

(defn process-turn [state]
  (-> state
      process-effects
      player-turn
      (->>
       (map process-effects)
       (map boss-turn)
       (map (fn [state]
              (cond (<= (:boss-hp state) 0) (assoc state :winner :player)
                    (<= (:hp state) 0) (assoc state :winner :boss)
                    :else state))))))


(loop [game-states (list initial-game-state)
       i 0]
  (println i (count game-states) "game-states"
           (take 2 game-states)
           "finished" (count (filter :winner game-states))
           "unfinished" (count (remove :winner game-states)))
  (let [finished (filter :winner game-states)
        unfinished (remove :winner game-states)]
    (if (or (> i 10) (not (empty? finished))) :done
        ;;(concat '() (mapcat process-turn unfinished))
        (if (not (empty? unfinished))
          ;;(concat finished (mapcat process-turn unfinished))
          (recur (concat finished (mapcat process-turn unfinished)) (inc i))
          game-states
          )
        ))
  )
