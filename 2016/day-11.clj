(ns day-11.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
;; Move microchips to the fourth floor.

(def test-input
  {3 '("LiG")
   2 '("HG")
   1 '("E" "HM" "LiM")})

(def real-input
  {3 '("CoM" "CmM" "RuM" "PuM")
   2 '("CoG" "CmG" "RuG" "PuG")
   1 '("E" "PmG" "PmM")})

(defn print-state [state]
  (for [i (reverse (range 1 5))]
    (let [floor (get state i)]
     (apply println
            (concat (list (str/join (list "F" (str i))))
                    (if (some #{"E"} floor) "E" " ")
                    (remove #{"E"} floor))))))

(defn elevator-floor [state]
  (first
   (for [i (range 1 5)
         :when (some #{"E"} (get state i))]
     i)))

(defn adjacent-floors [floor]
  (cond (= floor 1) '(2)
        (= floor 2) '(1 3)
        (= floor 3) '(2 4)
        (= floor 4) '(3)))

(defn contents [state floor]
  (remove #{"E"} (get state floor)))

(defn microchips [contents]
  (filter #(str/ends-with? % "M") contents))

(defn generators [contents]
  (filter #(str/ends-with? % "G") contents))

(defn matching-item [item]
  (let [[_ element type] (re-matches #"(\w+)([G|M])" item)]
    (if (= type "G")
      (str/join (list element "M"))
      (str/join (list element "G")))))

(defn moveables [contents]
  (concat (map list contents)
          (distinct
           (for [item (map matching-item contents)
                 :when (some #{item} contents)]
             (sort (list item (matching-item item)))))))

(defn move [state items start-floor end-floor]
  (-> state
      (update start-floor (fn [floor] (remove (set (concat items '("E"))) floor)))
      (update end-floor (fn [floor] (concat '("E") (sort (concat items floor)))))))

(defn valid? [state]
  (reduce #(and %1 %2)
          true
          (map
           (fn [floor]
             (or (empty? (generators floor))
                 (every? 
                  (set (generators floor))
                  (map matching-item (microchips floor)))
                 )
             )
           (vals state))))

(defn next-states [state]
 (let [start-floor (elevator-floor state)
       end-floors (adjacent-floors start-floor)
       contents (contents state start-floor)]
   (for [f end-floors
         items (moveables contents)
         :let [new-state (move state items start-floor f)]
         :when (valid? new-state)]
     new-state)))

;; Next steps: add function to detect the desired end state,
;; brute force it by producing next-states until we see the end state.
;;(map next-states (flatten (map next-states (next-states test-input))))


