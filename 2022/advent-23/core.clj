(ns advent-23.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; How many empty tiles are in the smallest rectangle that contains all the elves after ten rounds?
(defn parse-input-lines [lines]
  (reduce #(apply conj %1 %2)
          []
          (map (fn [[row elves]]
                 (map (fn [elf] [(first elf) row]) elves))
               (keep-indexed list (map (fn [line] (filter #(= \# (second %)) line))
                                       (map #(keep-indexed list %) lines))))))

(defn parse-input [f]
  (parse-input-lines (read-lines f)))

(defn print-elves [elves]
  (let [min-x (apply min (map first elves))
        max-x (apply max (map first elves))
        min-y (apply min (map second elves))
        max-y (apply max (map second elves))]
    (str/join \newline
              (for [j (range min-y (inc max-y))]
                (str/join (for [i (range min-x (inc max-x))]
                            (if (some #{[i j]} elves) "#" ".")))))))

(def mini-input
  (list
   "....."
   "..##."
   "..#.."
   "....."
   "..##."
   "....."))

(defn neighbors
  ([[x y]]
   #{[x (dec y)]
     [(inc x) (dec y)]
     [(inc x) y]
     [(inc x) (inc y)]
     [x (inc y)]
     [(dec x) (inc y)]
     [(dec x) y]
     [(dec x) (dec y)]})
  ([[x y] d]
   (case d
     "N" #{[(dec x) (dec y)]
           [x (dec y)]
           [(inc x) (dec y)]}
     "S" #{[(dec x) (inc y)]
           [x (inc y)]
           [(inc x) (inc y)]}
     "W" #{[(dec x) (dec y)]
           [(dec x) y]
           [(dec x) (inc y)]}
     "E" #{[(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]})))

(defn move [[x y] d]
  (case d
    "N" [x (dec y)]
    "S" [x (inc y)]
    "W" [(dec x) y]
    "E" [(inc x) y]))

(defn is-active? [elf elves]
  (not (empty? (some (neighbors elf) elves))))

(def initial-directions ["N" "S" "W" "E"])

(defn rotate-directions [directions]
  (conj (vec (rest directions)) (first directions)))

(defn propose-move [[x y :as elf] elves directions]
  (some identity (map (fn [d] (if (nil? (some (neighbors elf d) elves)) d)) directions)))

(defn move-elves [elves directions]
  (let [proposals (reduce (fn [r-proposals elf]
                            ;; (println "reducing" r-proposals elf)
                            (if (is-active? elf elves)
                              (let [proposal (propose-move elf elves directions)]
                                ;; (println "proposal" proposal (move elf proposal))
                                ;; (println "move to" (move elf proposal))
                                (if proposal
                                  (update r-proposals (move elf proposal) #(conj % elf))
                                  r-proposals))
                              r-proposals))
                          {}
                          elves)
        smap (reduce (fn [new-map [k v]]
                       (if (> (count v) 1) new-map (assoc new-map (first v) k)))
                     {}
                     proposals)]
    (replace smap elves)))

(defn find-positions [f max-iterations]
  (loop [elves (parse-input f)
         directions initial-directions
         i 0]
    (if (= 0 (mod i 50))
      (println "round" i))
    ;; (println (print-elves elves))
    (cond (not-any? true? (map #(is-active? % elves) elves))
          (list (inc i) elves)
          (= i max-iterations)
          (list :hit-max-rounds elves)
          :else
          (recur (move-elves elves directions) (rotate-directions directions) (inc i)))))


;; (time (count (filter #{\.} (print-elves (second (find-positions small-input 10))))))
;; "Elapsed time: 8.0214 msecs"
;; 110
;; (time (count (filter #{\.} (print-elves (second (find-positions large-input 10))))))
;; "Elapsed time: 24890.0002 msecs"
;; 4249



;; Part 2
;; 


;; (time (first (find-positions small-input Integer/MAX_VALUE)))
;; "Elapsed time: 10.9756 msecs"
;; 20
;; (time (first (find-positions large-input Integer/MAX_VALUE)))
;; "Elapsed time: 2047079.2923 msecs"
;; 980
