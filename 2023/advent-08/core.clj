(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many steps are required to move from AAA to ZZZ?
(defn parse-node [s]
  (let [[_ a l r] (re-matches #"(.{3}) = \((.{3}), (.{3})\)" s)]
    [a {\L l \R r}]))

(defn parse-input [input]
  (let [[[instructions] _ nodes] (partition-by empty? (read-lines input))]
    {:instructions instructions
     :network (into {} (map parse-node nodes))}))

(defn traverse [input]
  (let [{:keys [instructions network]} (parse-input input)]
    (loop [instructions (flatten (repeat (vec instructions)))
           current "AAA"
           steps 0]
      (if (= current "ZZZ")
        steps
        (recur (rest instructions)
               (get (get network current) (first instructions))
               (inc steps))))))

;; (traverse small-input)
;; 2

;; (traverse small-input-2)
;; 6

;; (traverse large-input)
;; 16897



;; Part 2
;; Act like a ghost. Follow multiple paths simultaneously, start and end based on the last letter.
(defn traverse-like-a-ghost [input]
  (let [{:keys [instructions network]} (parse-input input)
        starts (filter #(= \A (last %)) (keys network))]
    (loop [currents starts
           steps 0
           loop-sizes (vec (repeat (count currents) nil))]
      ;; (if (= 0 (mod steps 10000)) (println "steps" steps))
      (let [ends (filter #(= \Z (last (second %))) (map-indexed list currents))
            new-loop-sizes (reduce (fn [acc [i _]]
                                     (if (nil? (get loop-sizes i))
                                       (assoc acc i steps)
                                       acc))
                                   loop-sizes
                                   ends)]
        (if (> steps 30000) ["error, too many steps" "currents" currents "loop-sizes" loop-sizes]
            (if (every? some? new-loop-sizes)
              (* (count instructions) (apply * (map #(/ % (count instructions)) new-loop-sizes)))
              (let [instruction (get instructions (mod steps (count instructions)))]
                ;; (println "instruction" instruction "currents" currents "steps" steps)
                (recur (doall (map #(get-in network [% instruction]) currents))
                       (inc steps)
                       new-loop-sizes))))))))

;; This program actually gives the wrong answer for the test case

;; (time (traverse-like-a-ghost small-input-3))
;; "Elapsed time: 1.0707 msecs"
;; 3N

;; (time (traverse-like-a-ghost large-input))
;; "Elapsed time: 219.9673 msecs"
;; 16563603485021
