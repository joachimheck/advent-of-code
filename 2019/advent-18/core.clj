(ns advent-18.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 18: Many-Worlds Interpretation

;; Part 1
;; How many steps is the shortest path that collects all of the keys?
(defn to-map [grid]
  (into {}
        (let [width (count (first grid))
              height (count grid)]
          (for [y (range height)
                x (range width)]
            [[x y] (get-in grid [y x])]))))

(defn find-matching [grid regex]
  (filter (fn [[k v]] (re-matches regex (str v)))
          grid))

(defn parse-input [f]
  (let [grid (to-map (->> f
                          (read-lines)
                          (mapv vec)))
        width (count (first grid))
        height (count grid)
        start (first (first (find-matching grid #"@")))
        keys (map second (find-matching grid #"[a-z]"))
        doors (map second (find-matching grid #"[A-Z]"))]
    {:pos start
     :keys (set keys)
     :doors (set doors)
     :grid (assoc grid start \.)
     :steps 0}))

(defn neighbors [[x y]]
  (list [x (dec y)]
        [(inc x) y]
        [x (inc y)]
        [(dec x) y]))

(defn reachable-keys [start grid]
  (loop [open-set (list start)
         visited #{}
         keys {}
         steps 1]
    ;; (println "reachable-keys loop" "open-set" open-set "visited" visited "keys" keys "steps" steps)
    (if (empty? open-set)
      keys
      (let [raw-adjacent (distinct (apply concat (map neighbors open-set)))
            adjacent (remove visited (filter #(re-matches #"[a-z\.]" (str (get grid %))) raw-adjacent))
            new-keys (apply merge keys (map (fn [pos] [(get grid pos) steps]) (filter #(re-matches #"[a-z]" (str (get grid %))) adjacent)))]
        (recur adjacent (apply conj visited open-set) new-keys (inc steps))))))

(defn door-for-key [key]
  (char (- (int key) 32)))

(defn useable-keys [keys doors]
  (println "useable-keys" keys doors)
  (filter #(some doors (list (door-for-key %))) keys))

(defn use-key [key grid]
  (let [key-pos (first (first (find-matching grid (re-pattern (str key)))))
        door-pos (first (first (find-matching grid (re-pattern (str (door-for-key key))))))
        without-key (assoc grid key-pos \.)]
    ;; (println "use-key" key "key-pos" key-pos "door-pos" door-pos)
    (if door-pos
      (assoc grid key-pos \. door-pos \.)
      without-key)))

(defn steps [start goal grid]
  (loop [open-set (list start)
         visited #{}
         steps 0]
    (cond
      (empty? open-set)
      nil
      (some #{goal} open-set)
      steps
      :else
      (let [current (first open-set)
            new-open-set (concat (rest open-set) (remove visited (filter #(re-matches #"[a-z\.]" (str (get grid %))) (neighbors current))))]
       (recur new-open-set
              (conj visited current)
              (inc steps))))))

(defn print-grid [grid pos]
  (let [width (inc (apply max (map first (keys grid))))
        height (inc (apply max (map second (keys grid))))]
    (str/join "\n"
     (for [j (range height)]
       (str/join
        (for [i (range width)]
          (if (= [i j] pos)
            "@"
            (str (get grid [i j])))))))))

(defn collect-keys [state]
  (loop [states (list state)
         finished '()]
    ;; (println "collect-keys loop" (count states) (count finished))
    ;; (println "states" (map #(list (:keys %) (:steps %)) states))
    ;; (println "finished" (map #(:steps %) finished))
    (if (empty? states)
      (apply min (map #(:steps %) finished))
      (let [state (first states)
            ;; _ (println "state" (dissoc state :grid) (count (:grid state)))
            pos (:pos state)
            state-keys (:keys state)
            state-doors (:doors state)
            grid (:grid state)]
        (let [reachable (reachable-keys pos grid)
              ;; _ (println "reachable" reachable)
              ;; _ (println (print-grid grid pos))
              new-states (map (fn [[k steps]]
                                (let [key-pos (first (first (find-matching grid (re-pattern (str k)))))]
                                  {:pos key-pos
                                   :steps (+ (:steps state) steps)
                                   :grid (use-key k grid)
                                   :keys (disj state-keys k)
                                   :doors (disj state-doors (door-for-key k))}))
                              reachable)
              new-finished (if (empty? reachable) (conj finished state))]
          (recur (distinct (remove #(empty? (:keys %)) (concat (rest states) new-states)))
                 (filter #(empty? (:keys %)) (concat finished (conj new-states state)))))))))

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")

