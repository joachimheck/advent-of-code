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
        keys-by-position (find-matching grid #"[a-z]")
        keys (map second keys-by-position)
        doors (map second (find-matching grid #"[A-Z]"))]
    {:pos start
     :keys (set keys)
     :doors (set doors)
     :grid (assoc grid start \.)
     :steps 0
     :path []
     :key-positions (assoc (set/map-invert keys-by-position) nil start)}))

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
      (let [adjacent (distinct (apply concat (map neighbors open-set)))
            accessible (remove visited (filter #(re-matches #"\." (str (get grid %))) adjacent))
            new-keys (apply merge keys (map (fn [pos] [(get grid pos) {:steps steps :pos pos}]) (filter #(re-matches #"[a-z]" (str (get grid %))) adjacent)))]
        (recur accessible (apply conj visited open-set) new-keys (inc steps))))))

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

(defn use-keys [keys grid]
  (reduce (fn [acc k] (use-key k acc)) grid keys))

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
      (let [adjacent (distinct (apply concat (map neighbors open-set)))
            accessible (remove visited (filter #(re-matches #"[A-Za-z\.]" (str (get grid %))) adjacent))]
       (recur accessible
              (apply conj visited open-set)
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
         finished '()
         i 0]
    ;; (println "collect-keys loop" (count states) (count finished))
    ;; (println "states" (map #(list (:keys %) (:steps %)) states))
    ;; (println "finished" (map #(:steps %) finished))
    (if (= 0 (mod i 1000))
      (println "iteration" i "states" (count states)))
    (cond (empty? states)
          (let [result (first (doall (sort-by :steps finished)))]
            ;; (println "finished" finished)
            (list "complete in" i "steps:" (:steps result) (:path result)))
          ;; (= i 200)
          ;; (list "iteration limit states:" (reverse (sort-by first (map (fn [s] (list (:steps s) (:path s))) states))))
          :else
          (let [state (first (doall (sort-by :steps states)))
                ;; _ (println "state" (dissoc state :grid) (count (:grid state)))
                pos (:pos state)
                state-keys (:keys state)
                state-doors (:doors state)
                grid (:grid state)]
            (let [reachable (doall (reachable-keys pos grid))
                  ;; _ (println "reachable" reachable)
                  ;; _ (println (print-grid grid pos))
                  new-states (doall (map (fn [[k {:keys [steps]}]]
                                           (let [key-pos (first (first (doall (find-matching grid (re-pattern (str k))))))]
                                             {:pos key-pos
                                              :steps (+ (:steps state) steps)
                                              :path (conj (:path state) k)
                                              :grid (use-key k grid)
                                              :keys (disj state-keys k)
                                              :doors (disj state-doors (door-for-key k))}))
                                         reachable))
                  new-finished (doall (if (empty? reachable) (conj finished state)))]
              (recur (doall (concat (remove #{state} states) (remove #(empty? (:keys %)) new-states)))
                     (doall (concat finished (filter #(empty? (:keys %)) (conj new-states state))))
                     (inc i)))))))

(def small-input-2 "small-input-2.txt")
(def small-input-3 "small-input-3.txt")
(def small-input-4 "small-input-4.txt")
(def small-input-5 "small-input-5.txt")

(defn h-minimal [{:keys [keys pos] :as node} goal grid]
  (- (count goal) (count keys)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn h-distance [{:keys [keys pos] :as node} goal grid]
  (->> (find-matching grid #"[a-z]")
       (remove #(some #{(second %)} keys))
       (map first)
       (map #(manhattan-distance pos %))
       (apply +)))

(defn h-steps [{:keys [keys pos] :as node} goal grid]
  (->> (find-matching (use-keys keys grid) #"[a-z]")
       (remove #(some #{(second %)} keys))
       (map first)
       (map #(steps pos % grid))
       (apply +)))

(defn h-steps-and-keys [{:keys [keys pos] :as node} goal grid]
  (let [keys-left (find-matching (use-keys keys grid) #"[a-z]")]
   (->> keys-left
        (remove #(some #{(second %)} keys))
        (map first)
        (map #(steps pos % grid))
        (apply +)
        (+ (* 100 (count keys-left))))))

(defn h-flood [{:keys [keys pos] :as node} goal grid]
  (let [keys-left (find-matching (use-keys keys grid) #"[a-z]")]
    (loop [open-set (list pos)
           visited #{}
           keys-found #{}
           steps 0]
      (cond
        (set/subset? goal (apply conj keys keys-found))
        steps
        (empty? open-set)
        ;;keys-found
        steps
        :else
        (let [adjacent (distinct (apply concat (map neighbors open-set)))
              new-keys (set (map first (filter #(re-matches #"[a-z]" %) (map #(str (get grid %)) adjacent))))
              accessible (remove visited (filter #(re-matches #"[A-Za-z\.]" (str (get grid %))) adjacent))]
          (recur accessible
                 (apply conj visited open-set)
                 (apply conj keys-found new-keys)
                 (inc steps)))))))

(defn h-closest [{:keys [keys pos] :as node} goal grid]
  (apply min (map :steps (vals (reachable-keys pos grid)))))

(defn h-minimal-times-closest [{:keys [keys pos] :as node} goal grid]
  (* (h-minimal node goal grid) (h-closest node goal grid)))

(defn reconstruct-path [froms start goal]
  ;; (println "reconstruct-path" froms)
  (loop [path (list {:from goal :cost 0})
         cost 0
         ordered-keys []]
    (let [head (:from (first path))]
      (if (= head start)
        {:cost cost
         :path (reverse (apply concat ordered-keys))}
        (let [{next-node :from to :to next-cost :cost :as combined} (get froms head)]
          (recur (conj path combined) (+ cost next-cost) (conj ordered-keys (remove (set (:keys next-node)) (:keys to)))))))))

(defn neighbors-by-steps [current-keys current-pos grid]
  (into {}
        (map (fn [[k {:keys [steps pos]}]]
               [{:keys (conj current-keys k) :pos pos} steps])
             (reachable-keys current-pos grid))))

(defn process-node [{current-keys :keys current-pos :pos :as current} open-set f-scores g-scores froms goal h-fn grid]
  (let [grid (use-keys current-keys grid)]
    (loop [neighbors (neighbors-by-steps current-keys current-pos grid)
           open-set (disj open-set current)
           f-scores f-scores
           g-scores g-scores
           froms froms]
      (if (empty? neighbors)
        [open-set f-scores g-scores froms]
        (let [[neighbor steps] (first neighbors)
              tentative-g-score (+ (get g-scores current) steps)]
          (if (< tentative-g-score (get g-scores neighbor Integer/MAX_VALUE))
            (let [new-f-score (+ tentative-g-score (h-fn neighbor goal grid))]
             (recur (rest neighbors)
                    (conj open-set neighbor)
                    (assoc f-scores neighbor new-f-score)
                    (assoc g-scores neighbor tentative-g-score)
                    (assoc froms neighbor {:from current :to  neighbor :cost steps :f-score new-f-score})))
            (recur (rest neighbors) open-set f-scores g-scores froms)))))))

(defn collect-keys-a* [state h-fn]
  ;; A node consists of the set of acquired keys, plus the current position.
  (let [grid (:grid state)
        start {:keys #{} :pos (:pos state)}
        goal (:keys state)]
   (loop [open-set #{start}
          f-scores {start (h-fn start goal grid)}
          g-scores {start 0}
          froms {}]
     (if (empty? open-set)
       :error-no-open-nodes
       (let [current (first (sort-by #(get f-scores %) open-set))]
         ;; (println "a*" "current" current (get f-scores current))
         ;; (println "f-scores" f-scores)
         ;; (newline)
         (if (= goal (set (:keys current)))
           (reconstruct-path froms start current)
           (let [[open-set f-scores g-scores froms] (process-node current open-set f-scores g-scores froms goal h-fn grid)]
             (recur open-set f-scores g-scores froms))))))))

;; I think h-minimal is admissive and consistent but I get the wrong answer for this:
;; (time (collect-keys-a* (parse-input small-input-4) h-minimal))
;; and it doesn't halt for small-input-3.


;; Ok, so Reddit tells me I'm barking up the wrong tree. I should just be doing a depth-first search, and caching.
;; Also, maybe branching and bounding will help here, to prune sub-trees that take longer to traverse than some full path.
(defn collect-keys-cache [initial-state]
  (let [initial-grid (:grid initial-state)
        all-keys (:keys initial-state)]
   (loop [states (list initial-state)
          best-state nil
          cache {}
          i 0]
     ;; (println "collect-keys loop" (count states) (count finished))
     ;; (println "states" (map #(list (:keys %) (:steps %)) states))
     ;; (println "finished" (map #(:steps %) finished))
     (if (= 0 (mod i 1000))
       (println "iteration" i "states" (count states)))
     (if (empty? states)
       best-state
       (let [state (first states)
             current (last (:path state))
             ;; pos (:pos state)
             
             state-keys (:keys state)
             remaining-keys (apply disj all-keys state-keys)
             cached (get cache [current remaining-keys])
             grid (use-keys state-keys initial-grid)]
         (if cached
           cached ; TODO - what is this and how do I add it to my result?

           (let [pos (first (first (find-matching initial-grid (re-pattern (str current)))))
                 reachable (reachable-keys pos grid)
                 new-states (map (fn [[k {:keys [steps]}]]
                                   (let [key-pos (first (first (find-matching grid (re-pattern (str k)))))]
                                     {:pos key-pos
                                      :steps (+ (:steps state) steps)
                                      :path (conj (:path state) k)
                                      :keys (disj state-keys k)}))
                                 reachable)
                 new-cache (if (= (count remaining-keys) 1)
                             (assoc cache [current remaining-keys] (:steps (first new-states))))




                 ]
             (recur (doall (concat (remove #{state} states) (remove #(empty? (:keys %)) new-states)))
                    (doall (concat finished (filter #(empty? (:keys %)) (conj new-states state))))
                    (inc i)))))))))

(defn shortest-path [{:keys [path steps] :as state} grid key-positions]
  (let [pos (get key-positions (last path))
        reachable (reachable-keys pos (use-keys path grid))
        remaining-keys (remove (set path) (keys key-positions))]
    (if (empty? remaining-keys)
      path
      (map (fn [k] (shortest-path {:path (conj path k) :steps (+ steps (get reachable k))} (disj remaining-keys k) grid)) remaining-keys))

    ;;(first (sort-by :steps (map (fn [key] (shortest-path (conj path key) (disj remaining-keys key) grid)) remaining-keys)))
    ))

;; (let [state (parse-input small-input)]
;;                   (shortest-path state (:grid state) (:key-positions state)))
