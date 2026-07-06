(ns advent-12.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (let [lines (read-lines input)
        groups (->> lines
                    (partition-by #{""})
                    (remove #{'("")})
                    (group-by #(boolean (re-matches #"\d+:" (first %))))
                    )
        shape-inputs (get groups true)
        region-inputs (filter #(re-find #"\d+x\d+" %) lines)
        ]
    {:shapes (into {}
                   (map (fn [l]
                          {(parse-long (second (re-matches #"(\d+):" (first l))))
                           (mapv vec (rest l))})
                        shape-inputs))
     :regions (map (fn [r]
                     (let [parsed (re-find #"(\d+)x(\d+): (\d+) (\d+) (\d+) (\d+) (\d+) (\d+)" r)]
                       {:width (parse-long (nth parsed 1))
                        :length (parse-long (nth parsed 2))
                        :counts (mapv parse-long (drop 3 parsed))}))
                   region-inputs)
     }))

;; Part 1
;; How many regions can fit all the desired presents?
(defn rotate [shape]
  (mapv vec
        (for [i (range 0 3)]
          (for [j (range 2 -1 -1)]
            (get-in shape [j i])))))

(defn flip [shape]
  ;; 0 1 2  2 1 0
  ;; 3 4 5  5 4 3
  ;; 6 7 8  8 7 6
  (mapv vec
        (for [j (range 0 3)]
          (for [i (range 0 3)]
            (get-in shape [j (- 2 i)])))))

(defn format-shape [shape]
  (str/join "\n"
            (for [j (range 0 3)]
              (str/join
               (for [i (range 0 3)]
                 (get-in shape [j i]))))))

(defn make-all-variations [shape]
  (if (empty? shape)
    nil
    (distinct
     (concat (take 4 (iterate rotate shape))
             (take 4 (iterate rotate (flip shape)))))))

(defn get-positions [shape]
  (remove nil?
          (apply concat
                 (for [j (range 0 3)]
                   (for [i (range 0 3)]
                     (if (= (get-in shape [j i]) \#) [i j]))))))

(defn translate [[x y] ps]
  (map (fn [[i j]]
         [(+ i x) (+ j y)])
       ps))

(defn overlap? [ps1 ps2]
  (some (set ps1) ps2))

(defn add-shape [{:keys [width length grid] :as state} shape [x y] shape-id]
  (reduce (fn [state p]
              (if (get (:grid state) p)
                (reduced nil)
                (assoc-in state [:grid p] shape-id)))
            state
            (translate [x y] (get-positions shape))))

(defn shape-size [shape]
  (count (filter #{\#} (flatten shape))))

(defn space-remaining [{:keys [width length shape-positions]}]
  (println "space-remaining" width length shape-positions)
  (- (* width length) (apply + (map count shape-positions))))

(defn format-grid [{:keys [width length grid]}]
  (str/join "\n"
            (for [j (range 0 length)]
              (str/join
               (for [i (range 0 width)]
                 (if-let [id (get grid [i j])]
                   id
                   \.))))))

(defn flip-horizontal [{:keys [width length grid] :as state}]
  (assoc state :grid
         (into {} (for [[[x y] z] grid]
                    [[(- width x) y] z]))))

(defn flip-vertical [{:keys [width length grid] :as state}]
  (assoc state :grid
         (into {} (for [[[x y] z] grid]
                    [[x (- length y)] z]))))

(defn states-equivalent? [{:keys [width length grid] :as state1} {:keys [width length grid] :as state2}]
  (or (= state1 (flip-horizontal state2))
      (= state1 (flip-vertical state2))))

(defn make-equivalent-states [{:keys [width length grid] :as state}]
  [(flip-horizontal state) (flip-vertical state) (flip-vertical (flip-horizontal state))])

(defn fit-shapes [width length shapes]
  (if (< (* width length) (apply + (map shape-size shapes)))
    :not-enough-space
    (loop [shapes shapes
           variations []
           states []
           new-states [{:width width :length length :grid {}}]
           ignore-states #{}
           id (char (dec (int \A)))
           n 0]
      (println "fit-shapes" "state count:" (count states) "new state count:" (count new-states) "ignore state count:" (count ignore-states)
               "variation count:" (count variations) "id:" id "n:" n)
      (cond (> n 100)
            :error-over-n
            (and (empty? shapes) (empty? variations))
            {:finished new-states}
            (empty? variations)
            (let [ignore-removed (remove ignore-states new-states)
                  _ (println "Removed" (- (count new-states) (count ignore-removed)) "states")]
              (recur (rest shapes) (make-all-variations (first shapes)) ignore-removed [] #{} (char (inc (int id))) (inc n)))
            :else
            (let [add-states (apply concat
                                    (apply concat
                                           (remove empty?
                                                   (for [state states]
                                                     (do
                                                       (remove #(or (nil? %) (empty? %))
                                                               (for [x (range (- width 2))]
                                                                 (remove #(or (nil? %) (empty? %))
                                                                         (for [y (range (- length 2))]
                                                                           (add-shape state (first variations) [x y] id))))))))))
                  equivalent-states (set (mapcat make-equivalent-states add-states))
                  _ (if (not (empty? add-states)) (println (format-grid (first add-states))))
                  ]
              (recur shapes (rest variations) states (concat new-states add-states) (set/union ignore-states equivalent-states) id (inc n)))))))

(defn analyze-region [shape-map {:keys [width length counts] :as region}]
  (let [shapes (apply concat
                      (for [i (range (count counts))]
                        (repeat (get counts i) (get shape-map i))))]
    (let [fitted (fit-shapes width length shapes)]
      (if (:finished fitted)
        {:region-fillable (first (:finished fitted))}
        {:region-not-fillable fitted}))))

;; (let [input (parse-input small-input)
;;                       shapes (:shapes input)
;;                       regions (:regions input)]
;;                   (time (analyze-region shapes (nth (:regions input) 1))))
;; This is very slow. Maybe I should move the states for comprehension into the loop and check n?

;; (let [input (parse-input small-input)
;;                       shapes (:shapes input)
;;                       regions (:regions input)]
;;                   (time (println (format-grid (:region-fillable (analyze-region shapes (nth (:regions input) 1)))))))

(defn fit-shapes-dfs [width length shapes]
  (let [initial-states [{:length length :width width :grid {}}]]
    (loop [shapes shapes
           id (char (int \A))
           states initial-states]
      (if (empty? shapes)
        states
        (recur (rest shapes)
               (char (inc (int id)))
               (loop [states states
                      new-states []]
                 (if (empty? states)
                   new-states
                   (recur (rest states)
                          (concat new-states
                                  (loop [variations (make-all-variations (first shapes))
                                         new-states []]
                                    (if (empty? variations)
                                      new-states
                                      (recur (rest variations)
                                             (concat new-states
                                                     (loop [xs (range (- width 2))
                                                            new-states []]
                                                       (if (empty? xs)
                                                         new-states
                                                         (recur (rest xs) 
                                                                (concat new-states
                                                                        (let [x (first xs)]
                                                                          (loop [ys (range (- length 2))
                                                                                 new-states []]
                                                                            (let [y (first ys)]
                                                                              (if (empty? ys)
                                                                                (remove nil? new-states)
                                                                                (recur (rest ys) (conj new-states (add-shape (first states) (first variations) [x y] id))))))))))))))))))))))))

(defn analyze-region-dfs [shape-map {:keys [width length counts] :as region}]
  (let [shapes (apply concat
                      (for [i (range (count counts))]
                        (repeat (get counts i) (get shape-map i))))]
    (fit-shapes-dfs width length shapes)))

(def recursion-count (atom 0))

(defn fit-shapes-recurse
  ([width length shapes max-recursion]
   (reset! recursion-count 0)
   (let [shape-ids (map-indexed (fn [i s] (vector s (char (+ (int \A) i)))) shapes)]
     (fit-shapes-recurse {:width width :length length :grid {}} shape-ids max-recursion)))
  ([{:keys [width length grid] :as state} shape-ids max-recursion]
   (if (empty? shape-ids)
     (list state)
     (let [[shape id] (first shape-ids)]
       (remove #(or (nil? %) (empty? %))
               (apply concat
                      (for [variation (make-all-variations shape)
                            x (range (- width 2))
                            y (range (- length 2))]
                        (do
                          (if (> (swap! recursion-count inc) max-recursion)
                            (throw (Exception. (format "too many iterations %d" @recursion-count))))
                          (if-let [new-state (add-shape state variation [x y] id)]
                            (fit-shapes-recurse new-state (rest shape-ids) max-recursion))))))))))

(defn analyze-region-recurse [shape-map {:keys [width length counts] :as region} max-recursion]
  (let [shapes (apply concat
                      (for [i (range (count counts))]
                        (repeat (get counts i) (get shape-map i))))
        _ (println "Maximum steps:" (long (math/pow (* 4 width length) (count shapes))))
        result (fit-shapes-recurse width length shapes max-recursion)]
    (if result
      {:c (count result) :r (first result)}
      result)))

;; (let [input (parse-input small-input)
;;                       shapes (:shapes input)
;;                       regions (:regions input)
;;                       result (analyze-region-recurse shapes (nth regions 1))]
;;                   (if result
;;                     (println "c" (:c result) "rc" @recursion-count "\n" (if (:r result) (format-grid (:r result))))))
;; Maximum steps: 64000000
;; c 0 rc 66144 
;;  nil
;; nil
;; advent-12.core> 

(defn analyze-all-recurse [input max-recursion]
  (let [initial-state (parse-input input)
        regions (:regions initial-state)
        shape-map (:shapes initial-state)]
    (println "Regions that can fit all shapes:"
             (count (remove nil?
                            (for [r (range (count regions))]
                              (let [{:keys [width length counts] :as region} (nth regions r)
                                    shapes (apply concat
                                                  (for [i (range (count counts))]
                                                    (repeat (get counts i) (get shape-map i))))
                                    ;; result (time (try (first (fit-shapes-recurse width length shapes max-recursion))
                                    ;;                   (catch Exception e (str "caught exception: " (.getMessage e)))))
                                    result (try (first (fit-shapes-recurse width length shapes max-recursion))
                                                (catch Exception e nil))]
                                result)))))))
