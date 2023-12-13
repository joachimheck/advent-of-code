(ns advent-13.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; Find the mirror line in each pattern; summarize the results.
(defn parse-pattern [input]
  (let [vectorized (mapv vec input)
        height (count vectorized)
        width (count (first vectorized))]
    {:width width
     :height height
     :grid (into {}
                 (for [j (range height)
                       i (range width)
                       :let [c (get-in vectorized [j i])]]
                   [[i j] c]))}))

(defn parse-input [input]
  (->> (read-lines input)
       (partition-by #{""})
       (remove #{'("")})
       (map parse-pattern)))

(defn pattern-to-string [{:keys [width height grid]}]
  (str/join "\n"
            (for [j (range height)]
              (str/join
               (for [i (range width)]
                 (or (get grid [i j]) \.))))))

(defn get-column [{:keys [width height grid]} c]
  (vec
   (for [j (range 0 height)]
     (get grid [c j]))))

(defn get-row [{:keys [width height grid]} r]
  (vec
   (for [i (range 0 width)]
     (get grid [i r]))))

(defn get-columns [{:keys [width height grid] :as pattern}]
  (vec
   (for [i (range 0 width)]
     (get-column pattern i))))

(defn get-rows [{:keys [width height grid] :as pattern}]
  (vec
   (for [j (range 0 height)]
     (get-row pattern j))))

(defn is-mirror? [lines la lb]
  (reduce (fn [acc n]
            (let [lines-v (vec lines)
                  left-line (get lines-v (- la n))
                  right-line (get lines-v (+ lb n))]
              ;; (println "Comparing" (- la n) left-line (+ lb n) right-line) (flush)
              (cond (or (= left-line nil) (= right-line nil)) (reduced true)
                    (not= left-line right-line) (reduced false)
                    :else true)))
          true
          (iterate inc 0)))

(defn find-mirror-lines [{:keys [width height grid] :as pattern}]
  (remove nil?
          (concat
           (let [v-lines (filter #(is-mirror? (get-columns pattern) % (inc %)) (range 0 (dec width)))]
             (map (fn [l] {:to-left (inc l)}) v-lines))
           (let [h-lines (filter #(is-mirror? (get-rows pattern) % (inc %)) (range 0 (dec height)))]
             (map (fn [l] {:above (inc l)}) h-lines)))))

(defn score-mirror-line [mirror-line]
  (+ (get mirror-line :to-left 0) (* 100 (get mirror-line :above 0))))

(defn summarize-notes [input]
  (let [patterns (parse-input input)]
    (reduce (fn [acc p]
              (let [mirror-lines (find-mirror-lines p)]
                (if (empty? mirror-lines)
                  (println "no mirror!\n" (pattern-to-string p))
                  (let [mirror-line (first (find-mirror-lines p))]
                    (+ acc (score-mirror-line mirror-line))))))
            0
            patterns)))

;; (summarize-notes small-input)
;; 405

;; (summarize-notes large-input)
;; 30575



;; Part 2
;; Fix one smudge in each pattern, then find the mirror line.
(defn find-score-without-smudge [{:keys [width height grid] :as pattern}]
  (let [mirror-line (first (find-mirror-lines pattern))
        alterations (for [i (range width) j (range height)]
                      {:width width :height height :grid (update grid [i j] {\# \. \. \#})})]
    (score-mirror-line (first (remove #{mirror-line} (mapcat #(find-mirror-lines %) alterations))))))

(defn summarize-notes-2 [input]
  (let [patterns (parse-input input)
        scores (map find-score-without-smudge patterns)]
    (apply + scores)))

;; (summarize-notes-2 small-input)
;; 400

;; (summarize-notes-2 large-input)
;; 37478
