(ns advent-05.core)

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

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-input [input]
  (let [[order-rules _ pages] (partition-by #(= "" %)
                                            (read-lines input))]
    {:order-rules (map #(let [[_ a b] (re-find #"(\d+)\|(\d+)" %)]
                          [(parse-long a) (parse-long b)])
                       order-rules)
     :updates (map (fn [s] (->> s
                                (re-seq #"(\d+)")
                                (map second)
                                (mapv parse-long)))
                   pages)
     }
    ))


;; Part 1
;; Identify which safety updates are in the right order.

(defn partition-update [update page]
  (let [base-partition (partition-by #(= page %) update)]
    (cond (= (first base-partition) [page])
          [[] [] (last base-partition)]
          (= (last base-partition) [page])
          [(first base-partition) [] []]
          :else
          base-partition)))

(defn correct-order? [update rules]
  (every? true?
          (for [page update
                [lo hi] rules
                :when (and (or (= page lo) (= page hi))
                           (some #(= lo %) update)
                           (some #(= hi %) update))
                :let [[before _ after] (partition-update update page)]]
            (do
             (or (and (= page lo) (some #(= hi %) after))
                 (and (= page hi) (some #(= lo %) before)))))))

(defn middle-value [pages]
  (get pages (quot (count pages) 2)))

(defn sum-middle-pages-of-valid-updates [input]
  (let [{:keys [order-rules updates]} (parse-input input)]
    (->> updates
         (filter #(correct-order? % order-rules))
         (map middle-value)
         (apply +))))


;; (time (sum-middle-pages-of-valid-updates small-input))
;; "Elapsed time: 1.3344 msecs"
;; 143
;; (time (sum-middle-pages-of-valid-updates large-input))
;; "Elapsed time: 500.049 msecs"
;; 4790



;; Part 2
;; Fix the order of the invalid updates.
(defn relevant-rules [update rules]
  (for [[lo hi] rules
        :when (and (some #{lo} update)
                   (some #{hi} update))]
    [lo hi]))

(defn valid-for-rule? [update [lo hi :as rule]]
  (let [lo-pos (.indexOf update lo)
        hi-pos (.indexOf update hi)]
    (< lo-pos hi-pos)))

(defn valid? [update rules]
  (every? #(valid-for-rule? update %) rules))

(defn swap [pages v1 v2]
  (let [v1-pos (.indexOf pages v1)
        v2-pos (.indexOf pages v2)]
    ;; (println "swapping" v1 "and" v2 "in" pages "positions" v1-pos v2-pos)
    (assoc (assoc pages v1-pos v2)
           v2-pos v1)))

(defn fix-updates [updates order-rules]
  (for [pages updates
        :let [rules (relevant-rules pages order-rules)]]
    (reduce (fn [acc rule]
              (if (valid? acc rules)
                (reduced acc)
                (let [broken-rule (first (filter #(= (second %) false) (map #(list % (valid-for-rule? acc %)) rules)))]
                  ;; (println "broken rule" acc broken-rule)
                  (swap acc (first (first broken-rule)) (second (first broken-rule)))
                  )))
            pages
            rules)))

(defn sum-middle-pages-of-fixed-updates [input]
  (let [{:keys [order-rules updates]} (parse-input input)
        broken (filter #(not (valid? % (relevant-rules % order-rules))) updates)
        fixed (fix-updates broken order-rules)]
    (->> fixed
         (map middle-value)
         (apply +))))


;; (time (sum-middle-pages-of-fixed-updates small-input))
;; "Elapsed time: 5.5998 msecs"
;; 123
;; (time (sum-middle-pages-of-fixed-updates large-input))
;; "Elapsed time: 2932.1844 msecs"
;; 6319
