(ns advent-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

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

(defn parse-fresh-range [range]
  (map parse-long (str/split range #"-")))

(defn parse-input [input]
  (let [[fresh-ranges _ ingredient-ids] (->> input
                                             (read-lines)
                                             (partition-by #{""}))]
    {:fresh-ranges (map parse-fresh-range fresh-ranges)
     :ingredient-ids (map parse-long ingredient-ids)}))
    

;; Part 1
;; How many of the available ingredient ids are fresh?
(defn in-fresh-range? [id [range-min range-max]]
  (and (>= id range-min)
       (<= id range-max)))

(deftest test-in-fresh-range
  (is (= false (in-fresh-range? 2 [3 5])))
  (is (= true (in-fresh-range? 3 [3 5])))
  (is (= true (in-fresh-range? 4 [3 5])))
  (is (= true (in-fresh-range? 5 [3 5])))
  (is (= false (in-fresh-range? 6 [3 5]))))

(defn count-fresh-ingredients [input]
  (let [{:keys [fresh-ranges ingredient-ids]}
        (parse-input input)]
    (count
     (remove nil?
             (for [id ingredient-ids]
               (some #(in-fresh-range? id %) fresh-ranges))))))

;; (time (count-fresh-ingredients small-input))
;; "Elapsed time: 0.8964 msecs"
;; 3
;; (time (count-fresh-ingredients large-input))
;; "Elapsed time: 23.6454 msecs"
;; 513


;; Part 2
;; How many fresh ingredient ids are there?
(defn overlap? [[an ax] [bn bx]]
  (or (<= bn an bx)
          (<= bn ax bx)
          (<= an bn ax)
          (<= an bx ax)))

(deftest test-overlap
  (is (true? (overlap? [3 5] [4 6]))
      (false? (overlap? [3 5] [6 8]))))

(defn merge-ranges [[an ax :as a] [bn bx :as b]]
  (if (overlap? a b)
    [(min an bn) (max ax bx)]
    b))

(deftest test-merge-ranges
  (is (= [[12 20]] (merge-ranges [16 20] [12 18])))
  (is (= [3 4] (merge-ranges [1 2] [3 4]))))

(defn merge-all-ranges [fresh-ranges]
  (loop [first-range (first fresh-ranges)
           rest-ranges (rest fresh-ranges)
           result []]
      (if (empty? rest-ranges)
        (conj result first-range)
        (let [merged (map #(merge-ranges first-range %) rest-ranges)
              were-merged? (some true? (map #(overlap? first-range %) merged))]
          (if were-merged?
            (recur (first merged) (rest merged) result)
            (recur (first rest-ranges) (rest rest-ranges) (conj result (vec first-range))))))))

(defn count-fresh-ids [input]
  (letf [fresh-ranges (sort (map vec (:fresh-ranges (parse-input input))))]
    (reduce +
            (map #(inc (- (second %) (first %))) (merge-all-ranges fresh-ranges)))))

;; 385822127926476 -> too high
;; 389186782965059 -> too high
;; 339668510830757

;; (time (count-fresh-ids small-input))
;; "Elapsed time: 1.2596 msecs"
;; 14
;; (time (count-fresh-ids large-input))
;; "Elapsed time: 5.6614 msecs"
;; 339668510830757
