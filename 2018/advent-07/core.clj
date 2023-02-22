(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 7: The Sum of Its Parts

;; Part 1
;; In what order should the instruction steps be completed?
(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." %))
       (map rest)
       (map #(map first %))
       (map (fn [[a b]] {a (list b)}))
       (apply merge-with concat)))

(defn all-instructions [m]
  (set (flatten (concat (keys m) (vals m)))))

(defn dependent-instructions [m]
  (set (flatten (vals m))))

(defn available-instructions [m]
  (into (sorted-set) (remove (dependent-instructions m) (all-instructions m))))

(defn compute-step-order [m]
  (str/join
   (loop [remaining m ordered []]
     (if (empty? remaining)
       (apply conj ordered (vec (into (sorted-set) (remove (set ordered) (all-instructions m)))))
       (let [first-instruction (first (available-instructions remaining))]
         (recur (dissoc remaining first-instruction) (conj ordered first-instruction)))))))

;; (time (compute-step-order (parse-input small-input)))
;; "Elapsed time: 1.2915 msecs"
;; "CABDFE"

;; (time (compute-step-order (parse-input large-input)))
;; "Elapsed time: 8.5558 msecs"
;; "FHMEQGIRSXNWZBCLOTUADJPKVY"



;; Part 2
;; How long will it take for multiple workers to complete the steps?
(defn time-for-task [t]
  (- (int t) 4)
  ;; (- (- (int t) 4) 60) ; for small-input.
)

(defn map-ends [m]
  (let [ends (remove (set (keys m)) (all-instructions m))]
    (apply merge m (apply merge (map (fn [e] {e '()}) ends)))))

(defn get-complete [m now]
  (map first (filter (fn [[k v]] (= v now)) m)))

(defn time-to-complete [m worker-count]
  (loop [remaining (map-ends m) time 0 completion-times {} completed []]
    (if (= (count completed) (count (all-instructions m)))
      (list time (str/join completed))
      (let [completed-now (get-complete completion-times time)
            new-completed (vec (concat completed (sort completed-now)))
            completion-times (apply dissoc completion-times new-completed)
            available-workers (- worker-count (count completion-times))
            new-remaining (apply dissoc remaining new-completed)
            tasks (take available-workers (sort (remove (set (keys completion-times)) (available-instructions new-remaining))))
            task-completion-times (into {} (map #(vector % (+ time (time-for-task %))) tasks))
            new-completion-times (apply merge (concat (list completion-times) task-completion-times))
            new-time (if (empty? new-completion-times) time (second (apply min-key second new-completion-times)))
            ]
        (recur new-remaining new-time new-completion-times new-completed)))))


;; (time (time-to-complete (parse-input small-input) 2))
;; "Elapsed time: 1.7572 msecs"
;; (15 "CABFDE")

;; (time (time-to-complete (parse-input large-input) 5))
;; "Elapsed time: 16.9242 msecs"
;; (917 "FHRXMQSNEGWZIBCLOUATDJPKVY")
