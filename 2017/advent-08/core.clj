(ns advent-08.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 8: I Heard You Like Registers

;; Part 1
;; What's the largest value in a register after executing the instructions?
(def relation-mapping {">" ">"
                       "<" "<"
                       ">=" ">="
                       "<=" "<="
                       "==" "="
                       "!=" "not="})

(defn parse-input [f]
  (->> f
       read-lines
       (map #(re-find #"([a-z]+) (inc|dec) ([\-\d]+) if ([a-z]+) ([><=!]+) ([\-\d]+)" %))
       (map rest)
       (map (fn [[r dir amt c-v1 c-rel c-v2]]
              {:register r :direction dir :amount (parse-long amt) :relation (get relation-mapping c-rel) :v1 c-v1 :v2 (parse-long c-v2)}))))

(defn run-program [program]
  (reduce (fn [registers line]
            (let [{:keys [register direction amount relation v1 v2]} line
                  condition (load-string (str/join (list "(" relation " " (get registers v1 0) " " v2 ")")))]
              (if condition
                (as-> registers registers
                    (update registers register #(if (= direction "inc") ((fnil + 0 0) % amount) ((fnil - 0 0) % amount)))
                    (assoc registers :max (apply max (vals registers))))
                registers)))
          {:max Integer/MIN_VALUE}
          program))

;; (time (apply max (vals (run-program (parse-input small-input)))))
;; "Elapsed time: 2.4896 msecs"
;; 1

;; (time (apply max (vals (run-program (parse-input large-input)))))
;; "Elapsed time: 429.144 msecs"
;; 4877



;; Part 2
;; What's the highest register value at any time?

;; (time (:max (run-program (parse-input small-input))))
;; "Elapsed time: 2.0325 msecs"
;; 10

;; (time (:max (run-program (parse-input large-input))))
;; "Elapsed time: 407.4524 msecs"
;; 5471
