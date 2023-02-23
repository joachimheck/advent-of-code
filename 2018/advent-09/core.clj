(ns advent-09.core)

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

;; Day 9: Marble Mania

;; Part 1
;; What is the winning score in the marble game?
(defn print-circle [circle current]
  (loop [m (get circle 0) s " 0" last-was-current false]
    (if (= m 0)
      (if last-was-current
        (str/join (list s ")"))
        s)
      (recur (get circle m)
             (str/join (list s
                             (cond (= m current) "("
                                   last-was-current ")"
                                   :else " ")
                             (format "%2d" m)))
             (= m current)))))

(defn play-marbles [player-count max-marble]
  ;; start with marble 2 because the first turn is weird.
  (loop [circle (transient {0 1 1 0})
         current-marble 1
         new-marble 2
         scores (vec (repeat player-count 0))
         current-player 1]
    ;; (println "loop" (dec new-marble) "circle size" (count circle) "circle" (print-circle circle current-marble))
    (if (> new-marble max-marble)
      {:max-score (apply max scores)}
      (if (= 0 (mod new-marble 23))
        (let [before-remove (nth (iterate (partial get circle) current-marble) (- (count circle) 8))
              to-remove (get circle before-remove)
              after-remove (get circle to-remove)
              new-circle (-> circle
                             (assoc! before-remove after-remove)
                             (dissoc! to-remove))
              new-scores (update scores current-player #(+ % new-marble to-remove))]
          (recur new-circle after-remove (inc new-marble) new-scores (inc current-player)))
        (let [after-marble (get circle current-marble)
              after-after (get circle after-marble)
              new-circle (-> circle
                             (assoc! new-marble after-after)
                             (assoc! after-marble new-marble))]
          (recur new-circle new-marble (inc new-marble) scores (mod (inc current-player) player-count)))))))

(deftest test-play-marbles
  (is (= 32 (:max (play-marbles 9 25))))
  (is (= 8317 (:max (play-marbles 10 1618))))
  (is (= 146373 (:max (play-marbles 13 7999))))
  (is (= 2764 (:max (play-marbles 17 1104))))
  (is (= 54718 (:max (play-marbles 21 6111))))
  (is (= 37305 (:max (play-marbles 30 5807)))))

;; (time (play-marbles 9 25))
;; "Elapsed time: 0.2873 msecs"
;; {:max-score 32}

;; (time (play-marbles 491 71058))
;; "Elapsed time: 27261.041 msecs"
;; {:max-score 361466}



;; Part 2
;; What about 491 players and 7105800 marbles?
(defn play-marbles-prev [player-count max-marble]
  ;; start with marble 2 because the first turn is weird.
  (loop [circle (transient {0 '(1 1) 1 '(0 0)})
         current-marble 1
         new-marble 2
         scores (vec (repeat player-count 0))
         current-player 1]
    ;; (println "loop" (dec new-marble) "circle size" (count circle) "circle" (print-circle circle current-marble))
    (if (> new-marble max-marble)
      {:max-score (apply max scores)}
      (if (= 0 (mod new-marble 23))
        (let [to-remove (nth (iterate #(first (get circle %)) current-marble) 7)
              before-remove (first (get circle to-remove))
              after-remove (second (get circle to-remove))
              new-circle (-> circle
                             (assoc! before-remove (list (first (get circle before-remove)) after-remove))
                             (assoc! after-remove (list before-remove (second (get circle after-remove))))
                             (dissoc! to-remove))
              new-scores (update scores current-player #(+ % new-marble to-remove))]
          (recur new-circle after-remove (inc new-marble) new-scores (inc current-player)))
        (let [after-marble (second (get circle current-marble))
              after-after (second (get circle after-marble))
              new-circle (-> circle
                             (assoc! after-marble (list (first (get circle after-marble)) new-marble))
                             (assoc! new-marble (list after-marble after-after))
                             (assoc! after-after (list new-marble (second (get circle after-after)))))]
          (recur new-circle new-marble (inc new-marble) scores (mod (inc current-player) player-count)))))))

;; (time (play-marbles-prev 9 25))
;; "Elapsed time: 0.6431 msecs"
;; {:max-score 32}

;; (time (play-marbles-prev 491 7105800))
;; "Elapsed time: 47345.844 msecs"
;; {:max-score 2945918550}
