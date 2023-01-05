(ns advent-10.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 9: Syntax Scoring

;; Part 1
;; Find the total syntax error score for the corrupted lines in the input: those with improper closing delimiters.
(defn parse-input [f]
  (map vec (read-lines f)))

(def openers '(\( \[ \{ \<))

(def closers '(\) \] \} \>))

(def matching (into {} (concat (map vector openers closers) (map vector closers openers))))

(defn process-line [line]
  (reduce (fn [stack c]
            (if (some #{c} openers)
              (conj stack c)
              (if (= (peek stack) (matching c))
                (pop stack)
                (reduced (conj stack c)))))
          '()
          line))

(defn line-status [line]
  (let [processed (process-line line)]
    (cond
      (empty? processed) :valid
      (every? (set openers) processed) (list :incomplete line)
      :else (list :corrupted (first processed)))))

(def points {\) 3 \] 57 \} 1197 \> 25137})

(defn syntax-error-score [f]
  (let [lines (read-lines f)]
    (->> lines
         (map line-status)
         (remove #(= :incomplete (first %)))
         (map second)
         (map points)
         (apply +))))

;; (time (syntax-error-score small-input))
;; "Elapsed time: 0.7964 msecs"
;; 26397

;; (time (syntax-error-score large-input))
;; "Elapsed time: 8.5587 msecs"
;; 215229



;; Part 2
;; Compute the autocomplete score for the completions of the incomplete lines.
(def complete-points {\) 1 \] 2 \} 3 \> 4})

(defn score-completion [cs]
  (reduce (fn [result c]
            (+ (* result 5) (complete-points c)))
          0
          cs))

(defn completion-score [f]
  (->> (read-lines f)
       (map line-status)
       (filter #(= :incomplete (first %)))
       (map second)
       (map process-line)
       (map (fn [os] (map matching os)))
       (map score-completion)
       sort
       ((fn [coll] (nth coll (/ (dec (count coll)) 2))))))

;; (completion-score small-input)
;; 288957

;; (completion-score large-input)
;; 1105996483
