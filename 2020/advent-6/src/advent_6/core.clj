(ns advent-6.core)

(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(defn collect-answer
  "Adds to the current answer or, if the string is empty, starts a new one."
  [coll x]
  (if (= x "")
    (conj coll [])
    (conj (vec (butlast coll)) (conj (last coll) x))))

(defn collect-answers
  "Returns a list of strings representing the answers of a group."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce collect-answer [[]] (line-seq rdr))))

(defn answered-by-anyone [answers]
  (let [min-string (str/join (distinct (str/join answers)))
        cnt (count min-string)]
    (println min-string "=>" cnt)
    cnt))

(defn count-distinct-answers [answers]
  (apply + (map answered-by-anyone answers)))

(defn answered-by-everyone [answers]
  (let [min-string (str/join (distinct (str/join answers)))
        result (reduce
          (fn [acc answer]
;            (println "counting" answer (type answer) "in" answers "aka" min-string "/" acc)
            (if (= (count answers) (count (filter #(str/includes? %1 (str answer)) answers)))
              (str/join (list acc (str answer)))
              acc))
          ""
          min-string)]
;    (println result "=>" (count result))
    (count result)))

(defn count-common-answers [answers]
  (apply + (map answered-by-everyone answers)))

;(count-distinct-answers (collect-answers "/home/runner/advent-6/small-input.txt"))
;(count-distinct-answers (collect-answers "/home/runner/advent-6/large-input.txt"))
;(count-common-answers (collect-answers "/home/runner/advent-6/small-input.txt"))
;(count-common-answers (collect-answers "/home/runner/advent-6/large-input.txt"))


;; Alternative method - sum up all the answers for a group, then filter out
;; the ones not found for each member of the group in turn. Seems more elegant.
(defn count-common-answers-2
  "Returns the total number of questions answered by every member of each group"
  [answers]
  (reduce
   (fn [acc strings]
     (+
      acc
      (count (reduce #(filter (set %1) %2)
                     (str/join (distinct (str/join strings)))
                     strings))))
   0
   answers))



;; Try to use thread-last. This is much more elegant, but it's too bad that
;; 1) distinct doesn't return a string so I need to convert and
;; 2) I need both the original strings and the unique characters, so I can't
;; push everything through the same transformation chain in count-common-answers-3.
(defn unique-chars [s] ((comp str/join distinct str/join) s))

(defn find-common-answers [coll]
  (reduce (fn [s1 s2] (filter (set s1) s2)) (unique-chars coll) coll))

(defn count-common-answers-3 [answers]
  (->> answers
       (map find-common-answers)
       (map count)
       (reduce +)))



;; I used zipping to get past issue 2) above.
(defn count-common-answers-4 [answers]
  (->> answers
       (map unique-chars)
       (map vector answers) ; Zip unique chars with original answer strings.
       (map reverse) ; Reverse so the order is right for apply on next line.
       (map #(apply reduce (fn [s1 s2] (filter (set s1) s2)) %))
       (map count)
       (reduce +)))
