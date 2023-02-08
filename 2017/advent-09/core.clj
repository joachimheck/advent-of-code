(ns advent-09.core)

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

;; Day 9: Stream Processing

;; Part 1
;; Score the groups in the character stream, ignoring garbage.
(defn parse-input [f]
  (first (read-lines f)))

(defn cancel-characters [s]
  (loop [s s i 0]
    (if (= i (count s))
      s
      (if (= \! (get s i))
        (recur (apply conj (subvec s 0 i) (subvec s (+ i 2))) i)
;;        (recur (assoc s (inc i) \space) (+ 2 i))
        (recur s (inc i))))))

(defn index-of [v x]
  (loop [v v x x i 0]
    (if (= i (count v))
      nil
      (if (= x (get v i))
        i
        (recur v x (inc i))))))

(defn find-garbage [s]
  (let [start (index-of s \<)
        end (index-of s \>)]
    (if start
      [start end]
      nil)))

(defn remove-garbage [s]
  (let [s-vec (cancel-characters (vec s))
        [start end :as garbage] (find-garbage s-vec)]
    (if garbage
      (remove-garbage (str/join (apply conj (subvec s-vec 0 start) (subvec s-vec (inc end)))))
      s)))

(deftest test-remove-garbage
  (is (= "" (remove-garbage "<>")))
  (is (= "" (remove-garbage "<random characters>")))
  (is (= "" (remove-garbage "<<<<>")))
  (is (= "" (remove-garbage "<{!>}>")))
  (is (= "" (remove-garbage "<!!>")))
  (is (= "" (remove-garbage "<!!!>>")))
  (is (= "" (remove-garbage "<{o\"i!a,<{i<a>"))))

(defn convert-to-tree [s]
  (let [parens (-> s
                   remove-garbage
                   (str/replace #"\{" "(")
                   (str/replace #"\}" ")"))]
    (load-string (str/join (list "'" parens)))))

(defn count-groups [s]
  (count (tree-seq (constantly true) identity (convert-to-tree s))))

(deftest test-count-groups
  (is (= 1 (count-groups "{}")))
  (is (= 3 (count-groups "{{{}}}")))
  (is (= 3 (count-groups "{{},{}}")))
  (is (= 6 (count-groups "{{{},{},{{}}}}")))
  (is (= 1 (count-groups "{<{},{},{{}}>}")))
  (is (= 1 (count-groups "{<a>,<a>,<a>,<a>}")))
  (is (= 5 (count-groups "{{<a>},{<a>},{<a>},{<a>}}")))
  (is (= 2 (count-groups "{{<!>},{<!>},{<!>},{<a>}}"))))

(defn score [s]
  (:score
   (reduce (fn [{score :score depth :depth :as acc} c]
             (if (= c \{)
               {:score (+ score depth) :depth (inc depth)}
               {:score score :depth (dec depth)}))
           {:score 0 :depth 1}
           (str/replace (remove-garbage s) #"," ""))))

(deftest test-score
 (is (= 1 (score "{}")))
 (is (= 6 (score "{{{}}}")))
 (is (= 5 (score "{{},{}}")))
 (is (= 16 (score "{{{},{},{{}}}}")))
 (is (= 1 (score "{<a>,<a>,<a>,<a>}")))
 (is (= 9 (score "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
 (is (= 9 (score "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
 (is (= 3 (score "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))

;; (time (score (parse-input large-input)))
;; "Elapsed time: 7701.6141 msecs"
;; 14190



;; Part 2
;; How many non-canceled characters are within the garbage in the stream?


;; ---> answer <---
;; 8629
;; 16905


(defn extract-garbage [s]
  (let [s-vec (cancel-characters (vec s))
        [start end :as garbage] (find-garbage s-vec)]
    (if garbage
      (apply conj (subvec s-vec (inc start) end) (extract-garbage (str/join (subvec s-vec (inc end)))))
      '())))

(defn count-garbage [s]
  (count (extract-garbage s)))

(deftest test-count-garbage
  (is (= 0 (count-garbage "<>")))
  (is (= 17 (count-garbage "<random characters>")))
  (is (= 3 (count-garbage "<<<<>")))
  (is (= 2 (count-garbage "<{!>}>")))
  (is (= 0 (count-garbage "<!!>")))
  (is (= 0 (count-garbage "<!!!>>")))
  (is (= 10 (count-garbage "<{o\"i!a,<{i<a>"))))

;; (time (count-garbage (parse-input large-input)))
;; "Elapsed time: 26469.7291 msecs"
;; 7053
