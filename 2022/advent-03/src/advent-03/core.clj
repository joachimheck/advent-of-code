(ns advent-03.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the total priority of all the duplicated items in the backpacks?

(def ordered-items "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn item-priority [item]
  (+ 1 (str/index-of ordered-items item)))

(defn split-string [s]
  (let [half (/ (count s) 2)] (list (subs s 0 half) (subs s half))))

(defn includes-or-nil [s c]
  (if (str/includes? s c) c nil))

(defn find-duplicate-item [s1 s2]
  (first (filter not-empty (map #(includes-or-nil s2 (str %)) s1))))

(defn duplicate-item-priority [s]
  (item-priority (let [[s1 s2] (split-string s)] (find-duplicate-item s1 s2))))


;; (reduce + (map duplicate-item-priority (read-lines small-input)))
;; 157
;; (reduce + (map duplicate-item-priority (read-lines large-input)))
;; 7863


;; Part 2
;; What is the total priority of the ID badges of all the elf groups?
(defn matching-items [s1 s2]
  (str/join (distinct (filter not-empty (map #(includes-or-nil s2 (str %)) s1)))))

(defn badge-item [[s1 s2 s3]]
  (matching-items (matching-items s1 s2) s3))


;; (reduce + (map item-priority (map badge-item (partition 3 (read-lines small-input)))))
;; 70
;; (reduce + (map item-priority (map badge-item (partition 3 (read-lines large-input)))))
;; 2488
