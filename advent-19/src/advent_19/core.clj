(ns advent-19.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall (map parse-line (line-seq rdr)))))

(defn parse-line [l]
  (let [[n & xs] (str/split l #":|\|")]
    (list (Integer/parseInt n)
          (map #(parse-section %) xs))))

(defn parse-section [s]
  (let [cleaned (str/replace (str/trim s) "\"" "")]
    (cond (re-find #"[a-z]" cleaned) cleaned
          :else (map #(Integer/parseInt %) (str/split cleaned #" ")))))

;; (rest (re-find #"( \d+)( \d+)?" " 2 3"))
;; (str/split (str/trim "\"a\"") #" ")
;; (parse-section "\"a\"")
;; (parse-line "0: \"a\"")
;; (parse-line "1: 2 | 3")
;; (parse-line "2: 3 4")
;; (parse-line "3: 4 5 | 6 7")

