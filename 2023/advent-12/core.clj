(ns advent-12.core)

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

;; Part 1
;; What is the sum of the possible spring arrangements?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #" "))
       (map (fn [[a b]] [a (load-string (str/join ["[" b "]"]))]))))

(defn trim-line [springs sizes]
  (loop [springs springs
         sizes sizes]
    (let [end (re-matches #".*?(\.?\#+\.?)$" springs)]
      (if (seq end)
        (recur (subs springs 0 (- (count springs) (count (second end)))) (butlast sizes))
        (let [start (re-matches #"^(\.?\#+\.?).*" springs)]
          (if (seq start)
            (recur (subs springs (count (second start))) (rest sizes))
            [springs sizes]))))))
