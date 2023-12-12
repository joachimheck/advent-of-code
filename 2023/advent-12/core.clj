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

(defn springs-to-allocate [springs sizes]
  (let [known (count (filter #{\#} springs))
        unknown (count (filter #{\?} springs))]
    (- (apply + sizes) known)))

(defn spring-permutations [spring-count positions]
  (if (= spring-count 0)
    '(())
    (let [starts (take (inc (- (count positions) spring-count)) positions)]
      (apply concat
       (for [start starts]
         (map #(conj % start)
              (spring-permutations (dec spring-count) (filter #(> % start) positions))))))))

(defn is-valid? [springs sizes]
  (= sizes (map count (str/split (str/replace springs #"^\.+|\.+$" "") #"\.+"))))

(defn populate-springs [springs position-sets pos-map]
  (for [ps position-sets]
    (str/replace (reduce (fn [acc p] (str (subs acc 0 (get pos-map p)) "#" (subs acc (inc (get pos-map p)))))
                         springs
                         ps)
                 #"\?" ".")))

(defn arrangements [springs sizes]
  (let [to-allocate (springs-to-allocate springs sizes)
        pos-map (into {} (map (fn [[n [p _]]] [n p]) (map-indexed list (filter #(= \? (second %)) (map-indexed list springs)))))
        positions (count pos-map)
        position-sets (spring-permutations to-allocate (range 0 positions))
        guesses (populate-springs springs position-sets pos-map)
        arrangements (filter #(is-valid? % sizes) guesses)]
    (count arrangements)))

(defn sum-arrangement-counts [input]
  (let [state (parse-input input)]
    (apply + (map #(apply arrangements %) state))))

;; (time (sum-arrangement-counts small-input))
;; "Elapsed time: 10.177 msecs"
;; 21

;; (time (sum-arrangement-counts large-input))
;; "Elapsed time: 8413.2694 msecs"
;; 7771



;; Part 2
