(ns advent-02.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of the IDs of the games possible with a certain number of cubes?
(defn parse-chunk [s]
  (let [cubes (map str/trim (str/split s #","))]
    (into {"red" 0 "green" 0 "blue" 0}
          (->> cubes
               (map #(str/split % #" "))
               (map (fn [[n c]] (vector c (parse-long n))))))))

(defn parse-game [l]
  (let [id (parse-long (last (re-find #"Game (\d+)" l)))
        chunks (rest (map str/trim (str/split l #"[:;]")))]
    {"id" id
     "chunks" (map parse-chunk chunks)}))

(defn possible-chunk? [max-r max-g max-b {:strs [red green blue] :as chunk}]
  (and (>= max-r red)
       (>= max-g green)
       (>= max-b blue)))

(defn possible-game? [max-r max-g max-b game]
  (every? (partial possible-chunk? max-r max-g max-b) (get game "chunks")))

(defn possible-games [input max-r max-g max-b]
  (->> (read-lines input)
       (map parse-game)
       (filter #(possible-game? max-r max-g max-b %))))

;; (apply + (map #(get % "id") (possible-games small-input 12 13 14)))
;; 8

;; (apply + (map #(get % "id") (possible-games large-input 12 13 14)))
;; 2061



;; Part 2
;; For each game, find the minimum set of cubes that must have been present.
;; What is the sum of the power of these sets?
(defn minimum-cubes [{:strs [id chunks] :as game}]
  (let [min-red (apply max (map #(get % "red") chunks))
        min-green (apply max (map #(get % "green") chunks))
        min-blue (apply max (map #(get % "blue") chunks))]
    [min-red min-green min-blue]))

(defn power-sum [input]
  (->> (read-lines input)
       (map parse-game)
       (map minimum-cubes)
       (map #(apply * %))
       (apply +)))

;; (power-sum small-input)
;; 2286

;; (power-sum large-input)
;; 72596
