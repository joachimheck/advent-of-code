(ns advent-04.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; How many points are the cards worth in total?
(defn parse-card [l]
  (let [[_ id winners numbers] (re-matches #"Card +(\d+): ([ \d]+)\|([ \d]+)" l)
        id (parse-long id)
        winners (set (map parse-long (str/split (str/trim winners) #" +")))
        numbers (set (map parse-long (str/split (str/trim numbers) #" +")))]
    {:id id :winners winners :numbers numbers}))

(defn pow [n p]
  (apply * (repeat p n)))

(defn score-card [c]
  (let [matches (count (set/intersection (:winners c) (:numbers c)))]
    (if (zero? matches)
      0
      (pow 2 (dec matches)))))

(defn score-cards [input]
  (->> (read-lines input)
       (map parse-card)
       (map score-card)
       (apply +)))

;; (score-cards small-input)
;; 13

;; (score-cards large-input)
;; 22674



;; Part 2
;; The cards actually win you more cards. How many do you end up with?
(defn parse-cards [input]
  (->> (read-lines input)
       (mapv parse-card)))

(defn find-next-cards [c cards]
  (let [matches (count (set/intersection (:winners c) (:numbers c)))]
    (subvec cards (:id c) (+ (:id c) matches))))

(defn score-cards-2 [input]
  (let [cards (parse-cards input)]
    (loop [c-idx 0
           card-counts (vec (repeat (count cards) 1))]
     (let [c (get cards c-idx)
           match-count (count (set/intersection (:winners c) (:numbers c)))
           idxs (take match-count (iterate inc (inc c-idx)))
           factor (get card-counts c-idx)]
       (if (< c-idx (count cards))
         (recur (inc c-idx)
                (reduce (fn [acc i] (update acc i #(+ factor %)))
                        card-counts
                        idxs))
         (apply + card-counts))))))

;; (time (score-cards-2 small-input))
;; "Elapsed time: 1.074 msecs"
;; 30

;; (time (score-cards-2 large-input))
;; "Elapsed time: 7.6213 msecs"
;; 5747443
