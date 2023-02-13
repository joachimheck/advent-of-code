(ns advent-16.core)

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

;; Day 16: Permutation Promenade

;; Part 1
;; How are the programs ordered after dancing?
(defn parse-move [move]
  (case (first (vec move))
    \s (let [size (parse-long (first (rest (re-find #"(\d+)" move))))]
         {:op :spin :x size})
    \x (let [[a b] (map parse-long (rest (re-find #"(\d+)/(\d+)" move)))]
         {:op :exchange :a a :b b})
    \p (let [[a b] (map first (rest (re-find #"([a-z])/([a-z])" move)))]
         {:op :partner :a a :b b})))

(defn parse-input [f]
  (let [input (first (read-lines f))
        split (str/split input #",")]
    (map parse-move split)))

(def start-state (vec "abcdefghijklmnop"))

(defn spin [programs x]
  (let [split-point (- (count programs) x)]
    (vec (concat (subvec programs split-point) (subvec programs 0 split-point)))))

(defn exchange [programs a b]
  (-> programs
      (assoc a (get programs b))
      (assoc b (get programs a))))

(defn partner [programs a b]
  (let [a-idx (.indexOf programs a)
        b-idx (.indexOf programs b)]
    (exchange programs a-idx b-idx)))

(defn move [programs move]
  (case (:op move)
    :spin (spin programs (:x move))
    :exchange (exchange programs (:a move) (:b move))
    :partner (partner programs (:a move) (:b move))))

(defn dance [programs moves]
  (loop [programs programs moves moves]
    (if (empty? moves)
      (str/join programs)
      (recur (move programs (first moves)) (rest moves)))))

;; (time (dance (vec "abcde") (parse-input small-input)))
;; "Elapsed time: 0.6444 msecs"
;; "baedc"

;; (time (dance start-state (parse-input large-input)))
;; "Elapsed time: 117.4336 msecs"
;; "gkmndaholjbfcepi"



;; Part 2
;; Repeat the dance a billion times.
(defn dance-until-loop [programs moves]
  (loop [programs programs i 0 seen-states {}]
    (cond
      (some (set (keys seen-states)) [programs]) {:loop-iteration i :previous-iteration (get seen-states programs)}
      :else (recur (dance (vec programs) moves) (inc i) (assoc seen-states programs i)))))

(defn dance-multiple [programs moves iterations]
  (let [{:keys [loop-iteration previous-iteration]} (dance-until-loop programs moves)
        reduced-iterations (- iterations previous-iteration)
        loop-size (- loop-iteration previous-iteration)
        solution-iterations (+ loop-iteration (mod reduced-iterations loop-size))]
    (loop [programs programs i 0]
      (if (= i solution-iterations)
        (str/join programs)
        (recur (dance (vec programs) moves) (inc i))))))

;; (time (dance-multiple start-state (parse-input large-input) 1000000000))
;; "Elapsed time: 7372.8519 msecs"
;; "abihnfkojcmegldp"
