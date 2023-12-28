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
        guesses (populate-springs springs position-sets pos-map)]
    (filter #(is-valid? % sizes) guesses)))

(defn count-arrangements [springs sizes]
  (count (arrangements springs sizes)))

(defn sum-arrangement-counts [input]
  (let [state (parse-input input)]
    (apply + (map #(apply count-arrangements %) state))))

;; (time (sum-arrangement-counts small-input))
;; "Elapsed time: 10.177 msecs"
;; 21

;; (time (sum-arrangement-counts large-input))
;; "Elapsed time: 8413.2694 msecs"
;; 7771

;; That's way too slow, and even runs out of memory, for part 2. Here's my implementation of a solution from:
;; https://www.reddit.com/r/adventofcode/comments/18h4ign/2023_day_12_part_2_how_to_approach_part_2/
(defn possible-arrangements
  ([springs sizes] (possible-arrangements springs sizes 0))
  ([springs sizes run-length]
   (if (empty? springs)
     (if (or (and (zero? run-length) (empty? sizes)) (and (= run-length (first sizes)) (= 1 (count sizes)))) 1 0)
     (case (first springs)
       \. (if (= run-length (first sizes))
            (possible-arrangements (subs springs 1) (rest sizes) 0)
            (if (zero? run-length)
              (possible-arrangements (subs springs 1) sizes 0)
              0))
       \# (if (< run-length (or (first sizes) -1))
            (possible-arrangements (subs springs 1) sizes (inc run-length))
            0)

       \? (+ (if (= run-length (first sizes))
               (possible-arrangements (subs springs 1) (rest sizes) 0)
               (if (zero? run-length)
                 (possible-arrangements (subs springs 1) sizes 0)
                 0))
             (if (< run-length (or (first sizes) -1))
               (possible-arrangements (subs springs 1) sizes (inc run-length))
               0))))))

(deftest test-possible-arrangements
  (is (= 0 (possible-arrangements "" [] 1)))
  (is (= 1 (possible-arrangements "#" [1])))
  (is (= 1 (possible-arrangements "##" [2])))
  (is (= 1 (possible-arrangements ".#" [1])))
  (is (= 1 (possible-arrangements "#." [1])))
  (is (= 1 (possible-arrangements "#.#" [1 1])))
  (is (= 1 (possible-arrangements "##.#" [2 1])))
  (is (= 0 (possible-arrangements "." [1])))
  (is (= 1 (possible-arrangements "?" [1])))
  (is (= 2 (possible-arrangements "??" [1])))
  (is (= 4 (possible-arrangements "??.??" [1 1])))
  (is (= 4 (possible-arrangements "????" [1])))
  (is (= 1 (possible-arrangements "?.##" [2])))
  (is (= 1 (possible-arrangements "???.###" [1 1 3])))
  (is (= 4 (possible-arrangements ".??..??...?##." [1 1 3])))
  (is (= 1 (possible-arrangements "?#?#?#?#?#?#?#?" [1 3 1 6])))
  (is (= 1 (possible-arrangements "????.#...#..." [4 1 1])))
  (is (= 4 (possible-arrangements "????.######..#####." [1 6 5])))
  (is (= 10 (possible-arrangements "?###????????" [3 2 1]))))

(defn possible-arrangement-counts [input]
  (let [state (parse-input input)]
    (map #(apply possible-arrangements %) state)))

(defn sum-possible-arrangements [input]
  (apply + (possible-arrangement-counts input)))

;; (time (sum-possible-arrangements small-input))
;; "Elapsed time: 4.1344 msecs"
;; 21

;; (time (sum-possible-arrangements large-input))
;; "Elapsed time: 500.1841 msecs"
;; 7771



;; Part 2
;; Same thing but much bigger input.
(defn parse-input-2 [input]
  (->> (read-lines input)
       (map #(str/split % #" "))
       (map (fn [[a b]] [(str/join "?" (repeat 5 a))
                         (load-string (str/join ["[" (str/join "," (repeat 5 b)) "]"]))]))))

;; The above implementation is still too slow by itself - gotta memoize.
(def arrangements (atom {}))

(defn possible-arrangements-memo
  ([springs sizes] (possible-arrangements-memo springs sizes 0))
  ([springs sizes run-length]
   (if-let [cached (get @arrangements [springs sizes run-length])]
     cached
     (let [new-val (if (empty? springs)
                     (if (or (and (zero? run-length) (empty? sizes)) (and (= run-length (first sizes)) (= 1 (count sizes)))) 1 0)
                     (case (first springs)
                       \. (if (= run-length (first sizes))
                            (possible-arrangements-memo (subs springs 1) (rest sizes) 0)
                            (if (zero? run-length)
                              (possible-arrangements-memo (subs springs 1) sizes 0)
                              0))
                       \# (if (< run-length (or (first sizes) -1))
                            (possible-arrangements-memo (subs springs 1) sizes (inc run-length))
                            0)

                       \? (+ (if (= run-length (first sizes))
                               (possible-arrangements-memo (subs springs 1) (rest sizes) 0)
                               (if (zero? run-length)
                                 (possible-arrangements-memo (subs springs 1) sizes 0)
                                 0))
                             (if (< run-length (or (first sizes) -1))
                               (possible-arrangements-memo (subs springs 1) sizes (inc run-length))
                               0))))]
       (swap! arrangements assoc [springs sizes run-length] new-val)
       new-val))))

(defn possible-arrangement-counts-2 [input]
  (let [state (parse-input-2 input)]
    (map #(apply possible-arrangements-memo %) state)))

(defn sum-possible-arrangements-2 [input]
  (apply + (possible-arrangement-counts-2 input)))

;; (time (sum-possible-arrangements-2 small-input))
;; "Elapsed time: 3.3463 msecs"
;; 525152

;; (time (sum-possible-arrangements-2 large-input))
;; "Elapsed time: 2327.8278 msecs"
;; 10861030975833
