(ns day-05.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def test-inputs (list
  (list "ugknbfddgicrmopn" :nice)
  (list "aaa" :nice)
  (list "jchzalrnumimnmhp" :naughty)
  (list "haegwjzuvuyypxyu" :naughty)
  (list "dvszwmarrgswjxmb" :naughty)))

(def puzzle-input (str/split-lines (slurp "puzzle-input.txt")))

;; Part 1
;; Determine which words are naughty and which are nice

(def vowels #{\a \e \i \o \u})
(def naughty-strings #{"ab" "cd" "pq" "xy"})

(defn nice? [s]
  (and
   (>= (count (keep vowels s)) 3)
   (>= (count (keep (fn [[a b :as cs]] (when (= a b) cs)) (partition 2 1 s))) 1) 
   (= 0 (count (filter (partial str/includes? s) naughty-strings)))))

;; (time (count (filter true? (map nice? puzzle-input))))
;; => 236
;; "Elapsed time: 11.6153 msecs"




;; Part 2
;; New rules
(def test-inputs-2
  (list
   (list "qjhvhtzxzqqjkmpb" :nice)
   (list "xxyxx" :nice)
   (list "uurcxstgmygtbstg" :naughty)
   (list "ieodomkazucvgmuy" :naughty)))


(defn pairs [s]
  (map str/join
       (map first
            (filter (fn [[k v]] (> (count v) 1))
                    (group-by second (map-indexed list (partition 2 1 s)))))))

(defn has-repeated-nonoverlapping-pair? [s]
  (->> s
       (partition 2 1)
       (map-indexed list)
       (group-by second)
       (filter (fn [[_ v]] (> (count v) 1)))
       (map (fn [[a b]] (list (str/join a) (map first b))))
       (map (fn [[p v]] (list p (- (apply max v) (apply min v)))))
       (some (fn [[p dist]] (> dist 1)))
       some?))

(defn has-repeated-letter-separated-by-one? [s]
  (some?
   (some (fn [[a b c]] (= a c))
         (partition 3 1 s))))

;; (time (count (filter true? (map (fn [s] (and (has-repeated-nonoverlapping-pair? s)
;;                                              (has-repeated-letter-separated-by-one? s)))
;;                                 puzzle-input))))
;; => 51
;; "Elapsed time: 55.8845 msecs"
