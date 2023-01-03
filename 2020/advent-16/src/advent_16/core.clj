(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def small-input-2 "resources/small-input-2.txt")
(def large-input "resources/large-input.txt")

(defrecord Notes [fields my-ticket nearby-tickets])

(defn- parse-line [notes-sec l]
  (let [notes (first notes-sec)
        sec (second notes-sec)]
    (cond (empty? l) notes-sec
          (= "your ticket:" l) (list notes :my-ticket)
          (= "nearby tickets:" l) (list notes :nearby-tickets)
          :else
          (case sec
            :fields (let [matches (rest (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" l))]
                      (list
                       (assoc notes
                              :fields
                              (assoc (:fields notes)
                                     (first matches)
                                     (partition 2 (map #(Long/parseLong %) (rest matches)))))
                       sec))
            :my-ticket (list (assoc notes :my-ticket (vec (map #(Long/parseLong %) (str/split l #",")))) sec)
            :nearby-tickets (list
                             (assoc notes :nearby-tickets
                                    (conj (:nearby-tickets notes) (vec (map #(Long/parseLong %) (str/split l #",")))))
                             sec)))))

(defn- read-notes [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (first (reduce parse-line (list (->Notes {} [] []) :fields) (line-seq rdr)))))

(defn sum-invalid [notes]
  (apply +
         (let [vals (for [vs (vals (:fields notes)) v vs] v)]
           (filter
            (fn [n] (not (reduce (fn [acc pair]
                                   ;; (println acc pair)
                                   (or acc (<= (first pair) n (second pair))))
                                 false
                                 vals)))
            (flatten (:nearby-tickets notes))))))

;; (sum-invalid (read-notes small-input))
;; 71
;; (sum-invalid (read-notes large-input))
;; 25916



;; Part 2
;; Understand the fields on my ticket.
(defn- valid-number? [fields n]
  (let [vals (for [vs (vals fields) v vs] v)]
    (reduce (fn [acc pair] (or acc (<= (first pair) n (second pair))))
            false
            vals)))

(defn- valid-ticket? [fields t]
  (reduce #(and %1 (valid-number? fields %2)) true t))

(defn- possible-fields [fields n]
  (filter seq
          (flatten
           (for [field fields]
             (for [range (second field)]
               (let [name (first field)
                     minval (first range)
                     maxval (second range)]
                 (when (<= minval n maxval) name)))))))

(defn- valid-tickets [notes]
  (filter (partial valid-ticket? (:fields notes)) (conj (:nearby-tickets notes) (:my-ticket notes))))

(defn determine-fields [notes]
  (let [fields (:fields notes)
        field-names (map first fields)]
    (for [i (range 0 (count field-names))]
      (let [possible (map #(possible-fields fields %)
                          (map #(nth % i) (valid-tickets notes)))]
        (filter (fn [name] (every? #(some #{name} %) possible)) field-names)))))

(defn narrow-fields [tried coll]
  (if (= (count tried) (count coll)) (flatten coll)
      (let [singles (flatten (filter #(= (count %) 1) coll))
            to-remove (first (remove #(some #{%} tried) singles))]
        (narrow-fields (conj tried to-remove)
                       (reduce
                        (fn [acc x]
                          (if (= 1 (count x))
                            (map
                             (fn [possible]
                               (if (> (count possible) 1)
                                 (filter #(not= (first x) %) possible)
                                 possible))
                             acc)
                            acc))
                        coll coll)))))

(defn understand-ticket [f]
  (let [notes (read-notes f)]
    (sort #(compare (first %1) (first %2))
     (map vector (narrow-fields '() (determine-fields notes)) (:my-ticket notes)))))

(defn departure-fields-product [f]
  (reduce #(* %1 (second %2)) 1 (filter #(str/starts-with? (first %) "departure ") (understand-ticket f))))

;; (understand-ticket large-input)
;; (["arrival location" 131]
;;  ["arrival platform" 227]
;;  ["arrival station" 223]
;;  ["arrival track" 109]
;;  ["class" 193]
;;  ["departure date" 179]
;;  ["departure location" 103]
;;  ["departure platform" 101]
;;  ["departure station" 61]
;;  ["departure time" 211]
;;  ["departure track" 107]
;;  ["duration" 199]
;;  ["price" 97]
;;  ["route" 181]
;;  ["row" 127]
;;  ["seat" 197]
;;  ["train" 191]
;;  ["type" 53]
;;  ["wagon" 59]
;;  ["zone" 89])

;; (departure-fields-product large-input)
;; 2564529489989
