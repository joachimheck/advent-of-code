(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; Find the most active monkeys.
(defn read-monkeys [f]
  (mapv vec (filter #(not= % '("")) (partition-by #(= % "") (read-lines f)))))

(defn parse-monkey [input]
  (let [monkey-name (subs (first input) 0 (- (count (first input)) 1))
        starting-items (mapv #(Long/parseLong %) (re-seq #"\d+" (input 1)))
        operation (load-string (let [matches (re-find #"= (.+) (.+) (.+)" (input 2))]
                                 (str/join (list "(fn [old] ("
                                                 (str/join " " (list (matches 2) (matches 1) (matches 3)))
                                                 "))"))))
        divisible-by (Long/parseLong (first (re-seq #"\d+" (input 3))))
        true-throw (Long/parseLong (first (re-seq #"\d+" (input 4))))
        false-throw (Long/parseLong (first (re-seq #"\d+" (input 5))))]
    {:name monkey-name
     :items starting-items
     :operation operation
     :test {:divisor divisible-by true true-throw false false-throw}
     :inspections 0}))

(defn parse-monkeys [f]
  (mapv parse-monkey (read-monkeys f)))

(defn process-monkey [monkeys i]
  (let [monkey (monkeys i)]
    (for [item (:items monkey)]
      (let [worry-level-temp ((:operation monkey) item)
            worry-level (int (/ worry-level-temp 3))
            test-result (int? (/ worry-level (get-in monkey [:test :divisor])))
            throw-to (if test-result (get-in monkey [:test true]) (get-in monkey [:test false]))]
        (list i worry-level throw-to)))))

(defn process-throw [monkeys [from item to :as throw]]
  (let [to-monkey (monkeys to)
        items (:items to-monkey)
        inspections (get-in monkeys [from :inspections])]
    (assoc-in
     (assoc-in
      (assoc-in monkeys [to :items] (conj items item))
      [from :items]
      [])
     [from :inspections]
     (inc inspections))))

(defn process-and-update [monkeys i]
  (let [monkey (monkeys i)
        throws (process-monkey monkeys i)]
    (reduce process-throw monkeys throws)))

(defn process-round [monkeys]
  (reduce process-and-update monkeys (range 0 (count monkeys))))

(defn compute-inspections [f rounds]
  (mapv #(% :inspections) (last (take (inc rounds) (iterate process-round (parse-monkeys f))))))

;; (apply * (take 2 (reverse (sort (compute-inspections small-input 20)))))
;; 10605
;; (apply * (take 2 (reverse (sort (compute-inspections large-input 20)))))
;; 51075


;; Part 2
;; 
(defn parse-monkey-big [[number input]]
  (let [monkey-name (subs (first input) 0 (- (count (first input)) 1))
        starting-items (mapv #(Long/parseLong %) (re-seq #"\d+" (input 1)))
        operation (load-string
                   (let [matches (re-find #"= (.+) (.+) (.+)" (input 2))]
                     (str/join (list "(fn [old] ("
                                       (str/join " " (list (matches 2) (matches 1) (matches 3)))
                                       "))"))))
        divisible-by (Long/parseLong (first (re-seq #"\d+" (input 3))))
        true-throw (Long/parseLong (first (re-seq #"\d+" (input 4))))
        false-throw (Long/parseLong (first (re-seq #"\d+" (input 5))))]
    {:name monkey-name
     :number number
     :items starting-items
     :operation operation
     :test {:divisor divisible-by true true-throw false false-throw}
     :inspections 0}))

(defn mod-items [monkey divisors]
  (assoc monkey :items
         (let [items (monkey :items)]
           (vec
            (for [item items]
              (vec
               (for [divisor divisors]
                 (mod item divisor))))))))

(defn parse-monkeys-big [f]
  (let [monkeys-in (read-monkeys f)
        numbered-monkeys-in (map vector (range 0 (count monkeys-in)) monkeys-in)
        parsed-monkeys (mapv parse-monkey-big numbered-monkeys-in)
        divisors (mapv #(get-in % [:test :divisor]) parsed-monkeys)]
    (mapv #(mod-items % divisors) parsed-monkeys)))

(defn process-monkey-big [monkeys i]
  (let [monkey (monkeys i)
        divisors (mapv #(get-in % [:test :divisor]) monkeys)]
    (for [item (:items monkey)]
      (let [numbered-item-vals (map vector (range 0 (count item)) item)
;            _ (println "item" item)
;            _ (println "numbered-item-vals" numbered-item-vals)
            worry-levels (mapv (fn [[n item-val]] (mod ((:operation monkey) item-val) (divisors n))) numbered-item-vals)
;            _ (println "worry-levels" worry-levels)
            throw-to (if (= (worry-levels (monkey :number)) 0)
                       (get-in monkey [:test true])
                       (get-in monkey [:test false]))]
        (list i worry-levels throw-to)))))

(defn process-throw-big [monkeys [from item to :as throw]]
  (let [to-monkey (monkeys to)
        items (:items to-monkey)
        inspections (get-in monkeys [from :inspections])]
    (assoc-in
     (assoc-in
      (assoc-in monkeys [to :items] (conj items item))
      [from :items]
      [])
     [from :inspections]
     (inc inspections))))

(defn process-and-update-big [monkeys i]
  (let [monkey (monkeys i)
        throws (process-monkey-big monkeys i)]
    (reduce process-throw-big monkeys throws)))

(defn process-round-big [monkeys]
  (reduce process-and-update-big monkeys (range 0 (count monkeys))))

(defn compute-inspections-big [f rounds]
  (mapv #(% :inspections) (last (take (inc rounds) (iterate process-round-big (parse-monkeys-big f))))))

;; (apply * (take 2 (reverse (sort (compute-inspections-big small-input 10000)))))
;; 2713310158
;; (apply * (take 2 (reverse (sort (compute-inspections-big large-input 10000)))))
;; 11741456163
