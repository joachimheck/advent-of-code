(ns advent-7.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(defrecord BagRule [color rules])

(defn parse-rule [rule]
  (let [re #"(.+) bags contain.+"
        re2 #"(\d) (.+?) bag"]
    (assoc '{}
      (second (re-find re rule))
      (->> (re-seq re2 rule)
        (map rest)
        (map reverse)
        (reduce (fn [coll in] (apply assoc coll in)) '{})))))

;(conj
;  (parse-rule "shiny teal bags contain 3 muted maroon bags, 1 bright salmon bag, 2 dark chartreuse bags.")
;  (parse-rule "dotted black bags contain no other bags."))

(defn collect-rules
  "Returns maps representing the bag nesting rules."
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce merge
      (map parse-rule (line-seq rdr)))))

;(collect-rules "/home/runner/advent-7/small-input.txt")

(defn find-direct-holders
  "Finds bags that can directly hold a bag of the given color."
  [color bag-rules]
  (map first (filter #(contains? (second %) color) bag-rules)))

;(find-direct-holders "shiny gold" (collect-rules "/home/runner/advent-7/small-input.txt"))

;; To traverse the tree of holders, we need a list of matching rules so far,
;; a list of rules to investigate (starting with the requested one),
;; and the list of all rules. From the whole list, keep those that can
;; contain any of the colors on the target list. This is the new target
;; list. Add the old target list to the list of solutions. Remove anything
;; in the list of solutions from the new target list to avoid loops. When
;; the target list is empty, we're done.

(defn find-holders
  "Finds bags that can directly hold a bag of any of the given colors."
  [bag-rules colors]
  (mapcat #(find-direct-holders % bag-rules) colors))

(defn find-holders-recursively
  "Finds bags that can directly or indirectly hold a bag of any of the given colors."
  [bag-rules colors]
  (when (seq colors)
    (let [new-colors (find-holders bag-rules colors)]
      (distinct (flatten (conj
                          (find-holders-recursively bag-rules new-colors)
                          new-colors))))))

; (find-holders-recursively (collect-rules "/home/runner/advent-7/small-input.txt") '("shiny gold"))

(defn count-holders [f color]
  (count (find-holders-recursively (collect-rules f) (list color))))

; (count-holders "/home/runner/advent-7/small-input.txt" "shiny gold")




(defn count-bags [bag-rules color]
  (let [rules (get bag-rules color)]
    (reduce
     (fn [acc rule]
         (+ acc (* (Integer/parseInt (second rule)) (count-bags bag-rules (first rule)))))
     1
     rules)))

(defn count-bags-inside [bag-rules color]
  (- (count-bags bag-rules color) 1))

; (count-bags-inside (collect-rules "/home/runner/advent-7/small-input.txt") "shiny gold")
