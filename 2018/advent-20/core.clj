(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 20: A Regular Map

;; Part 1
;; How many doors away is the furthest room in the area?
;; (defn parse-line [l]
;;   (let [l (-> l
;;               (str/replace #"\^|\$" "")
;;               (str/replace #"\|" " ")
;;               ;; (str/replace #"N" "\\\\N")
;;               ;; (str/replace #"S" "\\\\S")
;;               ;; (str/replace #"E" "\\\\E")
;;               ;; (str/replace #"W" "\\\\W")
;;               (str/replace #"([A-Z]+)" "\"$1\"")
;; )]
;;     (load-string (str/join (list "'(" l ")")))))

(defn parse-line
  ([l] (:expressions (parse-line l 0)))
  ([l start-char]
   (loop [i start-char sub-expressions [] current-expression []]
     ;; (println "i" i "sub-expressions" sub-expressions "current-expression" current-expression)
     (let [c (if (< i (count l)) (nth l i))]
       (if (or (= i (count l)) (= c \)))
         {:expressions (if (empty? current-expression)
                         sub-expressions
                         (conj sub-expressions current-expression))
          :index i}
         (cond (= c \|)
               (recur (inc i) (conj sub-expressions current-expression) [])
               (= c \()
               (let [{:keys [expressions index]} (parse-line l (inc i))]
                 (recur (inc index) sub-expressions (conj current-expression expressions)))
               :else
               (recur (inc i) sub-expressions (conj current-expression c))))))))

(defn parse-input [f]
  (->> f
       (read-lines)
       (map #(str/replace % #"\^|\$" ""))
       (first)
       (parse-line)))

(defn move [[x y] d]
  (case d
    \N [x (dec y)]
    \S [x (inc y)]
    \E [(inc x) y]
    \W [(dec x) y]))

(defn follow-path
  ([path] (follow-path path [0 0] #{}))
  ([path [x y :as pos] doors]
   (if (empty? path)
     doors
     (let [c (first path)]
       (if (seqable? c)
         (let [new-doors (apply conj  doors (follow-path c pos doors))]
           (recur (rest path) pos new-doors))
         (let [new-pos (move pos c)
               new-doors (conj doors #{pos new-pos})]
           (recur (rest path) new-pos new-doors)))))))

;; Do the branches ever merge back together? Apparently not.
(defn non-seqable-following-seqable? [v]
  (let [partitioned (partition 2 1 v)]
    (if (some true? (map (fn [[a b]] (and (seqable? a) (not (seqable? b)))) partitioned))
      true
      (map #(non-seqable-following-seqable? %) (filter seqable? v)))))

(defn get-bounds [rooms]
  (reduce (fn [[[minx miny] [maxx maxy]] [x y]]
            [[(min minx x)
              (min miny y)]
             [(max maxx x)
              (max maxy y)]])
          [[Integer/MAX_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MIN_VALUE]]
          (apply set/union rooms)))

(defn draw-map [doors]
  (let [[[minx miny] [maxx maxy]] (get-bounds doors)]
    (str/join
     "\n"
     (concat
      (list (str/join (repeat (+ 1 (* 2 (- (inc maxx) minx))) "#")))
      (for [j (range miny (inc maxy))]
        (str/join
         "\n"
         (list (str/join (concat '("#")
                                 (for [i (range minx (inc maxx))]
                                   (format ".%s" (if (get doors #{[i j] [(inc i) j]}) "|" "#")))))
               (str/join (concat '("#")
                                 (for [i (range minx (inc maxx))]
                                   (format "%s#" (if (get doors #{[i j] [i (inc j)]}) "-" "#"))))))))))))

(def small-input-2 "small-input-2.txt")

(defn adjacent [[x y] doors]
  (map (fn [s] (first (remove #{[x y]} s))) (filter #(some #{[x y]} %) doors)))

(defn flood-map [path]
  (let [doors (follow-path path)
        rooms (apply set/union doors)]
    (loop [open-set #{[0 0]} visited {} distance 0]
      (if (empty? open-set)
          visited
          (let [new-open-set (set (remove visited (apply concat (map #(adjacent % doors) open-set))))
                new-visited (into visited (map (fn [[x y]] [[x y] distance]) open-set))
                new-distance (inc distance)]
            ;; (println "adjacent" (apply concat (map #(adjacent % doors) open-set)))
            (recur new-open-set new-visited new-distance))))))

(defn distance-to-furthest-room [path]
  (apply max (vals (flood-map path))))

;; (time (distance-to-furthest-room (parse-input small-input)))
;; "Elapsed time: 1.3158 msecs"
;; 10

;; (time (distance-to-furthest-room (parse-input large-input)))
;; "Elapsed time: 107502.0492 msecs"
;; 3014



;; Part 2
;; How many rooms have a shortest path from your current location that pass through at least 1000 doors?
(defn rooms-at-least-1000-doors-away [path]
  (count (filter #(>= % 1000) (vals (flood-map path)))))

;; (time (rooms-at-least-1000-doors-away (parse-input large-input)))
;; "Elapsed time: 110650.2347 msecs"
;; 8279
