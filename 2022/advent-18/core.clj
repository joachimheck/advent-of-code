(ns advent-18.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (vec (doall(line-seq rdr)))))

;; Part 1
;; What is the surface area of a volume composed of 1x1x1 cubes?
(defn parse-input [f]
  (map (fn [s] (load-string (str/join ["[" s "]"]))) (read-lines f)))

(defn neighbors [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]})

(defn surface-area [cubes]
  (let [cubes (set cubes)]
    (reduce +
            (map (fn [cube] (count (remove cubes (neighbors cube))))
                 cubes))))

;; (surface-area (parse-input small-input))
;; 64
;; (surface-area (parse-input large-input))
;; 4340


;; Part 2
;; What is the exterior surface area?
(defn corners [cubes]
  (list
   [(reduce (fn [result [x y z]] (min result x)) Integer/MAX_VALUE cubes)
    (reduce (fn [result [x y z]] (min result y)) Integer/MAX_VALUE cubes)
    (reduce (fn [result [x y z]] (min result z)) Integer/MAX_VALUE cubes)]
   [(reduce (fn [result [x y z]] (max result x)) Integer/MIN_VALUE cubes)
    (reduce (fn [result [x y z]] (max result y)) Integer/MIN_VALUE cubes)
    (reduce (fn [result [x y z]] (max result z)) Integer/MIN_VALUE cubes)]))

(defn expand-corners [[[x-min y-min z-min] [x-max y-max z-max]]]
  [[(dec x-min) (dec y-min) (dec z-min)] [(inc x-max) (inc y-max) (inc z-max)]])

(defn all-cubes [[x-min y-min z-min] [x-max y-max z-max]]
  (for [i (range x-min (inc x-max))
        j (range y-min (inc y-max))
        k (range z-min (inc z-max))]
    [i j k]))

(defn on-boundary? [[[x-min y-min z-min] [x-max y-max z-max]] [x y z]]
  (or (= x-min x)
      (= x-max x)
      (= y-min y)
      (= y-max y)
      (= z-min z)
      (= z-max z)))

(defn is-contiguous? [points p]
  (some (into #{} points) (neighbors p)))

(defn interior-and-exterior-cubes [cubes]
  (let [
        ;;the-corners (corners cubes)
        the-corners (expand-corners (corners cubes))
        all-cubes (apply all-cubes the-corners)
        empty-cubes (remove (into #{} cubes) all-cubes)
        boundary-cubes (filter (partial on-boundary? the-corners) empty-cubes)]
    ;; (println "all cubes" (count all-cubes) "cubes" (count cubes) "empty" (count empty-cubes) "boundary" (count boundary-cubes))
    (loop [remaining-empty (remove (into #{} boundary-cubes) empty-cubes)
           outside-cubes boundary-cubes]
      ;; (println "remaining-empty" (count remaining-empty) "outside-cubes" (count outside-cubes)
      ;;          "total" (count (apply conj remaining-empty outside-cubes)))
      (let [grouped (group-by (fn [cube] (if (is-contiguous? outside-cubes cube) :outside :unknown)) remaining-empty)
            reduced-empty (:unknown grouped)
            expanded-outside (apply conj outside-cubes (:outside grouped))]
        ;; (println ":outside" (count (:outside grouped)) ":unknown" (count (:unknown grouped))
        ;;          "total" (+ (count (:outside grouped)) (count (:unknown grouped))))
        (if (= remaining-empty reduced-empty)
          (list remaining-empty outside-cubes)
          (recur reduced-empty expanded-outside))))))

(defn extract-volume [cubes]
  ;; (println "extract-volume from" (count cubes) "cubes")
  (loop [volume [(first cubes)]
         remaining (rest cubes)
         outside-volume []]
    (if (empty? remaining)
      ;;(list :in-volume (count volume) :out-of-volume (count outside-volume))
      (list volume outside-volume)
      (if (is-contiguous? volume (first remaining))
        (recur (conj volume (first remaining)) (rest remaining) outside-volume)
        (recur volume (rest remaining) (conj outside-volume (first remaining)))))))

(defn count-exterior-faces [cubes exterior]
  (count (filter (fn [cube] (some #{cube} exterior))
                 (mapcat neighbors cubes)))

  ;; (reduce (fn [exterior-faces cube]
  ;;           (+ exterior-faces (count (some (into #{} (neighbors cube)) exterior))))
  ;;         0
  ;;         cubes)
)

(defn count-interior-faces [cubes interior]
  (count (filter (fn [cube] (some #{cube} interior))
                   (mapcat neighbors cubes)))
  ;; (reduce (fn [exterior-faces cube]
  ;;           (+ exterior-faces (count (some (into #{} (neighbors cube)) exterior))))
  ;;         0
  ;;         cubes)
)

(defn exterior-surface-area [cubes]
  (let [[interior exterior] (interior-and-exterior-cubes cubes)]
    (count-exterior-faces cubes exterior)))

;; 930
;; -> 2468 <-
;; 3360
;; 3393

;; (time (exterior-surface-area (parse-input small-input)))
;; "Elapsed time: 10.378 msecs"
;; 58
;; (time (exterior-surface-area (parse-input large-input)))
;; "Elapsed time: 38617.042 msecs"
;; 2468
