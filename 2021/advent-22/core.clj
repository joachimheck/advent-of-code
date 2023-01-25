(ns advent-22.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 22: Reactor Reboot

;; Part 1
;; How many reactor cubes are on after executing the initialization instructions?
(defn parse-lines [lines]
  (->> lines
       (map #(re-find #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" %))
       (map rest)
       (map #(let [[op & vals] %]
               {:operation op
                :cuboid (mapv parse-long vals)}))))

(defn parse-input [f]
  (parse-lines (read-lines f)))

(def mini-input
  (parse-lines
   '("on x=10..12,y=10..12,z=10..12"
     "on x=11..13,y=11..13,z=11..13"
     "off x=9..11,y=9..11,z=9..11"
     "on x=10..10,y=10..10,z=10..10")))

(defn get-cubes [[min-x max-x min-y max-y min-z max-z :as cuboid]]
  (apply set/union
         (for [x (range min-x (inc max-x))]
           (apply set/union
                  (for [y (range min-y (inc max-y))]
                    (into #{}
                          (for [z (range min-z (inc max-z))]
                            [x y z])))))))

(defn in-bounds [cuboid]
  (every? #(<= -50 % 50) cuboid))

(defn initialize-reactor [instructions]
  (reduce (fn [cubes {operation :operation cuboid :cuboid :as instruction}]
            ;;(println "Reducing instruction" instruction)
            (if (in-bounds cuboid)
              (let [new-cubes (get-cubes cuboid)]
                (if (= operation "on")
                  (set/union cubes new-cubes)
                  (set/difference cubes new-cubes)))
              cubes))
          #{}
          instructions))

;; (time (count (initialize-reactor (parse-input small-input))))
;; "Elapsed time: 4518.0558 msecs"
;; 590784

;; (time (count (initialize-reactor (parse-input large-input))))
;; "Elapsed time: 4772.5268 msecs"
;; 589411



;; Part 2
;; Run the full reboot procedure. How many cubes are on?

(def small-input-2 "small-input-2.txt")

(defn overlap [[ax1 ax2 ay1 ay2 az1 az2] [bx1 bx2 by1 by2 bz1 bz2]]
  (if (and (or (<= bx1 ax1 bx2) (<= ax1 bx1 ax2))
           (or (<= by1 ay1 by2) (<= ay1 by1 ay2))
           (or (<= bz1 az1 bz2) (<= az1 bz1 az2)))
    ;; ax1 bx1 ax2 bx2 => bx1 ax2
    ;; bx1 ax1 bx2 ax2 => ax1 bx2
    ;; ax1 bx1 bx2 ax2 => bx1 bx2
    ;; bx1 ax1 ax2 bx2 => ax1 ax2
    [(max ax1 bx1) (min ax2 bx2)
     (max ay1 by1) (min ay2 by2)
     (max az1 bz1) (min az2 bz2)]))

(defn overlaps? [a b]
  (boolean (overlap a b)))

(defn count-cubes [[x1 x2 y1 y2 z1 z2 :as cuboid]]
  (* (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1)))

(defn subtract-cube [[ax1 ax2 ay1 ay2 az1 az2 :as a] [bx1 bx2 by1 by2 bz1 bz2 :as b]]
  ;; ax1 bx1 ax2 bx2 => overlap: [bx1 ax2] other: ([ax1 (dec bx1)])
  ;; bx1 ax1 bx2 ax2 => overlap: [ax1 bx2] other: ([(inc bx2) ax2])
  ;; ax1 bx1 bx2 ax2 => overlap: [bx1 bx2] other: ([ax1 (dec bx1)] [(inc bx2) ax2])
  ;; bx1 ax1 ax2 bx2 => overlap: [ax1 ax2] other: ()
  (let [result (if (overlap a b)
                 (let [[ox1 ox2 :as overlap-x] [(max ax1 bx1) (min ax2 bx2)]
                       other-x (remove (fn [[min max]] (or (< min ax1) (> max ax2) (> min max)))
                                       (list [ax1 (dec bx1)] [(inc bx2) ax2]))
                       remainder-x (map #(apply conj % [ay1 ay2 az1 az2]) other-x)]
                   (let [[oy1 oy2 :as overlap-y] [(max ay1 by1) (min ay2 by2)]
                         other-y (remove (fn [[min max]] (or (< min ay1) (> max ay2) (> min max)))
                                         (list [ay1 (dec by1)] [(inc by2) ay2]))
                         remainder-y (map #(apply conj (apply conj [ox1 ox2] %) [az1 az2]) other-y)]
                     (let [[oz1 oz2 :as overlap-z] [(max az1 bz1) (min az2 bz2)]
                           other-z (remove (fn [[min max]] (or (< min az1) (> max az2) (> min max)))
                                           (list [az1 (dec bz1)] [(inc bz2) az2]))
                           remainder-z (map #(apply conj [ox1 ox2 oy1 oy2] %) other-z)]
                       ;; (println "overlap-x" overlap-x)
                       ;; (println "remainders" remainder-x remainder-y remainder-z)
                       (concat remainder-x remainder-y remainder-z))))
                 (list a))]
    (if (some (fn [[x1 x2 y1 y2 z1 z2 :as cuboid]] (or (nil? cuboid) (> x1 x2) (> y1 y2) (> z1 z2))) result)
      (println "subtract-cube" a b "=" result))
    result))

(defn add-cube [[ax1 ax2 ay1 ay2 az1 az2 :as a] [bx1 bx2 by1 by2 bz1 bz2 :as b]]
  ;; (println "a" a "b" b)
  ;; (println "a-b" (subtract-cube a b))
  ;; (println "b-a" (subtract-cube b a))
  ;; (println "overlap" (list (overlap a b)))
  (let [result (remove nil? (concat (subtract-cube a b) (subtract-cube b a) (list (overlap a b))))]
    (if (some (fn [[x1 x2 y1 y2 z1 z2 :as cuboid]] (or (nil? cuboid) (> x1 x2) (> y1 y2) (> z1 z2))) result)
      (println "add-cube" a b "=" result))
    result))

(defn reboot-reactor [instructions]
  (let [cuboids (reduce (fn [cuboids {operation :operation op-cuboid :cuboid :as instruction}]
                          ;;(println "reduce-fn cuboids" cuboids)
                          (if (= operation "on")
                            (distinct (apply concat (map #(add-cube op-cuboid %) cuboids)))
                            (distinct (apply concat (map #(subtract-cube % op-cuboid) cuboids)))))
                        (list (:cuboid (first instructions)))
                        (rest instructions))]
    (println "cuboids" (sort-by second (map #(list % (count-cubes %)) cuboids)))
    (apply + (map count-cubes cuboids))))


;; Right answer for the mini-input:
;; (time (reboot-reactor mini-input))
;; "Elapsed time: 1.2437 msecs"
;; 39

;; Wrong answer for the sample input.
;; (time (reboot-reactor (parse-input small-input-2)))
;; "Elapsed time: 1595.4692 msecs"
;; 50152242282941844

;; [0 1 0 1 0 1] => [0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]
;; [1 2 1 2 1 2] => [1 1 1] [1 1 2] [1 2 1] [1 2 2] [2 1 1] [2 1 2] [2 2 1] [2 2 2]
(defn reboot-debug [instructions]
  (let [cuboids (reduce (fn [cuboids {operation :operation op-cuboid :cuboid :as instruction}]
                          (println "reduce-fn cuboids" cuboids op-cuboid)
                          (if (= operation "on")
                            ;; non-overlapping cuboids + non-overlapping parts of overlapping cuboids
                            ;; + non-overlapping parts of op-cuboid + overlaps
                            (let [{non-overlapping false overlapping true :as grouped} (group-by #(overlaps? % op-cuboid) cuboids)]
                              (distinct (concat non-overlapping
                                                (apply concat (map #(subtract-cube % op-cuboid) overlapping))
                                                (apply concat (map #(subtract-cube op-cuboid %) overlapping))
                                                (map #(overlap % op-cuboid) overlapping))))

                            ;;(distinct (apply concat (map #(add-cube op-cuboid %) cuboids)))

                            (distinct (apply concat (map #(subtract-cube % op-cuboid) cuboids)))))
                        (list (:cuboid (first instructions)))
                        (rest instructions))]
    cuboids
    ;; (reduce #(into %1 %2) (map get-cubes cuboids))
    ))

;; [-22 26 -29 23 -38 16] [-22 -21 -29 23 -38 16]

(defn get-pairs [xs]
  (if (= (count xs) 2)
    (list (set xs))
    (concat (map (fn [x] #{(first xs) x}) (rest xs))
            (get-pairs (rest xs)))))

(defn any-overlap? [cuboids]
  (remove nil? (map (fn [s] (let [[a b] (seq s)] (if (overlap a b) (list a b)))) (get-pairs cuboids))))
