(ns advent-5.core)

(def seat-spec "FBFBBFFRLR")

; Gets the position, choosing the first half when low-symbol is present.
(defn get-pos [seat-spec low high low-symbol]
  (first
    (reduce
      (fn [range spec]
;        (print [range spec]) (newline)
        (let [min (first range)
              max (second range)]
          (if (= low-symbol spec)
            (list
              min
              (- (+ min (/ (+ 1 (- max min)) 2)) 1))
            (list
              (+ min (/ (+ 1 (- max min)) 2))
              max))))
      (list low high)
      seat-spec)))

(defn get-row [seat-spec]
  (get-pos (subs seat-spec 0 7) 0 127 \F))

(defn get-column [seat-spec]
  (get-pos (subs seat-spec 7 10) 0 7 \L))

(defn get-seat [seat-spec]
  (let [row (get-row seat-spec)
        column (get-column seat-spec)
        seat-number (+ (* 8 row) column)]
;    (print ["row " row " column " column " => " seat-number])
;    (newline)
    seat-number))

;BFFFBBFRRR: row 70, column 7, seat ID 567.
;FFFBBBFRRR: row 14, column 7, seat ID 119.
;BBFFBBFRLL: row 102, column 4, seat ID 820.
(defn get-max-seat-id [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (apply max (doall (map get-seat (line-seq rdr))))))

;(get-max-seat-id "/home/runner/advent-5/large-input.txt")

(defn get-all-seat-ids [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (sort (conj (doall (map get-seat (line-seq rdr)))))))

;  (let [x (map #(if (not (= (+ 1 %1) (first %2))) (+ 1 [%1])) s (rest s))]
;    (mapcat #(if (nil? %1) (first %2) %1) x (concat (rest x) [nil]))))

(defn find-missing-value [s]
  (first (filter #(not (nil? %1))
    (map #(if (not (= (+ 1 %1) %2)) (+ 1 %1)) s (rest s)))))

;(find-missing-value (get-all-seat-ids "/home/runner/advent-5/large-input.txt"))
