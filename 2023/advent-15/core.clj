(ns advent-15.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of the hashes of the input elements?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(str/split % #","))
       (first)))

(defn apply-hash [s]
  (loop [v 0
         s s]
    (if (empty? s)
      v
      (recur (rem (* (+ v (int (first s))) 17) 256) (rest s)))))

(defn sum-hash [coll]
  (apply + (map apply-hash coll)))


;; (sum-hash (parse-input small-input))
;; 1320

;; (sum-hash (parse-input large-input))
;; 504449



;; Part 2
;; What is the focusing power of the final lens configuration?
(defn parse-operation [s]
  (let [[_ label op fl] (re-matches #"([a-z]+)([-=])(\d*)" s)]
    {:label label
     :op op
     :fl (parse-long fl)
     :box-id (apply-hash label)}))

(defn execute-operation [s boxes]
  (let [{:keys [label op fl box-id]} (parse-operation s)]
    (update boxes box-id
            (fn [box]
              (if (= op "-")
                (vec (remove #(= label (first %)) box))
                (let [[index _] (first (filter (fn [[i [l _]]] (= l label)) (map-indexed list box)))]
                  (if index
                    (assoc box index [label fl])
                    (conj box [label fl]))))))))

(defn print-boxes [boxes]
  (doseq [[i lenses] (filter (fn [[i l]] (seq l)) (map-indexed list boxes))]
    (printf "Box %d:" i)
    (doseq [[label focal-length] lenses]
      (printf " [%s %d]" label focal-length))
    (newline)))

(defn arrange-lenses [input]
  (loop [ops (parse-input input)
         boxes (vec (repeat 256 []))]
    (if (empty? ops)
      boxes
      (recur (rest ops)
             (execute-operation (first ops) boxes)))))

(defn focusing-power [box-number slot-number focal-length]
  (* (inc box-number) (inc slot-number) focal-length))

(defn box-focusing-power [box-number box]
  (let [lenses (map-indexed list box)]
    (apply +
           (map (fn [[slot-number [label focal-length]]]
                  (focusing-power box-number slot-number focal-length))
                lenses))))

(defn compute-total-focusing-power [input]
  (let [boxes (arrange-lenses input)]
    (apply + (map (fn [[i b]] (box-focusing-power i b)) (map-indexed list boxes)))))

;; (compute-total-focusing-power small-input)
;; 145

;; (compute-total-focusing-power large-input)
;; 262044
