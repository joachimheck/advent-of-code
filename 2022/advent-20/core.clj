(ns advent-20.core)

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
;; Mix a file (sequence of numbers) to decrypt it.
(defn parse-input [f]
  (vec (keep-indexed list (map #(Integer/parseInt %) (read-lines f)))))

(defn print-file [file]
  (println (map second file)))

(defn move-item [file original-file index]
  (let [item (nth original-file index)
        motion (second item)
        file-size (count file)
        removed (vec (remove #{item} file))
        current-index (.indexOf file item)
        new-index (mod (+ current-index motion) (dec file-size))
        new-index (if (and (= new-index 0) (< motion 0)) (dec file-size) new-index)]
    ;; (println "inserting" (second item) "at position" new-index)
    (apply conj (subvec removed 0 new-index)
           item
           (subvec removed new-index))))

(defn mix-file [file]
  (reduce (fn [result i] (move-item result file i))
               file
               (range (count file))))

(defn get-coordinates [f]
  (let [mixed (map second (mix-file (parse-input f)))
        zero-index (.indexOf mixed 0)
        indices (map #(mod (+ zero-index %) (count mixed)) (list 1000 2000 3000))]
    (map #(get (vec mixed) %) indices)))

;; (time (apply + (get-coordinates small-input)))
;; "Elapsed time: 0.83 msecs"
;; 3
;; (time (apply + (get-coordinates large-input)))
;; "Elapsed time: 14891.109 msecs"
;; 872


;; Part 2
;; Multiply the numbers by the decryption key, 811589153, and mix ten times.
(defn mix-file-2 [file original-file]
  (reduce (fn [result i] (move-item result original-file i))
               file
               (range (count file))))

(defn get-coordinates-2 [f]
  (let [file (parse-input f)
        decryption-key 811589153
        multiplied (map #(list (first %) (* decryption-key (second %))) file)
        multimixed (loop [i 0 mixed multiplied]
                     (if (= i 10)
                       (map second mixed)
                       (recur (inc i) (mix-file-2 mixed multiplied))))
        zero-index (.indexOf multimixed 0)
        indices (map #(mod (+ zero-index %) (count multimixed)) (list 1000 2000 3000))]
    (println indices)
    (map #(get (vec multimixed) %) indices)))

;; (time (apply + (get-coordinates-2 small-input)))
;; (4 3 2)
;; "Elapsed time: 10.8961 msecs"
;; 1623178306
;; (time (apply + (get-coordinates-2 large-input)))
;; (2201 3201 4201)
;; "Elapsed time: 176214.456 msecs"
;; 5382459262696
