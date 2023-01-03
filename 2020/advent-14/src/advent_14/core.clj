(ns advent-14.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def small-input-2 "resources/small-input-2.txt")
(def large-input "resources/large-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(defn- parse-line [l]
  (cond (str/starts-with? l "mask = ") (list :mask (replace {\0 0 \1 1} (vec (subs l 7))))
        (str/starts-with? l "mem[")
        (list :mem
              (Long/parseLong (subs l (inc (str/index-of l "[")) (str/index-of l "]")))
              (Long/parseLong (subs l (+ 2 (str/index-of l "=")))))
        :else nil))

(defn read-program [f]
  (map parse-line (read-lines f)))


;; Part 1
;; X bits in the mask have no effect, numeric bits overwrite the value to be stored.

(defn- apply-mask [x m]
  (reduce (fn [acc [bit val]]
            (if (= \X val) acc
                (if (= val 1) (bit-set acc bit) (bit-clear acc bit))))
          x
          (map-indexed list (reverse m))))

(defn run-program
  ([p] (run-program p [] {}))
  ([p mask mem]
   (if (empty? p) (reduce + (vals mem))
       (let [line (first p)
             inst (first line)
             arg1 (second line)
             arg2 (second (rest line))]
         (case inst
           :mask (run-program (rest p) arg1 mem)
           :mem (run-program (rest p) mask (assoc mem arg1 (apply-mask arg2 mask)))
           nil)))))


;; Part 2
;; 0 bits in the mask have no effect; 1 bits overwrite the value in the memory address;
;; X bits produce all possible combinations of values.
(defn- num-to-binary-vector [n]
  (replace {\0 0 \1 1} (vec (pp/cl-format nil "~,'0',B" n))))
  
(defn- binary-vector-to-num [v]
  (reduce #(+ (if (= 0 %2) 0 1) (bit-shift-left %1 1)) v))

(defn- parse-line-2 [l]
  (cond (str/starts-with? l "mask = ") (list :mask (replace {\0 0 \1 1} (vec (subs l 7))))
        (str/starts-with? l "mem[")
        (list :mem
              (num-to-binary-vector (Long/parseLong (subs l (inc (str/index-of l "[")) (str/index-of l "]"))))
              (Long/parseLong (subs l (+ 2 (str/index-of l "=")))))
        :else nil))

(defn read-program-2 [f]
  (map parse-line-2 (read-lines f)))

(defn- expand-to [bits x]
  (if (= bits (count x)) x
      (apply conj (vec (repeat (- bits (count x)) 0)) x)))

(defn- apply-mask-2 [x m]
  (let [pairs (map vector x m)]
    (vec (map (fn [[xval mval]]
                (case mval
                  \X \X
                  1 1
                  0 xval))
              pairs))))

(defn almost-flatten
  "From https://stackoverflow.com/a/5236875/625403"
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
    (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

(defn- make-variants
  ([v] (almost-flatten (make-variants [] v)))
  ([acc v]
   (if (empty? v) acc
       (if (= (first v) \X)
         (list
          (make-variants (conj acc 0) (rest v))
          (make-variants (conj acc 1) (rest v)))
         (make-variants (conj acc (first v)) (rest v))))))

(defn- write-to-mem
  [mem address mask x]
  (let [addresses (make-variants (apply-mask-2 address mask))]
    (reduce (fn [acc addr] (assoc acc (binary-vector-to-num addr) x)) mem addresses)))
  
(defn run-program-2
  ([p] (run-program-2 p [] {}))
  ([p mask mem]
   (if (empty? p) (reduce + (vals mem))
       (let [line (first p)
             inst (first line)
             arg1 (second line)
             arg2 (second (rest line))]
         (case inst
           :mask (run-program-2 (rest p) arg1 mem)
           :mem (run-program-2 (rest p) mask (write-to-mem mem (expand-to (count mask) arg1) mask arg2))
           nil)))))
