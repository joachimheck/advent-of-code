(ns advent-11.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

(def profile-times (atom {}))

(defmacro profile [name exp]
  `(let [start# (System/nanoTime)
         result# (doall ~exp)
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (swap! profile-times update ~name #(+ (or % 0) elapsed#))
     result#))

(defn parse-line [line]
  (let [substrings (str/split line #":? ")]
    {:name (first substrings)
     :outputs (rest substrings)}))


(defn parse-input [input]
  (reduce (fn [acc line]
                 (let [parsed (parse-line line)
                       name (:name parsed)
                       outputs (:outputs parsed)]
                   (assoc acc name (concat (or (get acc name) []) outputs))))
               {}
               (read-lines input)
               ))

;; Part 1
;; How many paths lead from "you" to "out"?

(defn find-paths-to-out [input start]
  (let [output-map (parse-input input)]
    (loop [open-paths [[start]]
           finished []
           n 0]
      (println "n" n)
      (cond (empty? open-paths)
            {:result finished}
            (> n 10)
            {:error :error-over-n
             :open-paths open-paths
             :finished finished}
            :else
            (let [extended (apply concat (for [p open-paths]
                                           (let [end (last p)
                                                 nexts (get output-map end)]
                                             (for [n (remove (set p) nexts)]
                                               (conj p n)))))
                  new-extended (remove #(= (last %) "out") extended)
                  new-finished (concat finished (filter #(= (last %) "out") extended))]
              (recur new-extended new-finished (inc n)))))))

(defn count-paths-to-out [input]
  (count (:result (find-paths-to-out input "you"))))


;; (time (count-paths-to-out small-input))
;; "Elapsed time: 2.9757 msecs"
;; 5
;; (time (count-paths-to-out large-input))
;; "Elapsed time: 50.8217 msecs"
;; 701



;; Part 2
;; Count the paths from svr to out that also visit both dac and fft.
(defn has-duplicates? [path]
  (> (apply max (vals (frequencies path))) 1))

(deftest test-has-duplicates?
  (is (true? (has-duplicates? ["you" "bbb" "eee" "out" "eee"])))
  (is (false? (has-duplicates? ["you" "bbb" "eee" "out"]))))

(defn is-success? [path]
  (boolean (and (some #{"dac"} path) (some #{"fft"} path) (= "out" (last path)))))

(deftest test-is-success?
  (is (true? (is-success? ["svr" "dac" "fft" "out"])))
  (is (false? (is-success? ["svr" "aaa"]))))

(defn is-failure? [path]
  (boolean (or (has-duplicates? path) (and (= "out" (last path)) (not (is-success? path))))))

(defn find-paths-to-out-2 [input]
  (let [output-map (parse-input input)]
    (loop [open-paths [["svr"]]
           successes []
           failures []
           n 0]
      (println "n" n)
      ;; (println "open-paths" open-paths)
      (cond (empty? open-paths)
            {:successes successes}
            (> n 10)
            {:error :error-over-n
             :open-paths open-paths
             :successes successes
             :failures failures}
            :else
            (let [extended (apply concat (for [p open-paths]
                                           (let [end (last p)
                                                 nexts (get output-map end)]
                                             (for [n nexts]
                                               (conj p n)))))
                  ;; _ (println "extended" extended)
                  {new-successes true others false} (group-by is-success? extended)
                  {new-failures true new-extended false} (group-by is-failure? others)
                  ;; _ (println "new-extended" new-extended)
                  ]
              (recur new-extended new-successes new-failures (inc n)))))))

(defn count-paths-to-out-2 [input]
  (count
   (filter #(and (some #{"dac"} %) (some #{"fft"} %))
           (:result (find-paths-to-out input "svr")))))

(defn find-all-paths [input start]
  (let [output-map (parse-input input)]
    (loop [open-paths [[start]]
           with-duplicates []
           n 0]
      (println "n" n)
      ;; (println "open-paths" open-paths)
      (cond (empty? open-paths)
            {:with-duplicates with-duplicates}
            (> n 10)
            {:error :error-over-n
             :with-duplicates with-duplicates}
            :else
            (let [extended (apply concat (for [p open-paths]
                                           (let [end (last p)
                                                 nexts (get output-map end)]
                                             (for [n nexts]
                                               (conj p n)))))
                  new-with-duplicates (filter has-duplicates? extended)
                  ]
              (recur extended new-with-duplicates (inc n)))))))

(def memo-data (atom {}))
(def fn-count (atom 0))
(def memo-count (atom 0))

(defn find-paths [node target path-map]
  (if (>= @fn-count 500)
    (throw (Exception. "error-over-n"))
    (do
      (if (zero? (mod @fn-count 100))
        (println "fn-count:" @fn-count))
      (reset! fn-count (inc @fn-count))
      (let [memo-result (get @memo-data node)]
        (if memo-result
          memo-result
          (let [sub-paths (mapcat #(find-paths % target path-map) (get path-map node))
                _ (if (some #{true} (map has-duplicates? sub-paths))
                    (do
                      (println "sub-paths with duplicates:" sub-paths)
                      (throw (Exception. "error: duplicates!"))))
                result (if (or (= target node) (empty? sub-paths))
                         (list (list node))
                         (map #(cons node %) sub-paths))]
            (reset! memo-count (inc @memo-count))
            (reset! memo-data (assoc @memo-data node result))
            result))))))

;; (let [path-map (parse-input large-input)]
;;                   (reset! memo-data {})
;;                   (reset! fn-count 0)
;;                   (reset! memo-count 0)
;;                   (time (try (let [result (find-paths "svr" path-map)]
;;                                (println "result count" (count result)))
;;                              (catch Exception e (println "fn-count" @fn-count "m-d-count" (count @memo-data)))))
;;                   {:fn-count @fn-count :memo-count @memo-count :map-count (count path-map)})

;; I'm trying to first find all the paths to "dac" and "fft", then all the paths to "out", but it isn't working.
