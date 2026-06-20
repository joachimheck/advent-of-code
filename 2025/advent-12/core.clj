(ns advent-12.core)

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

(defn parse-input [input]
  (let [lines (read-lines input)
        groups (->> lines
                    (partition-by #{""})
                    (remove #{'("")})
                    (group-by #(boolean (re-matches #"\d+:" (first %))))
                    )
        shape-inputs (get groups true)
        region-inputs (filter #(re-find #"\d+x\d+" %) lines)
        ]
    {:shapes (map (fn [l]
                    {(parse-long (second (re-matches #"(\d+):" (first l))))
                     (rest l)})
                  shape-inputs)
     ;; "4x4: 0 0 0 0 2 0"
     ;; :regions (map (fn [r]
     ;;                 (println "matching" r)
     ;;                 (re-matches #"(\d+)x(\d+):(.+)" r))#"
     ;;               region-inputs)
     :regions (map (fn [r]
                     (rest (re-find #"(\d+)x(\d+): (\d+) (\d+) (\d+) (\d+) (\d+) (\d+)" r)))
                   region-inputs)
     }
    ))

;; Part 1
