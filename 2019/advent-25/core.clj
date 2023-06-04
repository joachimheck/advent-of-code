(ns advent-25.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])
(require '[clojure.instant :as instant])
(require '[clojure.walk :as walk])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 23: Cryostasis

;; Part 1
;; 
(defn parse-line [line]
  (reduce (fn [m [k v]] (assoc m k v)) {} (map-indexed list (load-string (str/join (list "[" line "]"))))))

(defn parse-input [f]
  (parse-line (first (read-lines f))))

(defn digits [n]
  (if (< n 10)
    [n]
    (let [quotient (quot n 10)
          remainder (rem n 10)]
      (conj (digits quotient) remainder))))

(defn parse-operation [n]
  (let [n-digits (digits n)
        opcode (if (= (count n-digits) 1)
                 n
                 (+ (* 10 (nth n-digits (- (count n-digits) 2))) (last n-digits)))
        p-modes (vec (reverse (drop-last 2 n-digits)))]
    {:opcode opcode
     :p-modes p-modes}))

(def POSITION 0)
(def IMMEDIATE 1)
(def RELATIVE 2)

(defn get-value [n memory p-mode relative-base]
  (cond (= p-mode POSITION) (let [address n]
                              (if (< address 0)
                                "Negative address"
                                (get memory address 0)))
        (= p-mode IMMEDIATE) n
        (= p-mode RELATIVE) (let [address (+ relative-base n)]
                              (if (< address 0)
                                "Negative address"
                                (get memory address 0)))
        :else "Unknown p-mode"))

(defn get-address [n p-mode relative-base]
  (cond (= p-mode POSITION) n
        (= p-mode IMMEDIATE) n
        (= p-mode RELATIVE) (+ relative-base n)
        :else "Unknown p-mode"))

(defn op-add [a b c p-modes {:keys [ip memory relative-base] :as state}]
  ;; (if (= (get-address c (get p-modes 2 0) relative-base) 4810)
  ;;   (println "op-add into 4810:" ip a b c p-modes))
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (+ (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-mul [a b c p-modes {:keys [ip memory relative-base] :as state}]
  ;; (if (= (get-address c (get p-modes 2 0) relative-base) 4810)
  ;;   (println "op-mul into 4810:" ip a b c p-modes))
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory
                        (get-address c (get p-modes 2 0) relative-base)
                        (* (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-in [a b c p-modes {:keys [ip memory inputs relative-base] :as state}]
  ;; (if (= (get-address a (get p-modes 0 0) relative-base) 4810)
  ;;   (println "op-in into 4810:" ip a b c p-modes))
  (assoc state
         :ip (+ ip 2)
         :memory (assoc memory (get-address a (get p-modes 0 0) relative-base) (* 1 (first inputs)))
         :inputs (vec (rest inputs))))

(defn op-out [a b c p-modes {:keys [ip memory outputs relative-base] :as state}]
  (assoc state
         :ip (+ ip 2)
         :outputs (conj outputs (get-value a memory (get p-modes 0 0) relative-base))))

(defn op-jt [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (if (not= 0 (get-value a memory (get p-modes 0 0) relative-base))
               (get-value b memory (get p-modes 1 0) relative-base)
               (+ ip 3))))

(defn op-jf [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
           :ip (if (= 0 (get-value a memory (get p-modes 0 0) relative-base))
                 (get-value b memory (get p-modes 1 0) relative-base)
                 (+ ip 3))))

(defn op-lt [a b c p-modes {:keys [ip memory relative-base] :as state}]
  ;; (if (= (get-address c (get p-modes 2 0) relative-base) 4810)
  ;;   (println "op-lt into 4810:" ip a b c p-modes))
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (if (< (get-value a memory (get p-modes 0 0) relative-base)
                               (get-value b memory (get p-modes 1 0) relative-base))
                          1
                          0))))

(defn op-eq [a b c p-modes {:keys [ip memory relative-base] :as state}]
  ;; (if (= (get-address c (get p-modes 2 0) relative-base) 4810)
  ;;   (println "op-eq into 4810:" ip a b c p-modes))
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (if (= (get-value a memory (get p-modes 0 0) relative-base)
                               (get-value b memory (get p-modes 1 0) relative-base))
                          1
                          0))))

(defn op-rb [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 2)
         :relative-base (+ relative-base (get-value a memory (get p-modes 0 0) relative-base))))

(defn op-ex [a b c p-modes state]
  (assoc state :ip -1))

(def ops-by-code {1 op-add
                  2 op-mul
                  3 op-in
                  4 op-out
                  5 op-jt
                  6 op-jf
                  7 op-lt
                  8 op-eq
                  9 op-rb
                  99 op-ex
                  })

(defn do-op [opcode a b c p-modes state]
  (if (nil? (get ops-by-code opcode)) (println "no op for opcode" opcode))
  ((get ops-by-code opcode) a b c p-modes state))

(def initial-inputs (mapv int (str/join (str \newline)
                                        ["south" "west" "take fuel cell"
                                         "west" "take easter egg"
                                         "east" "east" "north" "north" "north" "east" "east" "take cake"
                                         "west" "west" "south" "south" "east" "take ornament"
                                         "east" "take hologram"
                                         "east" "take dark matter"
                                         "north" "north" "east" "take klein bottle"
                                         "north" "take hypercube"
                                         "north" "drop cake" "drop fuel cell" "drop hypercube" "drop ornament"
                                         "inv" ""])))

(def arg-descriptions {
                       "op-add" [:value :value :address]
                       "op-mul" [:value :value :address]
                       "op-in" [:address]
                       "op-out" [:value]
                       "op-jt" [:value :value]
                       "op-jf" [:value :value]
                       "op-lt" [:value :value :address]
                       "op-eq" [:value :value :address]
                       "op-rb" [:value]
                       "op-ex" []
                       })

(defn format-command [ip operation a b c memory relative-base]
  (let [{:keys [opcode p-modes]} (parse-operation operation)
        op-name (second (re-find #"/(.+)@" (demunge (str (get ops-by-code opcode)))))
        description (get arg-descriptions op-name)
        arg-count (count description)]
    (format "%d: %6s %s  [%5d %s]  rb: %s p-modes: %s @4810: %d"
            ip
            op-name
            (str/join " " (map-indexed (fn [i x] (cond
                                                   (= (get description i) :value)
                                                   (format "%4d" (get-value x memory (get p-modes i 0) relative-base))
                                                   (= (get description i) :address)
                                                   (format "%4d" (get-address x (get p-modes i 0) relative-base))
                                                   :else "    "))
                                       [a b c]))
            operation
            (str/join " " (concat (for [i (range (count description))] (format "%4d" (get [a b c] i)))
                                       (for [i (range (- 3 (count description)))] "    ")))
            (format "%5d" relative-base)
            p-modes
            (get memory 4810))))

(defn run-program [program initial-inputs debug-lines]
  (loop [state {:ip 0 :memory program :inputs initial-inputs :outputs [] :relative-base 0} commands []]
    ;; (println "memory" (sort (:memory state)))
    ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
    ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
    (let [{:keys [ip memory relative-base]} state
          [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
          {:keys [opcode p-modes]} (parse-operation operation)
          new-commands (conj commands (format-command ip operation a b c memory relative-base))
          ]
      (if (or (= opcode 0) (= ip -1))
        (do
          ;; (println "error, last" debug-lines "commands:")
          ;; (doseq [l (take-last debug-lines commands)]
          ;;   (println l))
          (println (str/join (map char (:outputs state)))))
        (if (and (= op-in (get ops-by-code opcode)) (empty? (:inputs state)))
          (do (print (str/join (map char (:outputs state))))
              (flush)
              (let [user-input (read-line)
                    _ (println ">" user-input)
                    state-with-input (-> state
                                         (assoc :inputs (conj (mapv int user-input) 10))
                                         (assoc :outputs []))]
                (recur (do-op opcode a b c p-modes state-with-input) new-commands)))
          (recur (do-op opcode a b c p-modes state) new-commands))))))

(def day-9 "day-9.txt")


;; ...

;; Items in your inventory:
;; - easter egg
;; - hologram
;; - dark matter
;; - klein bottle

;; Command?
;; > west



;; == Pressure-Sensitive Floor ==
;; Analyzing...

;; Doors here lead:
;; - east

;; A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
;; Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
;; "Oh, hello! You should be able to get in by typing 1090617344 on the keypad at the main airlock."

;; nil
