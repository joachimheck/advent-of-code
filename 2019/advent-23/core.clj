(ns advent-23.core)

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

;; Day 23: Category Six

;; Part 1
;; What is the Y value of the first packet sent to address 255?
(defn parse-line [line]
  (reduce (fn [m [k v]] (assoc m k v)) {} (map-indexed list (load-string (str/join (list "[" line "]"))))))

(defn parse-input [f]
  (parse-line (first (read-lines f))))

(defn pad-array [array]
  (let [remainder (rem (count array) 4)]
    (if (= remainder 0)
      array
      (apply conj array (vec (repeat (- 4 remainder) 0))))))

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
        (= p-mode RELATIVE) (+ relative-base n)
        :else "Unknown p-mode"))

(defn op-add [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (+ (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-mul [a b c p-modes {:keys [ip memory relative-base] :as state}]
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory
                        (get-address c (get p-modes 2 0) relative-base)
                        (* (get-value a memory (get p-modes 0 0) relative-base)
                           (get-value b memory (get p-modes 1 0) relative-base)))))

(defn op-in [a b c p-modes {:keys [ip memory inputs relative-base] :as state}]
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
  (assoc state
         :ip (+ ip 4)
         :memory (assoc memory (get-address c (get p-modes 2 0) relative-base)
                        (if (< (get-value a memory (get p-modes 0 0) relative-base)
                               (get-value b memory (get p-modes 1 0) relative-base))
                          1
                          0))))

(defn op-eq [a b c p-modes {:keys [ip memory relative-base] :as state}]
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
  ((get ops-by-code opcode) a b c p-modes state))

(defn fix-inputs [states]
  (reduce-kv (fn [states k v]
               (if (empty? (get-in states [k :inputs]))
                 (assoc-in states [k :inputs] [-1])
                 states))
             states
             states))

(defn send-packets [states nat-packet]
  (reduce-kv (fn [[states nat-packet] k v]
               (let [[a x y :as outputs] (get-in states [k :outputs])
                     new-states (if (= (count outputs) 3)
                                  (let [new-states (assoc-in states [k :outputs] [])]
                                    (if (some nil? outputs) (println "at least one nil output:" a x y))
                                    (if (< a 50)
                                      (update-in new-states [a :inputs] #(conj % x y))
                                      new-states))
                                  states)
                     _ (if (and (= (count outputs) 3) (< a 50) (nil? (get-in new-states [a :inputs]))) (println "nil inputs" a x y))
                     new-nat-packet (if (and (= (count outputs) 3)
                                             (= a 255)
                                             ;; (every? some? [x y])
                                             (not-every? nil? [x y])
                                             )
                                      [x y]
                                      nat-packet)]
                 (if (some nil? new-nat-packet) (println "at least one nil in new-nat-packet:" new-nat-packet a x y))
                 [new-states new-nat-packet]))
             [states nat-packet]
             states))

(defn run-computers [program]
  (let [states (into {} (for [i (range 50)] [i {:ip 0 :memory program :inputs [i] :outputs [] :relative-base 0}]))]
    (loop [step 0
           states states]
      (let [advanced (reduce
                      (fn [states address]
                        ;; (println "reduce" "address" address "state" (dissoc (get states address) :memory))
                        (let [state (get states address)
                              {:keys [ip memory]} state
                              _ (if (nil? ip)
                                  (println "nil ip" "address" address "state" state))
                              [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
                              {:keys [opcode p-modes]} (parse-operation operation)
                              {:keys [ip outputs] :as new-state} (do-op opcode a b c p-modes state)]
                          ;; (println "memory" (sort (:memory state)))
                          ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
                          ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
                          (if (= ip -1)
                            (str "Error in computer" address "outputs:" outputs)
                            (assoc states address new-state))))
                      states
                      (range 50))
            inputs-fixed (fix-inputs advanced)
            [packets-sent packet-255] (send-packets inputs-fixed nil)
            [x y] packet-255
            _ (if packet-255 (println "packet-255" packet-255))]
        (if y
          {:steps step :y y :packet-255 packet-255}
          (recur (inc step) packets-sent))))))




;; Part 2
;; What is the first Y value delivered by the NAT to the computer at address 0 twice in a row?

(defn update-recv-count [recv-count opcode]
  (cond (= (get ops-by-code opcode) op-out)
        0
        (= (get ops-by-code opcode) op-in)
        (inc recv-count)
        :else
        recv-count))

(defn all-idle? [states]
  (every? (fn [state] (and (> (:recv-count state) 2)
                           (= [-1] (:inputs state))))
          (vals states)))

(defn idle-count [states]
  (count
   (filter (fn [state] (and (> (:recv-count state) 2)
                            (= [-1] (:inputs state))))
           (vals states))))

(defn any-nil-inputs? [states]
  (not (empty? (filter #(nil? (:inputs %)) (vals states)))))

(defn run-computers-with-nat [program]
  (let [states (into {} (for [i (range 50)] [i {:ip 0 :memory program :inputs [i] :outputs [] :relative-base 0 :recv-count 0}]))]
    (loop [step 0
           states states
           nat-packet nil
           last-ys [nil nil]]
      (if (and (not-any? nil? last-ys) (apply = last-ys))
        {:y (first last-ys) :steps step}
        (let [advanced (reduce
                        (fn [states address]
                          ;; (println "reduce" "address" address "state" (dissoc (get states address) :memory))
                          (let [state (get states address)
                                {:keys [ip memory]} state
                                _ (if (nil? ip)
                                    (println "nil ip" "address" address "state" state))
                                [operation a b c] (for [i (range 4)] (get memory (+ ip i) 0))
                                {:keys [opcode p-modes]} (parse-operation operation)
                                {:keys [ip outputs] :as new-state}
                                (try (do-op opcode a b c p-modes state)
                                     (catch Exception e
                                       (do
                                         (println "exception" "address" address (get ops-by-code opcode) a b c p-modes (dissoc state :memory))
                                         (throw e))))
                                new-state (update new-state :recv-count #(update-recv-count % opcode))]
                            ;; (println "memory" (sort (:memory state)))
                            ;; (println "loop ip" (:ip state) "rb" (:relative-base state) "outputs" (:outputs state))
                            ;; (println "loop" "memory" memory "inputs" inputs "outputs" outputs)
                            (if (= ip -1)
                              (str "Error in computer" address "outputs:" outputs)
                              (assoc states address new-state))))
                        states
                        (range 50))
              inputs-fixed (fix-inputs advanced)
              [packets-sent [nat-x nat-y :as new-nat-packet]] (send-packets inputs-fixed nat-packet)
              [nat-handled new-nat-packet new-last-ys] (if (and (all-idle? packets-sent) (some? new-nat-packet))
                                                         [(assoc-in packets-sent [0 :inputs] new-nat-packet)
                                                          nil
                                                          [nat-y (first last-ys)]]
                                                         [packets-sent new-nat-packet last-ys])]
          (if (not= last-ys new-last-ys)
            (println "new last ys" new-last-ys))
          (recur (inc step) nat-handled new-nat-packet new-last-ys))))))

;; ---> answer <---
;; 23701

;; (time (run-computers-with-nat (parse-input large-input)))
;; new last ys [23701 nil]
;; new last ys [22029 23701]
;; new last ys [20698 22029]
;; new last ys [19697 20698]
;; new last ys [18970 19697]
;; new last ys [18451 18970]
;; new last ys [18085 18451]
;; new last ys [17827 18085]
;; new last ys [17646 17827]
;; new last ys [17519 17646]
;; new last ys [17430 17519]
;; new last ys [17368 17430]
;; new last ys [17325 17368]
;; new last ys [17295 17325]
;; new last ys [17274 17295]
;; new last ys [17259 17274]
;; new last ys [17248 17259]
;; new last ys [17241 17248]
;; new last ys [17236 17241]
;; new last ys [17232 17236]
;; new last ys [17229 17232]
;; new last ys [17227 17229]
;; new last ys [17226 17227]
;; new last ys [17225 17226]
;; new last ys [17225 17225]
;; "Elapsed time: 29510.0391 msecs"
;; {:y 17225, :steps 104003}
