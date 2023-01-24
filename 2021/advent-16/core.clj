(ns advent-16.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Day 16: Packet Decoder

;; Part 1
;; Interpret the structure of a Buoyancy Interchange Transmission System packet.
(def hex-mapping {\0 [0 0 0 0]
                  \1 [0 0 0 1]
                  \2 [0 0 1 0]
                  \3 [0 0 1 1]
                  \4 [0 1 0 0]
                  \5 [0 1 0 1]
                  \6 [0 1 1 0]
                  \7 [0 1 1 1]
                  \8 [1 0 0 0]
                  \9 [1 0 0 1]
                  \A [1 0 1 0]
                  \B [1 0 1 1]
                  \C [1 1 0 0]
                  \D [1 1 0 1]
                  \E [1 1 1 0]
                  \F [1 1 1 1]})

(defn parse-hex [s]
  (apply list ; Realize the list.
   (apply concat
          (map hex-mapping s))))

(defn parse-binary [s]
  (Long/parseLong (str/join s) 2))

(defn packet-version [p]
  (take 3 p))

(def LITERAL 4)

(defn parse-literal [version type-id s]
  (loop [remaining-s s acc [] bits-read 6]
    (if (empty? remaining-s)
      :empty!
      (let [[prefix a b c d] (take 5 remaining-s)
            bits-read (+ 5 bits-read)
            remaining-s (drop 5 remaining-s)
            skip-amount (- 4 (mod bits-read 4))]
        ;; (println prefix a b c d)
        (if (= prefix 0)
            {:version version
             :type-id type-id
             :value (parse-binary (apply conj acc [a b c d]))
             :skip (take skip-amount remaining-s)
             :bits-read bits-read
             :skip-amount skip-amount
             :remaining remaining-s}
            (recur remaining-s (apply conj acc [a b c d]) bits-read))))))

(def LENGTH-OP 0)

(def PACKETS-OP 1)

(defn parse-operator [version type-id s]
  (let [length-type-id (first s)
        contents (rest s)]
    (if (= length-type-id LENGTH-OP)
      (let [length (parse-binary (take 15 contents))]
        (loop [remaining (take length (drop 15 contents)) values []]
          (if (empty? remaining)
            {:version version
             :type-id type-id
             :value values
             :remaining (drop (+ 15 length) contents)}
            (let [result (parse-packet remaining)]
              (recur (:remaining result) (conj values result))))))
      (let [packet-count (parse-binary (take 11 contents))]
        (loop [remaining (drop 11 contents) values [] packets-left packet-count]
          (if (= packets-left 0)
            {:version version
             :type-id type-id
             :value values
             :remaining remaining}
            (let [result (parse-packet remaining)]
              (recur (:remaining result) (conj values result) (dec packets-left)))))))))

(defn parse-packet [p]
  (if (not (empty? p))
    (let [version (parse-binary (take 3 p))
          type-id (parse-binary (take 3 (drop 3 p)))
          contents (drop 6 p)]
      (if (= type-id LITERAL)
        (parse-literal version type-id contents)
        (parse-operator version type-id contents)))))

(defn sum-version-numbers [p]
  (apply + (map :version (tree-seq #(not= (:type-id %) 4) #(:value %) p))))

(defn run-tests []
  (let [results (list (list 2021 (parse-binary (:value (parse-packet (parse-hex "D2FE28")))))
                      (list '(10 20) (map parse-binary (map :value (:value (parse-packet (parse-hex "38006F45291200"))))))
                      (list '(1 2 3) (map parse-binary (map :value (:value (parse-packet (parse-hex "EE00D40C823060"))))))
                      (list 16 (sum-version-numbers (parse-packet (parse-hex "8A004A801A8002F478"))))
                      (list 12 (sum-version-numbers (parse-packet (parse-hex "620080001611562C8802118E34"))))
                      (list 23 (sum-version-numbers (parse-packet (parse-hex "C0015000016115A2E0802F182340"))))
                      (list 31 (sum-version-numbers (parse-packet (parse-hex "A0016C880162017C3686B18A3D4780"))))
                      )]
    (println results)
    (every? identity (map (fn [[a b]] (= a b) ) results))))


;; (time (sum-version-numbers (parse-packet (parse-hex (first (read-lines large-input))))))
;; "Elapsed time: 12.7761 msecs"
;; 986



;; Part 2
;; Fully decode a Buoyancy Interchange Transmission System packet.
(def type-id-fns {0 +
                  1 *
                  2 min
                  3 max
                  5 #(if (> %1 %2) 1 0)
                  6 #(if (< %1 %2) 1 0)
                  7 #(if (= %1 %2) 1 0)})

(defn evaluate-hex-packet [packet]
  (evaluate-packet (parse-packet (parse-hex packet))))

(defn evaluate-packet [p]
  (let [type-id (:type-id p)]
    (if (= type-id LITERAL)
      (:value p)
      (apply (type-id-fns type-id) (map evaluate-packet (:value p))))))

(defn run-tests-2 []
  (let [results (list (list 3 (evaluate-hex-packet "C200B40A82"))
                      (list 54 (evaluate-hex-packet "04005AC33890"))
                      (list 7 (evaluate-hex-packet "880086C3E88112"))
                      (list 9 (evaluate-hex-packet "CE00C43D881120"))
                      (list 1 (evaluate-hex-packet "D8005AC2A8F0"))
                      (list 0 (evaluate-hex-packet "F600BC2D8F"))
                      (list 0 (evaluate-hex-packet "9C005AC2F8F0"))
                      (list 1 (evaluate-hex-packet "9C0141080250320F1802104A08"))
                      )]
    (println results)
    (every? identity (map (fn [[a b]] (= a b) ) results))))

;; (time (evaluate-hex-packet (first (read-lines large-input))))
;; "Elapsed time: 11.0153 msecs"
;; 18234816469452
