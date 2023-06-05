(ns day-09.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(def test-input '("ADVENT"
                  "A(1x5)BC"
                  "(3x3)XYZ"
                  "A(2x2)BCD(2x2)EFG"
                  "(6x1)(1x3)A"
                  "X(8x2)(3x3)ABCY"))

(defn split-it [s]
  (let [index (str/index-of s \()]
    (if (nil? index)
      s
      (let [beginning (subs s 0 index)
            more (subs s index)]
        (list beginning more)))))

(defn multiply [s]
  (let [[_ n rpts more] (re-matches #"\((\d+)x(\d+)\)(.+)" s)
        n (Long/parseLong n)
        rpts (Long/parseLong rpts)]
    (list (str/join (repeat rpts (subs more 0 n)))
          (subs more n))))

(defn decompress [s]
  (loop [decompressed "" compressed s]
    (let [split (split-it compressed)]
      (if (string? split)
        (str/join (list decompressed split))
        (let [[pre post] split
              [multiplied more] (multiply post)]
          (recur (str/join (list decompressed pre multiplied)) more))))))

;; (map (fn [s]
;;        (let [decompressed (decompress s)]
;;          (list (count decompressed) decompressed)))
;;      test-input)
;; => ((6 "ADVENT")
;;     (7 "ABBBBBC")
;;     (9 "XYZXYZXYZ")
;;     (11 "ABCBCDEFEFG")
;;     (6 "(1x3)A")
;;     (18 "X(3x3)ABC(3x3)ABCY"))

;; (map (fn [s]
;;        (let [decompressed (decompress s)]
;;          (list (count decompressed) decompressed)))
;;      (str/split-lines (slurp "input-09.txt")))
;; => ((74532
;;      "JVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWVJVWV..."))



;; Part 2
;; Also decompress markers inside compressed areas.
(def test-input-2 '("(3x3)XYZ"
                    "X(8x2)(3x3)ABCY"
                    "(27x12)(20x12)(13x14)(7x10)(1x12)A"
                    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"))

;; get prefix (25x3): n=25 rpts=3
;; get area covered by prefix: "(3x3)ABC(2x3)XY(5x2)PQRST"
;; length = rpts x length of area = (* 3 (length-of "(3x3)ABC(2x3)XY(5x2)PQRST")
;; length of <a><pref><pref-area><b> = (+ a (* pref-rpts (length pref-area)) b)
(defn length-of [s]
  (if (or (nil? s) (empty? s))
    0
    (let [[_ chars more :as match] (re-matches #"([A-Z]+)(.*)" s)]
      (if match
        (+ (count chars) (length-of more))
        (let [[_ multiplier n rpts more :as match] (re-matches #"(\((\d+)x(\d+)\))(.+)" s)]
          (if match
            (let [n (Long/parseLong n) rpts (Long/parseLong rpts)]
              (+ (* rpts (length-of (subs more 0 n)))
                 (length-of (subs more n))
                 ))))))))

;; (map length-of test-input-2)
;; => (9 20 241920 445)

;; (length-of (first (str/split-lines (slurp "input-09.txt"))))
;; => 11558231665

