(ns advent-4.core)

(require '[clojure.string :as str])

(defrecord Passport [byr iyr eyr hgt hcl ecl pid cid])

; Adds to the current passport or, if the string is empty, starts a new one.
(defn collect-passport [coll x]
  (if (= x "")
    (conj coll [])
    (conj (vec (butlast coll)) (conj (last coll) x))))

(defn read-passports [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (reduce collect-passport [[]] (line-seq rdr))))

(defn string-to-passport [s]
  (map->Passport
  (apply merge
    (map
      (fn [astr]
        (let [pair (str/split astr #":")]
          (hash-map (keyword (first pair)) (second pair))))
      (str/split s #" ")))))

(string-to-passport "iyr:2017 ecl:blue")

(defn parse-passports [f]
  (let [passports (read-passports f)]
    (map string-to-passport
      (map (fn [coll] (str/join " " coll)) passports))))

(defn validate-passport [^Passport p]
  (and
    (not (nil? (.byr p)))
    (not (nil? (.iyr p)))
    (not (nil? (.eyr p)))
    (not (nil? (.hgt p)))
    (not (nil? (.hcl p)))
    (not (nil? (.ecl p)))
    (not (nil? (.pid p)))
    ;(not (nil? (.cid p)))
    ))

;(defn validate-passports [f]
;  (let [passports (parse-passports f)]
;    (map vector passports (map validate-passport passports))))

(defn count-valid-passports [f validator]
  (count
    (filter true?
      (map validator (parse-passports f)))))

;(count-valid-passports "/home/runner/advent-4/large-input.txt" validate-passport)

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.
;cid (Country ID) - ignored, missing or not.

(defn validate-number [nstr min max]
;  (print ["validate-number" nstr])
;  (newline)
  (and
    (not (nil? nstr))
    (let [n (Integer/parseInt nstr)]
      (and (>= n min) (<= n max)))))

(defn validate-fancy [^Passport p]
  (print p) (newline)
  (and
    (validate-passport p)
    (validate-number (.byr p) 1920 2002)
    (validate-number (.iyr p) 2010 2020)
    (validate-number (.eyr p) 2020 2030)
    (let [match (re-find #"(\d+)(cm|in)" (.hgt p))
          number (second match)]
;      (print match) (print (second match)) (newline)
      (if (= "cm" (last match))
        (validate-number number 150 193)
        (if (= "in" (last match))
          (validate-number number 59 76)
          false)))
    (not (nil? (re-matches #"\#[a-f0-9]{6}" (.hcl p))))
    (some #(= (.ecl p) %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
    (not (nil? (re-matches #"\d{9}" (.pid p))))))

;(count-valid-passports "/home/runner/advent-4/large-input.txt" validate-fancy)
