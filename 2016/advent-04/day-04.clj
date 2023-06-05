(ns day-04.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1
(def test-input '("aaaaa-bbb-z-y-x-123[abxyz]"
                  "a-b-c-d-e-f-g-h-987[abcde]"
                  "not-a-real-room-404[oarel]"
                  "totally-real-room-200[decoy]"))

(defn ->checksum [name]
  (-> name
      (str/replace "-" "")
      frequencies
      (->>
       (sort-by (fn [[ltr cnt]] (- (* 1000 cnt) (int ltr))))
       reverse
       (take 5)
       (map first)
       str/join)))

(defn parse-room [[_ name sector checksum]]
  (list name (Long/parseLong sector) checksum (->checksum name)))

(defn parse-rooms [rooms]
  (->> rooms
   (map #(re-matches #"([a-z\-]+)-(\d+)\[(\w+)\]" %))
   (map parse-room)
   (map (fn [[name sec-id chksum real-chksum]]
          (assoc {}
                 :name name
                 :sector-id sec-id
                 :checksum chksum
                 :real-checksum real-chksum)))))

(defn real-room? [room]
  (= (:checksum room) (:real-checksum room)))

(defn sum-real-room-sector-ids [rooms]
  (-> rooms
      parse-rooms
      (->>
       (filter real-room?)
       (map #(get % :sector-id))
       (reduce + 0))))

;; (sum-real-room-sector-ids test-input)
;; => 1514

(defn read-input [f]
  (str/split-lines (slurp f)))

;; (sum-real-room-sector-ids (read-input "input-04.txt"))
;; => 409147




;; Part 2
;; Decrypt the list

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn rotate [l shift]
  (nth (drop-while #(not= % l) (take (+ 26 shift) (cycle alphabet))) shift))

(defn decrypt [name sector-id]
  (str/join (map (fn [l] (if (= l \-) \space (rotate l sector-id))) name)))

(decrypt "qzmt-zixmtkozy-ivhz" 343)

(defn decrypt-rooms [rooms]
  (-> rooms
      parse-rooms
      (->>
       (filter real-room?)
       (map #(list (get % :sector-id) (decrypt (get % :name) (get % :sector-id)))))))

;; (decrypt-rooms (read-input "input-04.txt"))
;; => ...
;; => (991 "northpole object storage")
;; => ...
