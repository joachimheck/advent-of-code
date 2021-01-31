(ns day-05.core
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;; Part 1
(defn md5 [^String s]
    (let [algorithm (MessageDigest/getInstance "MD5")
          raw (.digest algorithm (.getBytes s))]
        (format "%032x" (BigInteger. 1 raw))))

(defn hack-password [door-id]
 (->> (iterate inc 0)
      (map (fn [n] (str/join (list door-id (str n)))))
      (map md5)
      (filter #(str/starts-with? % "00000"))
      (take 8)
      (map #(subs % 5 6))
      (str/join)))

;; (time (hack-password "abbhdwsy"))
;; => "801b56a7"
;; "Elapsed time: 35478.0584 msecs"



;; Part 2
(defn hack-better-password [door-id]
  (->> (iterate inc 0)
       (map (fn [n] (str/join (list door-id (str n)))))
       (map md5)
       (keep #(let [[_ pos val :as match] (re-matches #"00000(\d)(\w).+" %)]
                (when (and match (< (Long/parseLong pos) 8))
                  [(Long/parseLong pos) val])))
       ;; Fill each position but skip positions we've already filled. 
       (reduce
        (fn [m [pos val]]
          (if (not (get m pos))
            (let [result (assoc m pos val)]
              (if (= (count result) 8)
                (reduced result)
                result))
            m))
        {})
       sort
       (map second)
       str/join))

;; (time (hack-better-password "abc"))
;; => "05ace8e3"
;; "Elapsed time: 63214.2782 msecs"

;; (time (hack-better-password "abbhdwsy"))
;; => "424a0197"
;; "Elapsed time: 119724.4028 msecs"
