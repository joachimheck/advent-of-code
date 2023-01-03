(ns advent-18.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "resources/small-input.txt")
(def large-input "resources/large-input.txt")

(defn read-input [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall (line-seq rdr))))

;; Part 1
;; Evaluate math expressions

;; Planned plan:
;; things separated be spaces are expressions
;; 
;;   n = \d+
;;   op = +|*
;;   ex = n
;;        \(ex\)
;;        n op (ex)+

;; use a stack -- scan input
;; push numbers, operators and \(s
;; when we find ")" pop to previous "(" and evaluate what's between.
;;
;; Actual plan:
;; when we hit "(", grab that parenthetical expression, remove the parens, and evaluate
;; only call evaulate for the whole expression after all parens have been removed
;; push values unless the value is a number and the previous was an operator, then operate
(defn token-type [t]
  (cond (re-matches #"\d+" t) :number
        (re-matches #"\+|\*" t) :operator
        (re-matches #"\(" t) :open
        (re-matches #"\)" t) :close
        ))  

(defn token [val type]
  {:value val :type type})

(defn tokenize
  ([e] (tokenize e []))
  ([e tokens]
   (if (empty? e) tokens
       (let [[_ val rest] (re-matches #"\s*(\d+|\+|\*|\(|\))(.*)" e)
             type (token-type val)]
         (tokenize rest (conj tokens
                              (cond (= type :number) (token (Long/parseLong val) type)
                                    (= type :operator) (token (resolve (symbol val)) type)
                                    :else (token val type))))))))

(defn pop-until-open
  ([stack] (pop-until-open stack []))
  ([stack acc]
   (if (= :open (:type (peek stack))) acc
       (pop-until-open (pop stack) (conj acc (peek stack))))))

(defn get-paren-exp
  ([vals] (get-paren-exp vals 0 []))
  ([[v & vals] depth exp]
   (cond (and (= depth 0) (seq exp)) exp
         (= (:type v) :open) (get-paren-exp vals (inc depth) (conj exp v))
         (= (:type v) :close) (get-paren-exp vals (dec depth) (conj exp v))
         :else (get-paren-exp vals depth (conj exp v)))))

(defn contains-parens? [exp]
  (some #(= :open (:type %)) exp))

(defn reduce-parens [exp]
  (if (not (contains-parens? exp)) exp
      (let [before (take-while #(not= :open (:type %)) exp)
            paren-exp (get-paren-exp (drop (count before) exp))
            reduced (token (eval-exp (rest (butlast paren-exp))) :number)
            after (drop (+ (count before) (count paren-exp)) exp)]
        (reduce-parens (apply conj (vec before) reduced (vec after))))))

(defn eval-exp
  ([tokens] (eval-exp (reduce-parens tokens) '()))
  ([tokens stack]
   (if (empty? tokens)
     (if (= 1 (count stack))
       (:value (first stack))
       (eval-exp stack '()))
     (let [tkn (first tokens)
           type (:type tkn)]
       (eval-exp (rest tokens)
                 (cond 
                   (or (= type :operator) (empty? stack)) (conj stack tkn)
                   :else
                   (let [op (:value (first stack))
                         arg (:value (second stack))
                         newstack (pop (pop stack))
                         result (op arg (:value tkn))]
                     (conj newstack (token result :number)))
                   ))))))

;; (map (comp eval-exp tokenize) (read-input small-input))
;; (51 26 437 12240 13632)
;;
;; (reduce + (map (comp eval-exp tokenize) (read-input large-input)))
;; 21022630974613




;; Part 2
;; Addition takes precedence over multiplication
;; Plan: remove parentheses, then process only additions, then only multiplications.
(def operators (list (resolve (symbol "+")) (resolve (symbol "*"))))

(defn reduce-parens-2 [exp]
  (if (not (contains-parens? exp)) exp
      (let [before (take-while #(not= :open (:type %)) exp)
            paren-exp (get-paren-exp (drop (count before) exp))
            reduced (token (eval-exp-2 (rest (butlast paren-exp))) :number)
            after (drop (+ (count before) (count paren-exp)) exp)]
        (reduce-parens-2 (apply conj (vec before) reduced (vec after))))))

(defn update-stack [stack op tkn]
  (cond 
    (and (= (:type tkn) :number) (= op (:value (peek stack))))
    (let [op (:value (first stack))
          arg (:value (second stack))
          newstack (pop (pop stack))
          result (op arg (:value tkn))]
      (conj newstack (token result :number)))
    :else (conj stack tkn)))

(defn eval-exp-2
  ([tokens] (eval-exp-2 (reduce-parens-2 tokens) (first operators) '()))
  ([tokens op stack]
   (let [next-op (first (filter #(not= op %) operators))]
     (if (empty? tokens)
       (if (= 1 (count stack))
         (:value (first stack))
         (eval-exp-2 stack next-op '()))
       (eval-exp-2 (rest tokens) op (update-stack stack op (first tokens)))))))

;; (reduce + (map (comp eval-exp-2 tokenize) (read-input large-input)))
;; 169899524778212
