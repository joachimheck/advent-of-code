(ns advent-20.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.test :refer :all])

(def small-input "small-input.txt")
(def small-input-2 "small-input-2.txt")
(def large-input "large-input.txt")
(def test-input "test-input.txt")

(defn- read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What's the product of the numbers of low and high pulses sent?
(defn parse-module [s-name s-destinations]
  (let [type (cond (str/starts-with? s-name "%") :flip-flop
                   (str/starts-with? s-name "&") :conjunction
                   (= s-name "button") :button
                   (= s-name "broadcaster") :broadcaster
                   :else :none)
        name (if (some #{type} #{:flip-flop :conjunction}) (subs s-name 1) s-name)
        module {:name name :type type :destinations (str/split s-destinations #", ")}
        module (if (= type :flip-flop) (assoc module :state :off) module)
        module (if (= type :conjunction) (assoc module :memory {}) module)]
    [name module]))

(defn setup-memory [modules]
  (let [destinations (into {} (map (fn [[n m]] [n (set (:destinations m))]) modules))
        inverted (reduce (fn [a [k v]] (assoc a k (conj (get a k #{}) v))) {} (for [[k s] destinations v s] [v k]))]
    (map (fn [[n m]]
           [n
            (if (= (:type m) :conjunction)
              (assoc m :memory (into {} (mapv (fn [i] [i :low]) (get inverted (:name m)))))
              m)])
         modules)))

(defn parse-input [input]
  (->> (read-lines input)
       (concat (list "button -> broadcaster"))
       (map #(rest (re-matches #"(.+) -> (.+)" %)))
       (mapv (fn [[k v]] (parse-module k v)))
       (setup-memory)
       (into {})))

(defn pulse-to-string [p]
  (str (:from p) " -" (if (= :low (:p-type p)) "low" "high") "-> " (:to p)))

(defn handle-pulse [{:keys [from to p-type] :as pulse} modules]
  (let [module (get modules to)
        {:keys [name type destinations state memory] :as new-module}
        (cond (= (:type module) :flip-flop) (if (= p-type :low) (update module :state #(if (= % :off) :on :off)) module)
              (= (:type module) :conjunction) (assoc-in module [:memory from] p-type)
              :else module)
        new-pulses (remove nil? (map (fn [destination]
                                       (case type
                                         :button {:from name :to destination :p-type :low}
                                         :broadcaster {:from name :to destination :p-type p-type}
                                         :flip-flop (if (= p-type :low)
                                                      {:from name :to destination :p-type (if (= state :on) :high :low)})
                                         :conjunction {:from name :to destination :p-type
                                                       (if (every? #{:high} (vals memory)) :low :high)}))
                                     destinations))]
    {:module new-module :new-pulses new-pulses}))

(defn process [config]
  (loop [pulses [{:from "button" :p-type :low :to "broadcaster"}]
         modules config
         sent-pulses {:low 0 :high 0}
         rx-low-pulse-count 0]
    (if (empty? pulses)
      {:modules modules :sent-pulses sent-pulses :rx-low-pulse-count rx-low-pulse-count}
      (let [current (first pulses)
            {:keys [module new-pulses]} (handle-pulse current modules)]
        (recur (apply conj (vec (rest pulses)) new-pulses)
               (assoc modules (:name module) module)
               (update sent-pulses (:p-type current) inc)
               (+ rx-low-pulse-count (count (filter (fn [{:keys [_ to p-type]}] (and (= to "rx") (= p-type :low))) new-pulses))))))))

(defn multi-process [input n]
  (loop [config (parse-input input)
         n n
         sent {:low 0 :high 0}]
    (if (= n 0)
      (* (:low sent) (:high sent))
      (let [{:keys [modules sent-pulses]} (process config)]
        (recur modules
               (dec n)
               (-> sent
                   (update :low #(+ % (:low sent-pulses)))
                   (update :high #(+ % (:high sent-pulses)))))))))

;; (multi-process small-input 1000)
;; 32000000

;; (multi-process small-input-2 1000)
;; 11687500

;; (multi-process large-input 1000)
;; 866435264



;; Part 2
;; How many button presses until a single low pulse is sent to module "rx"?
(defn format-node-state [modules nodes]
  (str/join " "
            (for [s nodes]
              (str/join
               (for [n s
                     :let [module (get modules n)]]
                 (if (= (:type module) :flip-flop)
                   (format "%d" (case (:state module) :on 1 :off 0))
                   (format "|%d/%d|" (count (filter #{:high} (vals (:memory module)))) (count (:memory module)))))))))

(defn process-until-rx
  ([input max-n] (process-until-rx input max-n nil))
  ([input max-n debug-step]
   (println "arguments" [input max-n debug-step])
   (loop [config (parse-input input)
          n 1]
     (if (>= n max-n)
       {:failure n}
       (let [{:keys [modules sent-pulses rx-low-pulse-count]} (process config)
             nodes
             (if (= input test-input)
               [["aa" "ab" "ac" "ad" "ax" "ay"] ["ba" "bb" "bc" "bd" "bx" "by"] ["ww"]]
               [["kx" "sg" "tr" "br" "jq" "mm" "vh" "xx" "cz" "lc" "vn" "cc" "vv" "zv"]
                ["mg" "hl" "qb" "cd" "ps" "xc" "kz" "mc" "xv" "tx" "rv" "tp" "jc" "lk"]
                ["nt" "cx" "qh" "ln" "mq" "hs" "sl" "zl" "nj" "pt" "gt" "hg" "xq" "sp"]
                ["rc" "rm" "dm" "bl" "jt" "vp" "fd" "td" "hx" "gf" "dc" "nv" "dv" "xt"]
                ["dg"]])]
         (if (or (nil? debug-step) (< (- debug-step 5) n (+ debug-step 5)))
              (printf "step %3d: %s\n" n (format-node-state modules nodes)))
            (if (>= rx-low-pulse-count 1)
              [:success n rx-low-pulse-count]
              (recur modules (inc n))))))))

;; Build a graph input appropriate for https://csacademy.com/app/graph_editor/
;; Except the ::s have to be converted into newlines.
(defn build-graph [input]
  (let [config (parse-input input)
        type-string (fn [name]
                      (case (:type (get config name))
                        :flip-flop "%"
                        :conjunction "&"
                        ""))]
    (->> config
         (vals)
         (map (fn [{:keys [name destinations]}]
                (for [d destinations]
                  (format "%s%s %s%s" (type-string name) name (type-string d) d))))
         (apply concat)
         (str/join "::"))))


;; 4095
;; 4096 - maximum 12-digit binary number is 4095, then it looks like there's one more node.
;; 228979973476800
;; ---> answer <---

;; Is there a bug? This looks like it's just counting, but it resets to zero at step 4052,
;; not, as expected, at step 4096.

;; From the _directional_ (!) graph in graph-20.png:
;; node vv triggers at 2r111111010010 = 4050
;; node jc triggers at 2r111011101110 = 3822
;; node xq triggers at 2r111101011000 = 3928
;; node dv triggers at 2r111010110110 = 3766


;; (apply * (map inc [4050 3822 3928 3766]))
;; 229215609826339
