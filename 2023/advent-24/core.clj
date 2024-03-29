(ns advent-24.core)

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
;; Ignoring the Z axis, how many hailstones' paths intersect in the test area?
(defn parse-input [input]
  (->> (read-lines input)
       (map #(re-matches #"([\-\d]+), +([\-\d]+), +([\-\d]+) +@ +([\-\d]+), +([\-\d]+), +([\-\d]+)" %))
       (map rest)
       (map #(mapv parse-long %))
       (map (fn [[x y z vx vy vz]] {:x x :y y :z z :vx vx :vy vy :vz vz}))))

(defn pairs [coll]
  (if (empty? coll)
    '()
    (concat (map #(list (first coll) %) (rest coll))
          (pairs (rest coll)))))

(defn find-intersection [stone1 stone2]
  (let [slope1 (/ (:vy stone1) (:vx stone1))
        slope2 (/ (:vy stone2) (:vx stone2))]
    (if (not= slope1 slope2)
      (let [x (/ (+ (- (:y stone2) (:y stone1)) (- (* (:x stone1) slope1) (* (:x stone2) slope2))) (- slope1 slope2))
            y (+ (:y stone1) (* (- x (:x stone1)) slope1))]
        [x y]))))

(defn find-time-for-point [stone [x y]]
  (cond (and (= (:x stone) x) (= (:y stone) y)) :present
        (= (neg? (- x (:x stone))) (neg? (:vx stone))) :future
        :else :past))

(defn stone-to-string [{:keys [x y z vx vy vz]}]
  (format "%d, %d, %d @ %d, %d, %d" x y z vx vy vz))

(defn find-intersections-print [stones min-pos max-pos]
  (let [intersections (map (fn [[a b]] (list a b (find-intersection a b))) (pairs stones))]
    (doseq [[a b intersection] intersections]
      (println "Hailstone A: " (stone-to-string a))
      (println "Hailstone B: " (stone-to-string b))
      (cond (nil? intersection)
            (println "Hailstones' paths are parallel; they never intersect.")
            (and (= :past (find-time-for-point a intersection))
                 (= :past (find-time-for-point b intersection)))
            (println "Hailstones' paths crossed in the past for both hailstones.")
            (= :past (find-time-for-point a intersection))
            (println "Hailstones' paths crossed in the past for hailstone A.")
            (= :past (find-time-for-point b intersection))
            (println "Hailstones' paths crossed in the past for hailstone B.")
            :else
            (let [[x y] intersection]
              (if (and (<= min-pos x max-pos) (<= min-pos y max-pos))
                (printf "Hailstones' paths will cross INSIDE the test area (at x=%.3f, y=%.3f).\n" (double x) (double y))
                (printf "Hailstones' paths will cross outside the test area (at x=%.3f, y=%.3f).\n" (double x) (double y))))))))

(defn find-intersections [stones min-pos max-pos]
  (for [[a b [x y :as intersection]] (map (fn [[a b]] (list a b (find-intersection a b))) (pairs stones))
        :when (and (some? intersection)
                   (= :future (find-time-for-point a intersection))
                   (= :future (find-time-for-point b intersection))
                   (<= min-pos x max-pos)
                   (<= min-pos y max-pos))]
    [x y]))

(defn count-intersections [input min-pos max-pos]
  (count (find-intersections (parse-input input) min-pos max-pos)))

;; (time (count-intersections small-input 7 27))
;; "Elapsed time: 1.2314 msecs"
;; 2

;; (time (count-intersections large-input 200000000000000 400000000000000))
;; "Elapsed time: 758.8846 msecs"
;; 11098



;; Part 2
;; Throw a rock that will hit every hailstone.

;; At time t, stone {:x x0 :y y0 :z z0 :vx vx :vy vy :vz vz} will be at position
;; [(+ x0 (* t vx)) (+ y0 (* t vy)) (+ z0 (* t vz))]


(defn find-intersection-3 [stone1 stone2]
  (let [slope1 (/ (:vy stone1) (:vx stone1))
        slope2 (/ (:vy stone2) (:vx stone2))]
    (if (not= slope1 slope2)
      (let [x (/ (+ (- (:y stone2) (:y stone1)) (- (* (:x stone1) slope1) (* (:x stone2) slope2)))
                 (- slope1 slope2))
            y (+ (:y stone1) (* (- x (:x stone1)) slope1))]
        [x y]))))

;;  {:x 19, :y 13, :z 30, :vx -2, :vy 1, :vz -2}
;;  {:x 18, :y 19, :z 22, :vx -1, :vy -1, :vz -2}
;;  {:x 20, :y 25, :z 34, :vx -2, :vy -2, :vz -4}
;;  {:x 12, :y 31, :z 28, :vx -1, :vy -2, :vz -1}
;;  {:x 20, :y 19, :z 15, :vx 1, :vy -5, :vz -3}

;; -2t + t -2t = 62
;; 19 + -2t

;; y = mx + b
;; y = (-1/2)x + 22.5

;; x = 19 + -2t
;; y = 13 + t
;; z = 30 -2t

;; x - 19 = -2t
;; y - 13 = t
;; z - 30 = -2t

;; 19/2 - x/2 = t
;; y - 13 = t
;; 15 - z/2 = t

;; 19/2 - x/2 = y - 13
;; 45/2 - x/2 - y = 0

;; 45/2 - x/2 - y = 15 - z/2
;; 45/2 - 30/2 - x/2 - y + z/2 = 0
;; 15/2 - x/2 - y + z/2 = 0
;; 15 - x - 2y + z = 0
;; - x - 2y + z = - 15


;; {:x 19, :y 13, :vx -2, :vy 1}
;; {:x 18, :y 19, :vx -1, :vy -1}

;; x = (+ 19 (* -2 t)) = (+ 18 (* -1 s))
;; y = (+ 13 (* 1 t)) = (+ 19 (* -1 s))

;; (+ 19 (* -2 t)) = (+ 18 (* -1 s))
;; (* -2 t) = (+ 18 (* -1 s) -19)
;; (* -2 t) = (+ -1 (* -1 s))
;; (- (* 2 t)) = (- (+ s 1))
;; (* 2 t) = (+ s 1)
;; t = (/ (+ s 1) 2)

;; (+ 13 t) = (+ 19 (- s))
;; t = (+ 19 (- s) -13)
;; t = (+ 6 (- s))

;; (/ (+ s 1) 2) = (+ 6 (- s))
;; (+ s 1) = (+ 12 (* -2 s))
;; (* 3 s) = 11
;; s = (/ 11 3)

;; x(s) = (+ 18 - (/ 11 3)) = (/ (- 54 11) 3) = (/ 43 3) =~ 14.333
;; y(s) = (+ 19 (* -1 (/ 11 3))) = (- (/ 57 3) (/ 11 3)) = (/ 46 3) =~ 15.333

;; OK, I was able to manually take the parameterized equations for the two lines,
;; solve for t in terms of s, then set x(s) = x(t) and solve for s, then backfill
;; to get the intersection point. Next I need to write code to do this, and then
;; it needs to be expanded to three (four?) dimensions to solve the problem.


;; (let [parsed (parse-input small-input)
;;                       with-t (map #(assoc % :t 0 :vt 1) parsed)
;;                       inputs (map list [\A \B \C \D \E] [\a \b \c \d \e] with-t)
;;                       format-eq (fn [[line-name param coefficients]]
;;                                   (for [[axis v axis-name] [[:x :vx "x"] [:y :vy "y"] [:z :vz "z"] [:t :vt "t"]]]
;;                                     (format "%s%s(%s) = %d%s + %d" line-name axis-name param (get coefficients v) param (get coefficients axis))))]
;;                   (mapcat format-eq inputs))

(defn vector-add [v1 v2]
  (mapv + v1 v2)
  ;; (mapv int (map + v1 v2))
  )

(defn vector-multiply [v s]
  (mapv #(* s %) v)
  ;; (mapv int (map #(* s %) v))
  )

(defn gaussian-eliminate [matrix]
  (loop [matrix matrix
         r 0]
    (if (= r (count matrix))
      matrix
      (let [row (get matrix r)
            at-pivot (get row r)
            row-with-pivot (vector-multiply row (/ 1 at-pivot))
            new-matrix (vec (for [fr (range (count matrix))
                                  :let [f-row (get matrix fr)
                                        at-col (get f-row r)]]
                              (if (= fr r)
                                row-with-pivot
                                (vector-add f-row (vector-multiply row-with-pivot (- at-col))))))]
        (recur new-matrix (inc r))))))

(deftest test-gaussian-eliminate
  (is (= [[1 0 7] [0 1 4]] (gaussian-eliminate [[3 -3 9] [2 -3 2]])))
  (is (= [[1 0 0 -17] [0 1 0 -3] [0 0 1 2]] (gaussian-eliminate [[2 -8 6 2] [-3 16 -5 -7] [-3 15 -9 -12]]))))

;; A {:x 19, :y 13, :z 30, :t 0 :vx -2, :vy 1, :vz -2 :vt 1}
;; B {:x 18, :y 19, :z 22, :t 0 :vx -1, :vy -1, :vz -2 :vt 1}
;; C {:x 20, :y 25, :z 34, :t 0 :vx -2, :vy -2, :vz -4 :vt 1}
;; D {:x 12, :y 31, :z 28, :t 0 :vx -1, :vy -2, :vz -1 :vt 1}
;; E {:x 20, :y 19, :z 15, :t 0 :vx 1, :vy -5, :vz -3 :vt 1}

;; How to compute the intersection of lines A and B, ignoring the z component (from part 1)?

;; Ax = -2t + 19
;; Ay =  1t + 13
;; Az = -2t + 30
;; Bx = -1t + 18
;; By = -1t + 19
;; Bz = -2t + 22
;; Lx =  lt +  i
;; Ly =  mt +  j
;; Lz =  nt +  k

;; 16 variables: ax ay az bx by bz lx ly lz t l m n i j k, 9 equations - too many variables!

(defn create-matrix [params]
  (let [var-count (+ (* 3 (count params)) 2)]
    (reduce (fn [acc [i {:keys [x y z vx vy vz]}]]
              (-> acc
                  (assoc-in [(+ 0 (* i 3)) (+ 0 (* i 3))] 1)
                  (assoc-in [(+ 0 (* i 3)) (- var-count 2)] (- vx))
                  (assoc-in [(+ 0 (* i 3)) (dec var-count)] x)
                  (assoc-in [(+ 1 (* i 3)) (+ 1 (* i 3))] 1)
                  (assoc-in [(+ 1 (* i 3)) (- var-count 2)] (- vy))
                  (assoc-in [(+ 1 (* i 3)) (dec var-count)] y)
                  (assoc-in [(+ 2 (* i 3)) (+ 2 (* i 3))] 1)
                  (assoc-in [(+ 2 (* i 3)) (- var-count 2)] (- vz))
                  (assoc-in [(+ 2 (* i 3)) (dec var-count)] z)))
            (vec (repeat (* 3 (count params)) (vec (repeat var-count 0))))
            (map-indexed list params))))

;; Ax = -2p + 19
;; Ay =  1p + 13
;; Az = -2p + 30
;; At = p
;; Bx = -1p + 18
;; By = -1p + 19
;; Bz = -2p + 22
;; Bt = p
(defn create-matrix-2 [params]
  (let [var-count (+ (* 4 (count params)) 2)]
    (reduce (fn [acc [i {:keys [x y z vx vy vz]}]]
              (-> acc
                  (assoc-in [(+ 0 (* i 4)) (+ 0 (* i 4))] 1)
                  (assoc-in [(+ 0 (* i 4)) (- var-count 2)] (- vx))
                  (assoc-in [(+ 0 (* i 4)) (dec var-count)] x)
                  (assoc-in [(+ 1 (* i 4)) (+ 1 (* i 4))] 1)
                  (assoc-in [(+ 1 (* i 4)) (- var-count 2)] (- vy))
                  (assoc-in [(+ 1 (* i 4)) (dec var-count)] y)
                  (assoc-in [(+ 2 (* i 4)) (+ 2 (* i 4))] 1)
                  (assoc-in [(+ 2 (* i 4)) (- var-count 2)] (- vz))
                  (assoc-in [(+ 2 (* i 4)) (dec var-count)] z)
                  (assoc-in [(+ 3 (* i 4)) (+ 3 (* i 4))] 1)
                  (assoc-in [(+ 3 (* i 4)) (- var-count 2)] -1)
                  (assoc-in [(+ 3 (* i 4)) (dec var-count)] 0)))
            (vec (repeat (* 4 (count params)) (vec (repeat var-count 0))))
            (map-indexed list params))))


;; I finally tried a different tack, brute forcing the solution.
;; I'll check every tuple [a b c t1 t2 t3] of three lines and three times with equal intervals
;; between them. If the three points are all on the line, I've found the solution. This works
;; for the example input but will be extremely slow for the real input. Or maybe I'll get lucky.
(defn compute-result [input]
  (let [data (parse-input input)
        pairs (pairs data)
        max-t 10]
    (first
     (for [[a b] pairs
           t1 (range max-t)
           t2 (range (inc t1) max-t)
           :let [[ax ay az :as pa] [(+ (:x a) (* t1 (:vx a)))
                                    (+ (:y a) (* t1 (:vy a)))
                                    (+ (:z a) (* t1 (:vz a)))]
                 [bx by bz :as pb] [(+ (:x b) (* t2 (:vx b)))
                                    (+ (:y b) (* t2 (:vy b)))
                                    (+ (:z b) (* t2 (:vz b)))]
                 t3 (+ t2 (- t2 t1))]
           c data
           :when (and (not= c a) (not= c b))
           :let [[cx cy cz :as pc] [(+ (:x c) (* t3 (:vx c)))
                                    (+ (:y c) (* t3 (:vy c)))
                                    (+ (:z c) (* t3 (:vz c)))]
                 l1 (- bx ax)
                 l2 (- cx bx)
                 m1 (- by ay)
                 m2 (- cy by)
                 n1 (- bz az)
                 n2 (- cz bz)]
           :when (and (= l1 l2) (= m1 m2) (= n1 n2))]
       (let [x (- ax (* (/ (- bx ax) (- t2 t1)) t1))
             y (- ay (* (/ (- by ay) (- t2 t1)) t1))
             z (- az (* (/ (- bz az) (- t2 t1)) t1))]
        [:x x :y y :z z :vx l1 :vy m1 :vz n1
         ;;:pa pa :t1 t1 :pb pb :t2 t2
         :sum (+ x y z)])))))

(defn compute-result-2 [input max-t]
  (let [data (parse-input input)
        [a b c] (take 3 data)]
    (first
     (for [t1 (range 1 (- max-t 2))
           t2 (range (inc t1) (dec max-t))
           t3 (range (inc t2) max-t)
           :let [[ax ay az :as pa] [(+ (:x a) (* t1 (:vx a)))
                                    (+ (:y a) (* t1 (:vy a)))
                                    (+ (:z a) (* t1 (:vz a)))]
                 [bx by bz :as pb] [(+ (:x b) (* t2 (:vx b)))
                                    (+ (:y b) (* t2 (:vy b)))
                                    (+ (:z b) (* t2 (:vz b)))]
                 [cx cy cz :as pc] [(+ (:x c) (* t3 (:vx c)))
                                    (+ (:y c) (* t3 (:vy c)))
                                    (+ (:z c) (* t3 (:vz c)))]
                 l1 (/ (- bx ax) (- t2 t1))
                 m1 (/ (- by ay) (- t2 t1))
                 n1 (/ (- bz az) (- t2 t1))]
           ;; :when (and (int? l1) (int? m1) (int? n1))
           :let [l2 (/ (- cx bx) (- t3 t2))
                 m2 (/ (- cy by) (- t3 t2))
                 n2 (/ (- cz bz) (- t3 t2))]
           :when (and (= l1 l2) (= m1 m2) (= n1 n2))]
       (let [x (- ax (* l1 t1))
             y (- ay (* m1 t1))
             z (- az (* n1 t1))]
        [:x x :y y :z z :vx l1 :vy m1 :vz n1
         ;;:pa pa :t1 t1 :pb pb :t2 t2
         :sum (+ x y z)])))))

(defn replace-vars [s v-map]
  (reduce (fn [acc k] (str/replace acc (re-pattern k) (get v-map k)))
          s
          (keys v-map)))

;; (avx - bvx)TaTb + (cvx - avx)TaTc + (bvx - cvx)TbTc + (cx - bx)Ta + (ax - cx)Tb + (bx - ax)Tc = 0
;; (avy - bvy)TaTb + (cvy - avy)TaTc + (bvy - cvy)TbTc + (cy - by)Ta + (ay - cy)Tb + (by - ay)Tc = 0
;; (avz - bvz)TaTb + (cvz - avz)TaTc + (bvz - cvz)TbTc + (cz - bz)Ta + (az - cz)Tb + (bz - az)Tc = 0
(defn format-equations [input var-set]
  (let [data (parse-input input)
        [{ax :x ay :y az :z avx :vx avy :vy avz :vz}
         {bx :x by :y bz :z bvx :vx bvy :vy bvz :vz}
         {cx :x cy :y cz :z cvx :vx cvy :vy cvz :vz}] (take 3 data)
        v-maps [{"Ta" "Ta" "Tb" "Tb" "Tc" "Tc"}
                 {"Ta" "t" "Tb" "u" "Tc" "v"}]]
    [(format "ax %d ay %d az %d avx %d avy %d avz %d" ax ay az avx avy avz)
     (format "bx %d by %d bz %d bvx %d bvy %d bvz %d" bx by bz bvx bvy bvz)
     (format "cx %d cy %d cz %d cvx %d cvy %d cvz %d" cx cy cz cvx cvy cvz)
     (newline)
     (replace-vars (format "%dTaTb + %dTaTc + %dTbTc + %dTa + %dTb + %dTc = 0"
                           (- avx bvx) (- cvx avx) (- bvx cvx) (- cx bx) (- ax cx) (- bx ax))
                   (get v-maps var-set))
     (replace-vars (format "%dTaTb + %dTaTc + %dTbTc + %dTa + %dTb + %dTc = 0"
                           (- avy bvy) (- cvy avy) (- bvy cvy) (- cy by) (- ay cy) (- by ay))
                   (get v-maps var-set))
     (replace-vars (format "%dTaTb + %dTaTc + %dTbTc + %dTa + %dTb + %dTc = 0"
                           (- avz bvz) (- cvz avz) (- bvz cvz) (- cz bz) (- az cz) (- bz az))
                   (get v-maps var-set))]))

(defn compute-start-position [input t u]
  (let [data (parse-input input)
        [{ax :x ay :y az :z avx :vx avy :vy avz :vz}
         {bx :x by :y bz :z bvx :vx bvy :vy bvz :vz}
         {cx :x cy :y cz :z cvx :vx cvy :vy cvz :vz}] (take 3 data)
        [pax pay paz] [(+ ax (* avx t)) (+ ay (* avy t)) (+ az (* avz t))]
        [pbx pby pbz] [(+ bx (* bvx u)) (+ by (* bvy u)) (+ bz (* bvz u))]
        l (/ (- pbx pax) (- u t))
        m (/ (- pby pay) (- u t))
        n (/ (- pbz paz) (- u t))
        [startx starty startz :as start-pos] [(- pax (* l t)) (- pay (* m t)) (- paz (* n t))]]
    {:start-pos start-pos
     :result (apply + start-pos)
     ;; :pa [pax pay paz]
     ;; :pb [pbx pby pbz]
     ;; :slope [l m n]
      }))


;; (format-equations small-input 1)

;; ["ax 19 ay 13 az 30 avx -2 avy 1 avz -2"
;;  "bx 18 by 19 bz 22 bvx -1 bvy -1 bvz -2"
;;  "cx 20 cy 25 cz 34 cvx -2 cvy -2 cvz -4"
;;  nil
;;  "-1tu + 0tv + 1uv + 2t + -1u + -1v = 0"
;;  "2tu + -3tv + 1uv + 6t + -12u + 6v = 0"
;;  "0tu + -2tv + 2uv + 12t + -4u + -8v = 0"]

;; From Wolfram Alpha:
;; t = 5 and u = 3 and v = 4

;; (compute-start-position small-input 5 3)
;; {:start-pos [24 13 10], :result 47}


;; (format-equations large-input 1)
;; ["ax 156689809620606 ay 243565579389165 az 455137247320393 avx -26 avy 48 avz -140"
;;  "bx 106355761063908 by 459832650718033 bz 351953299411025 bvx 73 bvy -206 bvz -52"
;;  "cx 271915251832336 cy 487490927073225 cz 398003502953444 cvx 31 cvy -414 cvz -304"
;;  nil
;;  "-99tu + 57tv + 42uv + 165559490768428t + -115225442211730u + -50334048556698v = 0"
;;  "254tu + -462tv + 208uv + 27658276355192t + -243925347684060u + 216267071328868v = 0"
;;  "-88tu + -164tv + 252uv + 46050203542419t + 57133744366949u + -103183947909368v = 0"]

;; From Wolfram Alpha:
;; t = 931974028142 and u = 829702369046 and v = 474506740599

;; (compute-start-position large-input 931974028142 829702369046)
;; {:start-pos [446533732372768 293892176908833 180204909018503], :result 920630818300104}
