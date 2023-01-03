(ns joachimheck.aoc.util)
  
(defmacro bench
  " Times the execution of your function,
    discarding the output and returning the elapsed time in seconds
    (you can modify this by changing the divisor from 1e9 (i.e. for milliseconds it would be 1e6."
  ([& forms]
   `(let [start# (System/nanoTime)]
      (list
       ~@forms
       (double (/ (- (System/nanoTime) start#) 1e6))))))
