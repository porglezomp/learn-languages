(ns tests.core
  (:gen-class))

(defn fac 
  "Computes n!"
  [n]
  (cond
   (<= n 1) 1
   (> n 1) (* n (fac (- n 1)))))

(defn dumb-fibo
  "Naively evaluates the fibonacci sequence with recursion, returning
  `F(n)`."
  [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (dumb-fibo (- n 2))
             (dumb-fibo (- n 1)))))

(defn fibo
  "Evaluates the fibonacci function using 'iterative' recursion,
  returning `F(n)`"
  [n]
  ((fn this [n a b]
     (cond (= n 0) a
           :else (this (- n 1)
                       b
                       (+ a b)))) n 0 1))

(defn rev
  "Returns the reverse of a sequence. Implemented for fun."
  [l]
  ((fn this [in out]
     (cond (empty? in) out
           :else (this (rest in)
                       (conj out (first in))))) l '()))

(defn fibo-range
  "Returns a seq of `n` fibonacci numbers."
  [n]
  ((fn this [n a b l]
     (cond (= n 0) (rev l)
           :else (this (- n 1)
                       b
                       (+ a b)
                       (conj l a)))) n 0 1 '()))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (map fac (range 10)))
  (println (map dumb-fibo (range 10)))
  (println (map fibo (range 10)))
  (println (fibo-range 10)))
