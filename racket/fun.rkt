#lang typed/racket

(define-type Nat Nonnegative-Integer)

(: .. (-> Integer Integer (Listof Integer)))
(define (.. a b) (if (> a b) '() [cons a (.. (+ a 1) b)]))
(define ^ expt)

(: nfold (All (i) (-> Nat (-> i i) i i)))
(define (nfold n f x) (if (= n 0) x (f (nfold (- n 1) f x))))