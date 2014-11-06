#lang racket

; 1: Multiples of 3 and 5
; Answer: (euler-1 1000)
(define (euler-1 n)
  (apply + (filter (lambda (i) (or (= (modulo i 3) 0)
                                   (= (modulo i 5) 0)))
                   (range n))))

(define (fibo-list n)
  (define (inner n a b l)
    (if (<= n 0)
        (reverse l)
        (inner (- n 1)
               b
               (+ a b)
               (cons a l))))
  (inner n 0 1 '()))

(define (fibo-up-to n)
  (define (inner n a b l)
    (if (> a n)
        (reverse l)
        (inner (- n 1)
               b
               (+ a b)
               (cons a l))))
  (inner n 0 1 '()))

; 2: Even fibonacci numbers
; Answer: (euler-2 4000000)
(define (euler-2 n) 
  (apply + (filter even? (fibo-up-to n))))