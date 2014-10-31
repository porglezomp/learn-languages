#lang racket
; 99 Lisp problems
; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
; I got 99 problems but a Lisp ain't one

; P01 Find the last box of a list.
(define (my-last list)
  (if (null? (rest list))
      list
      (my-last (rest list))))

; P02 Find the last but one box of a list
(define (my-but-last list)
  (if (<= (length list) 2)
      list
      (my-but-last (rest list))))

; P03 Find the K'th element of a list.
(define (element-at list n)
  (if (<= n 1)
      (first list)
      (element-at (rest list)
                  (- n 1))))

; P04 Find the number of elements of a list
(define (count list)
  (define (inner list n)
    (if (null? list)
        n
        (inner (rest list)
               (+ n 1))))
  (inner list 0))

; P05 Reverse a list
; (For this, my code looked exactly like the example code!)
; (Except not in Portuguese)
(define (reverse list)
  (define (inner source dest)
    (if (null? source)
        dest
        (inner (rest source)
               (cons (first source) dest))))
  (inner list '()))

; P06 Find out whether a list is a palindrome
; (I thought this was cheating, but this is how the example does it.)
(define (palindrome? list)
  (equal? (reverse list) list))

; P07 Flatten a nested list structure
(define (my-flatten list)
  (cond
    [(null? list) null]
    [(list? list)
     (append (my-flatten (first list))
             (my-flatten (rest list)))]
    [else (cons list '())]))

; P08 Eliminate consecutive duplicates of list elements
(define (compress list)
  (cond
    [(null? list) null]
    [(null? (rest list)) list]
    [(equal? (first list) (second list))
     (compress (rest list))]
    [else (cons (first list)
                (compress (rest list)))]))

; P09 Pack consecutive dubplicates into sublists
(define (pack list)
  (if (null? (rest list))
      list
      (if (list? (first list))
          (if (equal? (second list) (first (first list)))
              (pack (cons (cons (second list) (first list))
                          (rest (rest list))))
              (cons (first list) (pack (rest list))))
          (pack (cons (cons (first list) '()) (rest list))))))

; P10 Run-length encoding of a list
(define (encode-fat list)
  (map (lambda (item)
         (cons (length item)
               (cons (first item) '())))
       (pack list)))

; P11 Modified run-length encoding
(define (encode list)
  (map (lambda (item)
         (if (= (length item) 1)
             (first item)
             (cons (length item)
                   (cons (first item) '()))))
       (pack list)))

; P12 Decoding a RLE list
(define (decode list)
  (define (expand list)
    (build-list (first list) (const (second list))))
  (if (null? list)
      null
      (if (list? (first list))
          (append (expand (first list))
                  (decode (rest list)))
          (cons (first list)
                (decode (rest list))))))