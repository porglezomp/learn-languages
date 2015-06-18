#lang typed/racket

(provide (all-defined-out))

;; data Zipper a = Zip ![a] ![a]
(struct: (a) Zipper ([before : (Listof a)] [after : (Listof a)]) #:transparent)

;; fromList :: [a] -> Zipper a
(: list->zipper (All (a) (-> (Listof a) (Zipper a))))
(define (list->zipper l) (Zipper '() l))
;; toList :: Zipper a -> [a]
(: zipper->list (All (a) (-> (Zipper a) (Listof a))))
(define (zipper->list z)
  (cond [(empty? z) '()]
        [(begin? z) (Zipper-after z)]
        [else (zipper->list (left z))]))

;; emptyp :: Zipper a -> Bool
(: zipper-empty? (All (a) (-> (Zipper a) Boolean)))
(define (zipper-empty? z) (and (begin? z) (end? z)))
;; endp :: Zipper a -> Bool
(: end? (All (a) (-> (Zipper a) Boolean)))
(define (end? z) (empty? (Zipper-after z)))
;; beginp :: Zipper a -> Bool
(: begin? (All (a) (-> (Zipper a) Boolean)))
(define (begin? z) (empty? (Zipper-before z)))

;; cursor :: Zipper a -> a
(: cursor (All (a) (-> (Zipper a) a)))
(define (cursor z) (car (Zipper-after z)))

;; right :: Zipper a -> Zipper a
(: right (All (a) (-> (Zipper a) (Zipper a))))
(define (right z)
  (if (end? z) z
      (Zipper (cons (car (Zipper-after z)) (Zipper-before z))
              (cdr (Zipper-after z)))))

;; left :: Zipper a -> Zipper a
(: left (All (a) (-> (Zipper a) (Zipper a))))
(define (left z)
  (if (begin? z) z
      (Zipper (cdr (Zipper-before z))
              (cons (car (Zipper-before z)) (Zipper-after z)))))

;; push :: a -> Zipper a -> Zipper a
;; Insert a new value before the cursor
(: push (All (a) (-> a (Zipper a) (Zipper a))))
(define (push i z)
  (Zipper (cons i (Zipper-before z)) (Zipper-after z)))
;; insert :: a -> Zipper a -> Zipper a
;; Insert a new value at the cursor
(: insert (All (a) (-> a (Zipper a) (Zipper a))))
(define (insert i z)
  (Zipper (Zipper-before z) (cons i (Zipper-after z))))
;; pop :: Zipper a -> Zipper a
;; Remove the value before the cursor (if it exists)
(: pop (All (a) (-> (Zipper a) (Zipper a))))
(define (pop z)
  (if (begin? z) z
      (Zipper (cdr (Zipper-before z)) (Zipper-after z))))
;; delete :: Zipper a -> Zipper a
;; Remove the value at the cursor
(: delete (All (a) (-> (Zipper a) (Zipper a))))
(define (delete z)
  (if (end? z) z
      (Zipper (Zipper-before z) (cdr (Zipper-after z)))))
;; replace :: a -> Zipper a -> Zipper a
;; Replace the value at the cursor with a new value
(: replace (All (a) (-> a (Zipper a) (Zipper a))))
(define (replace i z)
  (if (end? z) z
      (Zipper (Zipper-before z)
              (cons i (cdr (Zipper-after z))))))