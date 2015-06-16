#lang typed/racket

(require racket/dict)

(define-type N Real)
(define-type Vec (Listof N))
(define-type Tag-Vec (Pairof Symbol Vec))
(define-type Nat Nonnegative-Integer)

(define (map2-factory [op : (-> N N N)]) : (-> Vec Vec Vec) 
  (define (inner [v1 : Vec] [v2 : Vec]) : Vec
    (if (null? v1) '()                     ;;; If v1 isn't empty, then keep applying the operation
        (cons (op (car v1) (car v2))       ;;; This isn't tail recursive, sadly, hopefully people
              (inner (cdr v1) (cdr v2))))) ;;; don't make vectors with more elements than there are
  inner)                                   ;;; stack frames

(define -- (map2-factory -)) ; Vector subtraction
(define ** (map2-factory *)) ; Vector multiplication

(define (mag2 [v : Vec]) : Nonnegative-Real (abs (apply + (** v v))))
(define (mag [v : Vec]) : Nonnegative-Real (sqrt (mag2 v)))
(define (distance [v1 : Vec] [v2 : Vec]) : Nonnegative-Real (mag (-- v1 v2)))

;; k-nearest produces a list of the `k` elements closest to the 
(: k-nearest (-> Positive-Integer (Listof Tag-Vec) Vec (Listof Tag-Vec)))
(define (k-nearest k items point)
  ;;
  (: replace-item (-> Tag-Vec (Listof Tag-Vec) (Listof Tag-Vec)))
  (define (replace-item item items)
    (if (null? items) '()
        (if (< (distance point (cdr item)) (distance point (cdr (car items))))
            ;; If you are closer, pass the item you're displacing further along to keep only the
            ;; closest items present
            (cons item (replace-item (car items) (cdr items)))
            (cons (car items) (replace-item item (cdr items))))))
  ;;
  (: helper (-> (Listof Tag-Vec) Vec (Listof Tag-Vec) (Listof Tag-Vec)))
  (define (helper items point cache)
    (if (null? items) cache ; If we're out of items to search, the k nearest are in the cache
        (if (< (length cache) k) ; If we're not out of items, keep looking
            ;; The cache isn't full, therefore this is one of the closest, just add it
            (helper (cdr items) point (cons (car items) cache))
            ;; If we're out of cache space, replace an item if this one is closer than it
            (helper (cdr items) point (replace-item (car items) cache)))))
  ;; Finally compute the k nearest using out helper function
  (helper items point '()))

(: knn (-> Positive-Integer (Listof Tag-Vec) (Listof Vec) (Listof Tag-Vec)))
(define (knn k prior items)
  (: increment-count (-> (HashTable Symbol Nat) Symbol (HashTable Symbol Nat)))
  (define (increment-count t i) (hash-update t i (lambda ([x : Nat]) (+ x 1)) (const 1)))
  ;;
  (: select-class (-> (Listof Tag-Vec) Vec Tag-Vec))
  (define (select-class tagged point)   
    (: helper (-> (HashTable Symbol Nat) (Listof Tag-Vec) Symbol))
    (define (helper tally tagged)
      (if (null? tagged)
          ;; Select the most common class. Explicitly instantiate sort because it can't be inferred
          (car (foldl (lambda ([a : (Pairof Symbol Nat)] [b : (Pairof Symbol Nat)]) (> (cdr a) (cdr b)) a b)
                       (car (hash->list tally)) (cdr (hash->list tally))))
          ;(caar ((inst sort (Pairof Symbol Nat) Nat) (hash->list tally) < #:key cdr))
          (helper (increment-count tally (caar tagged)) (cdr tagged))))
    (cons (helper #hash() tagged) point))
  ;; 
  (if (null? items) prior
      (knn k (cons (select-class (k-nearest k prior (car items)) (car items)) prior)
           (cdr items))))