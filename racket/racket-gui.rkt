#lang racket

(require racket/gui/base)
(require zipper)

;(define-type Char Positive-Byte)

;(: lines (Zipper (Zipper Char)))
(define lines (Zipper '[] '[]))

(define (loop)
  (loop))

(define frame (new frame% [label "Example"]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (match (send event get-key-code)
        ['up (print 'up)]
        ['down (print 'down)]
        ['left (print 'left)]
        ['right (print 'right)]
        [else (print 'other)]))
    (super-new)))

(new my-canvas% [parent frame])
(send frame show #t)