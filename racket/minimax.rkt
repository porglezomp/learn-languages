#lang racket

(define board (build-list 9 (const null)))

(define (nth list n)
  (if (= n 0)
      (if (null? list)
          null
          (first list))
      (if (null? (rest list))
          null
          (nth (rest list) (- n 1)))))

(define (none-equal? a . items)
  (if (null? items)
      #t
      (and
       (not (equal? a (first items)))
       (apply none-equal? (cons a (rest items))))))
            

(define (all-equal? . items)
  (if (null? items)
      #f
      (if (null? (rest items))
          (first items)
          (and
           (equal? (first items) (second items))
           (apply all-equal? (rest items))))))
           

(define (replace-at-n list n val)
  (if (= n 0)
      (cons val (rest list))
      (cons (first list) (replace-at-n (rest list) (- n 1) val))))

(define (first-true list)
  (if (null? list)
      #f
      (if (first list)
          (first list)
          (first-true (rest list)))))

(define (null-to-false list)
  (if (null? list)
      null
      (if (null? (first list))
          (cons #f (null-to-false (rest list)))
          (cons (first list) (null-to-false (rest list))))))
      

(define (check-win board)
  (define (item-function positions)
    (apply all-equal?
           (map (lambda (n) (nth board n))
                positions)))
  (first-true
   (flatten
    (list
     (null-to-false
      (for/list [(start (range 3))]
        (item-function
         (range start (+ start 7) 3))))
     (null-to-false
      (for/list [(start (range 0 9 3))]
        (item-function
         (range start (+ start 3)))))
     (null-to-false
      (list (item-function
             (range 0 9 4))))
     (null-to-false
      (list (item-function
             (range 2 7 2))))))))

(define (can-place-at? n board)
  (null? (nth board n)))

(define (print-board board)
  (for [(row (range 3))]
    (for [(col (range 3))]
      (let* [(index (+ col (* 3 row)))
             (item (nth board index))]
        (if (null? item)
            (write (+ index 1))
            (write item))
        (write-string " ")))
    (newline)))
  
(define (mainloop board turn)
  (print-board board)
  (let [(result (check-win board))]
    (if result
        (printf "~S wins!" result)
        (let [(point (read))]
          (if (not (number? point))
              (begin (write "Please specify a number 1-9")
                     (mainloop board turn))
              (if (can-place-at? (- point 1) board)
                  (mainloop (replace-at-n board (- point 1) turn)
                            (if (equal? turn 'X) 'O 'X))
                  (begin (write "You can't place there!")
                         (mainloop board turn))))))))

(define (start)
  (mainloop board 'X))