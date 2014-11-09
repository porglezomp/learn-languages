#lang racket

(define (replace-at-n list n val)
  (if (= n 0)
      (cons val (rest list))
      (cons (first list) (replace-at-n (rest list) (- n 1) val))))

(define (nth list n)
  (if (= n 0)
      (if (null? list)
          null
          (first list))
      (if (null? (rest list))
          null
          (nth (rest list) (- n 1)))))

;; Strange predicates

;; none-equal? takes a value, and ensures that none
;; of the following arguments are equal to it
(define (none-equal? a . items)
  (if (null? items)
      #t
      (and
       (not (equal? a (first items)))
       (apply none-equal? (cons a (rest items))))))

;; all-equal? takes many arguments, and ensures that
;; they are all equal
(define (all-equal? . items)
  (if (null? items)
      #f
      (if (null? (rest items))
          (first items)
          (and
           (equal? (first items) (second items))
           (apply all-equal? (rest items))))))

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

(define (tie? board)
  (and (not (check-win board))
       (apply none-equal? (cons null board))))

(define (done? board)
  (or (tie? board) (check-win board)))

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
  (newline)
  (let [(result (check-win board))]
    (cond [result (printf "~S wins!" result)]
          [(tie? board) (printf "Tie!")]
          [else
           (let [(point (if (equal? turn 'X) 
                            (read)
                            (+ (cdr (minimax board turn 6)) 1)))]
             (if (not (number? point))
                 (begin (write "Please specify a number 1-9")
                        (newline)
                        (mainloop board turn))
                 (if (can-place-at? (- point 1) board)
                     (mainloop (replace-at-n board (- point 1) turn)
                               (if (equal? turn 'X) 'O 'X))
                     (begin (write "You can't place there!")
                            (newline)
                            (mainloop board turn)))))])))

(define (sum . items)
  (apply + items))

(define (score board)
  ;; Count the number of Xs, Os, and Nulls in the list
  (define (count-x-o l)
    (define (worker x o n l)
      (if (null? l)
          (list x o n)
          (cond [(equal? (first l) 'X)
                 (worker (+ x 1) o n (rest l))]
                [(equal? (first l) 'O)
                 (worker x (+ o 1) n (rest l))]
                [else (worker x o (+ n 1) (rest l))])))
    (worker 0 0 0 l))
  (define (count cnt lists)
    (apply + (for/list [(list lists)]
               (let* [(xon (count-x-o list))
                      (x (first xon))
                      (o (second xon))
                      (n (third xon))]
                 (cond [(and (= x cnt) (= o 0) (= n (- 3 cnt))) 1]
                       [(and (= o cnt) (= x 0) (= n (- 3 cnt))) -1]
                       [else 0])))))
  (define (mapper indices)
    (for/list [(ind indices)]
      (map (lambda (n) (nth board n)) ind)))
  (let [(lists
         (mapper
          (append
           ;; Horizontal
           (for/list [(start (range 0 9 3))]
             (range start (+ start 3) 1))
           ;; Vertical
           (for/list [(start (range 3))]
             (range start (+ start 7) 3))
           ;; Diagonal from top right
           (list (range 0 9 4))
           ;; Diagonal from top left
           (list (range 2 7 2)))))]
    (sum 
     (* 100 (count 3 lists))
     (* 10 (count 2 lists))
     (* 1 (count 1 lists)))))
    
(define (minimax board player depth)
  (define (cmax list)
    (if (null? (rest list))
        (first list)
        (let [(rest-max (cmax (rest list)))]
          (if (>= (car (first list)) (car rest-max))
              (first list)
              rest-max))))
  (define (cmin list)
    (if (null? (rest list))
        (first list)
        (let [(rest-min (cmin (rest list)))]
          (if (<= (car (first list)) (car rest-min))
              (first list)
              rest-min))))
  (if (or (done? board) (= depth 0))
      (score board)
      (let [(next-player (if (equal? player 'X) 'O 'X))
            (optimize (if (equal? player 'X) cmax cmin))]
          (optimize (for/list [(index (range 9))
                               #:when (can-place-at? index board)]
                      (let [(next (minimax (replace-at-n board index player) next-player (- depth 1)))]
                        (if (pair? next)
                            (cons (car next) index)
                            (cons next index))))))))

(define board (build-list 9 (const null)))
(define (start)
  (mainloop board 'X))