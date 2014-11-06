#lang racket

;; Vec helpers
(define (vadd v1 v2)
  (map + v1 v2))
(define (vneg v)
  (map - v))
(define vinv vneg)
(define (vsub v1 v2)
  (map - v1 v2))
(define (vmul v s)
  (for/list [(c v)] (* c s)))
(define (vlen2 v)
  (apply + (map square v)))
(define (vlen v)
  (sqrt (vlen2 v)))
(define (vsum vs)
  (foldl1 vadd vs))
(define (foldl1 f list)
  (foldl f (first list) (rest list)))

;; Other helpers
(define (nth list n)
  (if (= n 0)
      (first list)
      (nth (rest list)
           (- n 1))))
(define (square n)
  (* n n))

;; Output vectors
(define (output buf)
  (display (for/list [(particle buf)]
           (second particle)))
  (newline))

;; Produce the sets needed for computing gravity
(define (except list n)
  (append (take list n)
          (drop list (+ n 1))))
(define (sets list)
  (for/list [(n (range (length list)))]
    (cons (nth list n) (except list n))))

;; Define variables for sim
(define G 6.674e-11)
(define particles
  (let [(r (make-pseudo-random-generator))
        (mass 1)]
    (for/list [(i (range 200))]
      (list mass
            (list
             (- (* (random r) 2) 1)
             (- (* (random r) 2) 1))))))
(define dt 60)

;; Calculate the force of gravity between two particles
(define (gravity p1 p2)
  (let* [(delta (vsub (second p1)
                      (second p2)))
         (magnitude 
          (* (first p2) (/ (vlen2 delta)) G))
         (force (vmul delta magnitude))]
    (vneg force)))

;; Compute one step of Verlet integration, and return the new state
(define (step previous current)
  (for/list [(states (sets current))
             (prev previous)]
    (let* [(particle (first states))
           (others (rest states))
           (mass (first particle))
           (position (second particle))
           (apply-gravity (lambda (p) (gravity particle p)))
           (accel (vsum (map apply-gravity others)))]
    (list mass (vadd (vsub (vmul position 2) (second prev))
                     (vmul accel (square dt)))))))

;; Do steps recursively
(define (do-step prev cur n)
  (output cur)
  (flush-output)
  (sleep 0.02)
  (when (positive? n)
    (do-step cur
             (step prev cur)
             (- n 1))))

;; Main
(display "2d")
(newline)
(flush-output)
(do-step particles particles 1000)
(display "done")
(newline)
(flush-output)