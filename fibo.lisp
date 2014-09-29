(defun fibo (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (T (+ (fibo (- n 1)) (fibo (- n 2))))))

(defun fibo0 (n p1 p2)
  (cond ((< n 0) 0)
        ((= n 0) p1)
        (T (fibo0 (- n 1) p2 (+ p1 p2)))))

(defun fast-fibo (n)
  (fibo0 n 0 1))
  

(loop for i from 0 upto 28 do (print (fibo i)))
(loop for i from 0 upto 100 do (print (fast-fibo i)))
