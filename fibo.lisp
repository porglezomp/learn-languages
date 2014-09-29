(defun fibo (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (T (+ (fibo (- n 1)) (fibo (- n 2))))))

(loop for i from 0 upto 28 do (print (fibo i)))
