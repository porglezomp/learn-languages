(defun qsort (l)
  (print l)
  (cond 
   ((null l) nil)
   ((= (length l) 1) l)
   (T
    (setq l1
          (qsort
           (remove-if-not
            (lambda (x) (> (first l) x))
            (rest l))))
    (setq l2
          (qsort
           (remove-if
            (lambda (x) (> (first l) x))
            (rest l))))
    (append
     (append
      l1
      (list (first l)))
     l2))))

(print (qsort '(3 4 2 5 1)))
