(defun merge-worker (l a b)
  (cond ((and (null a) (null b)) l)
        ((null a)
         (merge-worker (append l (list (first b)))
                       a
                       (rest b)))
        ((null b)
         (merge-worker (append l (list (first a)))
                       (rest a)
                       b))
        ((< (first a) (first b))
         (merge-worker (append l (list (first a)))
                       (rest a)
                       b))
        ((>= (first a) (first b))
         (merge-worker (append l (list (first b)))
                       a
                       (rest b)))))

(defun do-merge (a b)
  (merge-worker '() a b))

(defun first-half (l)
  (subseq l 0 (floor (length l) 2)))

(defun last-half (l)
  (subseq l (floor (length l) 2) (length l)))

(defun merge-sort (l)
  (cond ((null l) nil)
        ((= (length l) 1) l)
        (T (do-merge (merge-sort (first-half l))
                     (merge-sort (last-half  l))))))

(setq random-list (loop :repeat 500 :collect (random 1001)))
(print random-list)
(print "")
(print (merge-sort random-list))
