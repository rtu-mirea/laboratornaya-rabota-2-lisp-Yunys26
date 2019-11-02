;; №1 а)
(defun insert-at (number lis value)
  (if (= number 0) (cons value lis)
      (cons (car lis) (insert-at (1- number) (cdr lis) value))))

;; №1 б)
(defun del-by-num (number lis) 
(cond ((null lis) nil) 
((zerop number) (cdr lis)) 
((cons (car lis) (del-by-num (1- number) (cdr lis))))))

;; №1 в)
(defun postions (needing_elem lis)
  (loop
    for element in lis
    and position from 0
     when (eql element needing_elem)
      collect position))


;; №2
(defun out (path)
(let ((in (open path :if-does-not-exist nil)))
(when in
(loop for line = (read-line in nil)
while line do (format t "~a~%" line))
(close in))))
(out ("ts.txt"))

;; №3
(defun DeepDecompress (lst)
(if (zerop (car lst)) nil
(cons (second lst) (DeepDecompress (list (1- (car lst)) (second lst))))))

(defun Decompress (lst)
(cond ((null lst) nil)
((atom (car lst)) (cons (car lst) (Decompress (cdr lst))))
((consp (car lst)) (append (DeepDecompress (car lst)) (Decompress (cdr lst)))
)))

(defun CompareList (val acc)
(if (> acc 1) (list acc val) val))

(defun Accum (val acc lst)
(cond ((null lst) (cons (CompareList val acc) nil))
((eq val (car lst)) (Accum val (1+ acc) (cdr lst)))
(t (cons (CompareList val acc) (Accum (car lst) 1 (cdr lst))))))

(defun Compress (lst)
(cond ((null (cdr lst)) '())
(t (Accum (car lst) 1 (cdr lst))))))