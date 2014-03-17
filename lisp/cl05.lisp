; Chapter 5

(let (x y)
  (list x y))

; 1
(setf y '(1 2 3))
(let ((x (car y)))
  (cons x x))
; is equivalent to
((lambda (x)
    (cons x x))
 (car y))

(setf x '(1 2 3))
(setf z 4)
(let* ((w (car x))
       (y ( + w z)))
  (cons w y))
; is equivalent to
((lambda (w)
   ((lambda (y)
     (cons w y))
   (+ w z)))
 (car x))

; test order of evaluation
((lambda (x)
   (format t "outer")
   ((lambda (y)
      (format t "inner"))
    2))
 1)

; 3
(defun sq-pos (x)
  (if (> x 5)
      (* x x)))
