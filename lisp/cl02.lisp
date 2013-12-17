; Exercises
;1
(+ (- 5 1) (+ 3 7)) ; 14
(list 1 (+ 2 3)) ; (1 5)
(if (listp 1) (+ 1 2) (+ 3 4)) ; 1 is not a list so it outputs (+ 3 4) = 7
(list (and (listp 3) t) (+ 1 2)) ; (NIL 3)

;2 
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b (cons 'c ())))
(cons 'a (cons 'b (cons 'c '())))

;3
(defun 4th-elt (lst)
  (car (cdr (cdr (cdr lst)))))

;4
(defun greater (x y)
  (cond ((> x y) x)
	((< x y) y)
	(t nil)))

;5
; Returns true if the input list contains nil
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))
