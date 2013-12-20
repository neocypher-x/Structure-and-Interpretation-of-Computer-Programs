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

; Returns the index in y where x appears, or nil
; if x does not appear in y
(defun mystery (x y)
  (if (null y)
      nil
    (if (eql (car y) x)
	0
      (let ((z (mystery x (cdr y))))
	(and z (+ z 1))))))

;6
(car (car (cdr '(a (b c) d))))
(or 13 (/ 1 0))
(apply #'list 1 nil)

;7
(defun listlist (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  't
	  (listlist (cdr lst)))))

;8
(defun print-dots (n)
  (format t ". ")
  (if (= n 1)
      nil
      (print-dots (- n 1))))

(defun print-dots (n)
  (do ((i 1 (+ i 1)))
      ((> i n))
      (format t ". ")))

(defun nfind (x lst)
  (if (null lst)
      0
      (if (eql x (car lst))
	  (+ 1 (nfind x (cdr lst)))
	  (nfind x (cdr lst)))))

(defun nfind (obj lst)
  (let ((n 0))
    (dolist (x lst)
      (if (eql x obj)
	  (setf n (+ n 1))))
	n))

(defun nfind (obj lst)
  (let ((n 0))
    (dolist (x lst)
      (format t "~A " (eql x obj)))
    n))

;9
(defun summit (lst)
  (let ((l (remove nil lst)))
    (apply #'+ l)))

(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
	(if (null (cdr lst))
	    0
	  (+ 0 (summit (cdr lst))))
        (+ x (summit (cdr lst))))))
