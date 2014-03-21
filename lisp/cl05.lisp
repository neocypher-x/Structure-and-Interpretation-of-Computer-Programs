; Chapter 5

(let (x y)
  (list x y))

(defconstant month
  #(0 31 59 90 120 151 181 212 273 304 365))

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (nmod y 100))))))

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
n
; 2
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))
(defun mystery (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (mystery x (cdr y))))
	     (and z (+ z 1))))))

; 3
(defun sq-pos (x)
  (if (> x 5)
      (* x x)))

; 4
(defun month-sum (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))
(defun month-sum (m y)
  (let ((days (case m (1 0) (2 31) (3 59)
		(4 90) (5 120) (6 151) (7 181)
		(8 212)	(9 243) (10 273)
		(11 304) (12 334))))
    (+ days (if (and (> m 2) (leap? y)) 1 0))))
