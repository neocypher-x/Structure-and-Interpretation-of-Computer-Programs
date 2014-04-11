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
  (if (and (> x 5) (integerp x))
      (* x x)))

; 4
(defun month-sum (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))
(defun month-sum (m y)
  (+ (case m (1 0) (2 31) (3 59)
		(4 90) (5 120) (6 151) (7 181)
		(8 212)	(9 243) (10 273)
		(11 304) (12 334))
    (if (and (> m 2) (leap? y)) 1 0)))

; 5
(defun precedes (x v)
  (cond ((not (standard-char-p x))
	 (error "First arg must be a char"))
	((not (stringp v))
	 (error "Second arg must be a string"))
	(t (dotimes (i (length v))
	     (format t "~A " (aref v i))))))
; iterative version
(defun precedes (x v)
  (let ((result ()))
    (dotimes (i (length v))
      (if (and (> i 0) (equal x (elt v i)))
	  (setf result (adjoin (elt v (- i 1)) result))))
    result))
; recursive version
(defun precedes (x v)
  (cond ((or (= 0 (length v)) (= 1 (length v))) '())
	((equal x (elt v 1))
	 (adjoin (elt v 0) (precedes x (subseq v 1))))
	(t (precedes x (subseq v 1)))))

; 6
(defun intersperse (c lst)
  (reduce (lambda (x y)
	    (append
	     (append (if (not (listp x)) (list x) x)
		     (list c))
	     (list y)))
	  lst))

; 7
; vacuously true for empty lists, true for base case of lists
; with length 1
; it is not clear whether difference between each successive
; pair means absolute value, second number minus first
; number, or first number minus second number.
; Hence, f is a function that may fulfill any of those 3
; possibilities.
(defun succ-diff-1 (f lst)
  (cond ((or (null lst) (null (cdr lst))) t)
	(t (and (= 1
		   (funcall f (car lst) (cadr lst)))
		(succ-diff-1 f (cdr lst))))))
(defun asc (x y)
  (- y x))
(defun desc (x y)
  (- x y))
(defun abs-diff (x y)
  (abs (- x y)))
(succ-diff-1 #'desc
	     (reverse '(1 2 3 4 5 6)))

; do
(defun succ-diff-1 (f lst)
  (do ((i 1 (+ i 1)))
      ((= i (length lst)) T)
    (if (not (= 1
		(funcall f (elt lst (- i 1)) (elt lst i))))
	(return ()))))

(succ-diff-1 #'abs-diff
	     '(1 2 1 2 3 4))

; mapc and return
(mapc #'(lambda (x y)
	  (format t "(~A ~A) " x y))
      '(1 2 3 4 5 6)
      '(a b c d e))

(defun succ-diff-1 (f lst)
  (mapc #'(lambda (x y)
	    (if (not (= 1
			(funcall f x y)))
		(return-from succ-diff-1 ())))
	lst
	(cdr lst))
  T)

; 8
; works for lists
(defun minmax (v)
  (defun helper (comp v curr)
    (cond ((null v) curr)
	  ((funcall comp (car v) curr)
	   (helper comp (cdr v) (car v)))
	  (t (helper comp (cdr v) curr))))
  (values (helper #'< v (car v))
	  (helper #'> v (car v))))
; implementation for vectors
(defun minmax (v)
  (defun helper (comp v curr)
    (cond ((= (length v) 0) curr)
	  ((funcall comp (svref v 0) curr)
	   (helper comp (subseq v 1) (svref v 0)))
	  (t (helper comp (subseq v 1) curr))))
  (values (helper #'< v (svref v 0))
	  (helper #'> v (svref v 0))))

; 9
; a
(defun shortest-path (start end net)
  (catch 'fin
    (bfs end (list (list start)) net)))
(defun bfs (end queue net)
  (format t "~A ~%" queue);debug only
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net end))
		   net))))))
(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
	      (let ((p (cons n path)))
		(if (eql n end)
		    (throw 'fin p)
		    p)))
	  (cdr (assoc node net))))

(setf min '((a b c) (b c) (c d)))
(cdr (assoc 'a min))
(shortest-path 'a 'd min)
(mapcar #'(lambda (x)
	    (cons x 42))
	'(1 2 3 4 5))
; b
; We'll use return to accomplish function termination by
; enclosing the two helper functions and their calling code
; within a block nil.
(defun shortest-path (start end net)
  (block nil
    (defun bfs (end queue net)
      (format t "~A ~%" queue);debug only
      (if (null queue)
	  nil
	  (let ((path (car queue)))
	    (let ((node (car path)))
	      (if (eql node end)
		  (reverse path)
		  (bfs end
		       (append (cdr queue)
			       (new-paths path node net end))
		       net))))))
    (defun new-paths (path node net end)
      (mapcar #'(lambda (n)
		  (let ((p (cons n path)))
		    (if (eql n end)
			(return p)
			p)))
	      (cdr (assoc node net))))
    (bfs end (list (list start)) net)))
