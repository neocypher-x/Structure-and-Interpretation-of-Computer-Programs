; 2
(union '(a b c) '(c b s))
(union '(a b c) '(b a d))
(union '(a b c) '(1 2 3))

(defun new-union (lst1 lst2)
  (let ((lst lst1))
    (mapcar #'(lambda (x)
		(if (not (member x lst1))
		    ; could also use (push x (cdr (last lst)))
		    (setf lst (append lst (list x)))))
	    lst2)
    lst))

(new-union '(a b c) '(1 2 3))

; 3
(setf hash '((a . x) (b . y) (c . z)))
(assoc 'a hash)

(defun occurences (alst)
  (let ((ret nil))
    (if (consp alst)
	(progn
	  (mapcar #'(lambda (x)
		    (if (assoc x ret)
			(incf (cdr (assoc x ret)))
		        (push (cons x 1) ret)))
		alst)
	  (sort ret #'> :key #'cdr)))
    ret))

(occurences '(a b a d a c d c a))

; 4
; (member '(a) '((a) (b))) implicitly uses
; eql to test if '(a) and the (a) in the succeeding
; list are references to the same object. One must use
; (member '(a) '((a) (b)) :test #'equal) so that
; equal is used instead of eql to test if '(a) would
; print the same as the (a) in '((a) (b)).

; 5
(defun pos+ (lst)
  (defun recur (lst i)
    (if (null lst)
	nil
        (cons (+ i (car lst))
	      (recur (cdr lst) (+ i 1)))))
  (recur lst 0))

(defun pos+ (lst)
  (let ((ret nil))
    (do ((i 0 (+ i 1)))
	((= i (length lst)) ret)
	(setf ret (append ret (list (+ i (nth i y))))))))

(defun pos+ (lst)
  (let ((i -1))
  (mapcar #'(lambda (x)
	      (incf i)
	      (+ x i))
	  lst)))

; 8
(defun showdots (lst)
  (if (null lst)
      nil
      (cons 