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

; 6
; The idea here is to provide some common functions
; that'll accept arguments as their original counter-
; parts and in the same order, but provide output
; such that car and cdr as originally implemented in
; lisp will seem to have their definitions reversed.
(defun cons_ (a b)
  (cons b a))
; The idea here is to reverse the way we form
; lists using cons, eg (cons (cons (cons () c) b) a)
; This can be achieved by using cons_ and passing
; arguments in the order one would pass if one
; were to create a regular list using cons
; I'll be using variadic functions here
(defun list_ (&rest args)
  (let ((lst nil))
    (dolist (x (reverse args))
      (setf lst (cons_ x lst)))
    lst))
(defun length_ (lst)
  (if (null lst)
      0
      (+ (length_ (car lst)) 1)))
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))
(defun member_ (obj lst)
  (if (null lst)
      nil
      (if (eql (cdr lst) obj)
	  lst
	  (member_ obj (car lst)))))

; 7
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))
(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))
(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt))
(setf com (compress '(1 1 1 0 1 0 0 0 0 1)))
(showdots com)

; 8
(defun showdots (lst)
  (if (null lst)
      nil
      ;(format t "(~A . ~A)" (car lst) (showdots (cdr lst)))))
      (progn
	(format t "(~A . " (car lst))
	(showdots (cdr lst))
	(if (null (cdr lst))
	    (format t "NIL"))
	(format t ")"))))

; correct version accounts for possibility of
; any of the list elements being a list or 
; even a dotted list
(defun showdots (lst)
  (if (not (consp lst))
      (format t "~A" lst)
      (progn
	(format t "(")
	(if (consp (car lst))
	    (progn (showdots (car lst))
		   (format t " . "))
	    (format t "~A . " (car lst)))
	(showdots (cdr lst))
	(format t ")"))))

; 9
<<<<<<< HEAD
; This function finds the longest
; path from start to end nodes in a directed graph, with
; no cycles. If there are multiple solutions, it only
; returns one of them.
(defun longest-path (start end net)
  (dfs end (list (list start)) net))
; keep track of longest path length
; a variable for lognest candidate path seen so far
; allow for multiple longest paths
(defun bfs (end queue net)
  (if (null queue) ;if candidates are empty
=======
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
(defun bfs (end queue net)
  (if (null queue)
>>>>>>> 14b323bc97701e6f4686e838ad5132bbc2f5412d
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
<<<<<<< HEAD
	      (dfs end
=======
	      (bfs end
>>>>>>> 14b323bc97701e6f4686e838ad5132bbc2f5412d
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))
(setf min '((a b c) (b c) (c d)))
<<<<<<< HEAD

; let soln candidates be nil, curr greatest len = -1
; if candidate paths are not empty, then:
; 1) pop the last candidate path, path
; 2) pop the last element from path, x
; 3) if x equals end node then check 
;        if path length is greater than curr greatest 
;        length. if it is, then flush the current soln
;        candidates and add path to it. If not, then
;        discard.
;    If x does not equal end node, then create new
;    candidate paths by appending unvisited neighbor
;    nodes and push to the end of candidate paths.
; for now, solns just contains 1 path
(defun dfs (end queue net)
  (let ((solns nil)
	(greatestlen -1))
    (if (null queue)
	nil
        (let ((path (pop queue)))
	  (let ((x (car path)))
	    (if (eql x end)
		(if (> (length path) greatestlen)
		    (progn
		      (setf greatestlen (length path))
		      (setf solns (reverse path))))
	        (dfs end
		     (append (new-paths path x net)
			     queue)
		     net)))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (if (not (member n path))
		  (cons n path)))
	  (cdr (assoc node net))))
=======
(cdr (assoc 'a min))
(shortest-path 'a 'd min)
>>>>>>> 14b323bc97701e6f4686e838ad5132bbc2f5412d
