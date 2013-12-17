(setf arr (make-array '(2 3) :initial-element nil))

(aref arr 0 0)

(setf (aref arr 0 0) 'b)

(vector "a" 'b 3)

(setf vec (make-array 4))

(svref vec 0)

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start))
	    obj
	  nil)
      (let ((mid (+ start (round (/ range 2)))))
	(let ((obj2 (aref vec mid)))
	  (if (< obj obj2)
	      (finder obj vec start (- mid 1))
	    (if (> obj obj2)
		(finder obj vec (+ mid 1) end)
	      obj)))))))

(sort "elbow" #'char<)

(aref "abc" 1)

(char "abc" 1)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

(equal "fred" "fred")

(equal "fred" "Fred")

(string-equal "fred" "Fred")

; Hello world
(copy-seq "Hello World!")

(format nil "~A or ~A" "truth" "dare")

; Hello World
(format nil "~A" "Hello World!")

(defun block-height (b) (svref b 0))

(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))
(point-x p)
(setf (point-y p) 2)
(point-p p)
(typep p 'point)

(defstruct polemic
  (type (progn
	  (format t "What kind of polemic was it? ")
	  (read)))
  (effect nil))
(make-polemic)

(defstruct (point (:conc-name p)
		  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))
(make-point)

(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l   (bst-insert obj (node-l bst) <)
		 :r   (node-r bst))
	        (make-node
		 :elt elt
		 :r   (bst-insert obj (node-r bst) <)
		 :l   (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(bst-find obj (node-l bst) <)
	        (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(setf nums nil)

(dolist (x '( 5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))

(bst-find 12 nums #'<)

(bst-find 4 nums #'<)

(bst-min nums)

(bst-max nums)

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    (percolate bst)
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-remove obj (node-l bst) <)
		 :r (node-r bst))
	        (make-node
		 :elt elt
		 :r (bst-remove obj (node-r bst) <)
		 :l (node-l bst)))))))

(defun percolate (bst)
  (cond ((null (node-l bst))
	 (if (null (node-r bst))
	     nil
	     (rperc bst)))
	((null (node-r bst)) (lperc bst))
	(t (if (zerop (random 2))
	       (lperc bst)
	       (rperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
	     :l (node-l bst)
	     :r (percolate (node-r bst))))

(defun lperc (bst)
  (make-node :elt (node-elt (node-l bst))
	     :l (percolate (node-l bst))
	     :r (node-r bst)))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(setf nums (bst-remove 2 nums #'<))

(bst-find 2 nums #'<)

(bst-traverse #'princ nums)

; Section 4.8 Hash Tables

(setf ht (make-hash-table))
(gethash 'color ht)
(setf (gethash 'color ht) 'red)
(gethash 'color ht)

(setf bugs (make-hash-table))
(push "Doesn't take keyword arguments."
      (gethash #'our-member bugs))

(setf fruit (make-hash-table))
(setf (gethash 'apricot fruit) t)
(gethash 'apricot fruit)
(remhash 'apricot fruit)

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)
(maphash #'(lambda (k v)
	     (format t "~A = ~A~%" k v))
	 ht)
