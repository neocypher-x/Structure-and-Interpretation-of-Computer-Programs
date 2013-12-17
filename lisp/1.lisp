(defstruct polemic
	(type (progn
		(format t "What kind of polemic was it? ")
		(read)))
	(effect nil))

(setf arr (make-array '(2 3) :initial-element 42))

(setf vec (make-array 8 :initial-element 9000))
