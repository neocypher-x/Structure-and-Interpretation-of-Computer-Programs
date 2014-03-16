; 1
(setf y '(1 2 3))
(let ((x (car y)))
  (cons x x))

; 3
(defun sq-pos (x)
  (if (> x 5)
      (* x x)))
