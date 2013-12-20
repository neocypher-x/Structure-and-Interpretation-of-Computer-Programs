(union '(a b c) '(c b s))
(union '(a b c) '(b a d))

; 5
(defun pos+ (lst)
  (defun recur (lst i)
    (if (null lst)
	nil
        (cons (+ i (car lst))
	      (recur (cdr lst) (+ i 1)))))
  (recur lst 0))

