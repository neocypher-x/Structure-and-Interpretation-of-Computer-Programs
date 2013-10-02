(define (mult6 x)
  (* x 4))

(+ 3 2)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))
(factorial 20)

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(define (f n)
  (cond ((< n 3) n)
	((>= 3) ( + (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
	)))


(f 2)

(+ 3 2 1)
