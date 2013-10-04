; Structure and Interpretation of Computer Programs

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

; Ackerman's function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

; f(n) = f(n-1) + 2 f(n-2) + 3 f(n-3)
; both recursive and iterative implementations
(define (f n)
  (cond ((< n 3) n)
	((>= 3) ( + (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
	)))

(define (f n)
  (func-iter 2 1 0 n))

(define (func-iter a b c count)
  (if (= count 0)
      c
      (func-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

; computes values in Pascal's triangle
; i is the level starting with 1 at topmost
; j is the element starting at 1 
(define (f i j)
  (cond ((= j 1) 1)
	((= i j) 1)
	(else (+ (f (- i 1) (- j 1)) (f (- i 1) j)))
	))

; Recursive version
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;Iterative version
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

;fast version
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

; Exercise 1.16 logarithmic exponentiation
; Bounded above by O(2log(n)) = O(log(n)) in worse case
; which arises if n is not a power of 2
; this means before every division by 2, n will be odd,
; sums up to log(n) divisions by 2 that alternate with
; log(n) iterations of expt-square-iter that take care of n odd
; to give 2log(n)
(define (expt-square b n)
  (expt-square-iter b n 1))

(define (expt-square-iter b n a)
  (cond ((= n 0) 1)
	((= n 1) (* a b))
	((even? n) (expt-square-iter b
				     (/ n 2)
				     (* a (expt b (/ n 2)))))
	(else (expt-square-iter b (- n 1) (* a b)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	(else (fast-mult (double a) (halve b)))))

(define (halve a)
  (/ a 2))

(define (double a)
  (+ a a))
