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
