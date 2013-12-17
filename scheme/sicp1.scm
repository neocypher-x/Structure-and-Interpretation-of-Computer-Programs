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

; 1.17
(define (fast-mult a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	((odd? b) (+ a (fast-mult a (- b 1))))
	(else (fast-mult (double a) (halve b)))))

(define (halve a)
  (/ a 2))

(define (double a)
  (+ a a))

(define (odd? n)
  (= (remainder n 2) 1))

; 1.18
; a will be store state information:
; as the remainder of b / 2 if b is odd
; b must be non-negative
(define (fast-mult2 a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b c)
  (cond ((= b 0) 0)
	((= b 1) (+ a c))
	((odd? b) (fast-mult-iter a (- b 1) (+ a c)))
	(else (fast-mult-iter (double a) (halve b) c))))

; 1.19
; p=0 and q=1 causes p' and q' to be imaginary
; p' = sqrt(1/5) * sqrt(3p - q +- 2*sqrt(p^2 + pq - q^2))
; q' = sqrt(1/5) * sqrt(2p + q +- 2*sqrt(p^2 + pq - q^2))
(define (p-prime p q)
  (* (sqrt (/ 1 5))
     (sqrt (+ (- (* 3 p) q)
	      (* 2 (sqrt(+ (expt p 2) (- (* p q) (expt q 2)))))))))

(define (q-prime p q)
  (* (sqrt (/ 1 5))
     (sqrt (+ (+ (* 2 p) q)
	      (* 2 (sqrt(+ (expt p 2) (- (* p q) (expt q 2)))))))))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (p-prime p q)
		   (q-prime p q)
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
			     
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp M)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
		     (cond ((= times 0) true)
			   ((fermat-test n) (fast-prime? n (- times 1)))
			   (else false)))

; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (test n)
  (timed-prime-test n))

(define (test1 n)
  (display (+ n 1))
  (+ n 5)
  (+ n 6))

(define (test2 n)
  (if (> n 6)
      (display "yes")
      (display "no")))

(define (calculate a b)
  (timed-prime-test a)
  (if (< a b)
      (answer (+ 2 a) b)))


(define (answer a b n)
  (timed-prime-test a)
  (if (< n 4)
      ((display (timed-prime-test a))
      (if (prime? a)
	  (answer (+ a 2) b (+ n 1))
	  (answer (+ a 2) b n)))))

; 3 smallest primes > 1000 are
; 1009, 1013, and 1019

; computing the 3 smallest primes > 10000
; with (answer 10001 20001 0) gives
; 10007, 10009, and 10037

; 3 smallest primes > 100000:
; 100003, 100019, and 100043

; 3 smallest primes > 1000000:
; 1000003, 1000033, and 1000037

; 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; 1.25

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; this is correct. It would also serve our fast-prime-tester

; 1.26
; the square procedure would only require 1 call of expmod
; Upon evaluation of expmod, it would then multiply their
; values. However, Louis has now made 2 calls of expmod,
; doubling the number of expmod calls for each level of 
; the recursion tree.

; 1.27

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (test n x)
  (display x)
  (display (= (expmod x n n) x))
  (newline)
  (if (< (+ x 1) n) (test n (+ x 1))
      (+ n 0)))

; This procedure takes an integer n and performs
; the Fermat test on *all* integers less than n
; and greater than 0. It returns true if Fermat's
; Little Theorem holds true for all integers less
; than n and greater than 1, and false otherwise.
(define (fermatall n)
  (define (helper n x)
    (if (= (expmod x n n) x)
        (if (< (+ x 1) n)
            (helper n (+ x 1))
            #t)
        #f))
  (helper n 1))

; 1.28
; this took a while to understand

(define (expmodsqrt base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (if (= 1
		(remainder (square (expt base (/ exp 2)))
			   m))
	     0)
         (remainder (square (expmodsqrt base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmodsqrt base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmodsqrt a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

; Section 1.3.1
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (add-h x) (+ x (/ (- b a) n)))
  (define (f-all f)
    (if 
  (* (sum (f-all f) a add-h b)
     (/ (/ (- b a) n) 3)))

(define (f-all f a)
  (if (= 1 (remainder a 2))
      (* 5 (f a))
      0))

; 1.29
(define (simpsons f a b n)
  (define h
    (/ (- b a)
       n))
  (define (add-h x)
    (+ x (/ (- b a) n)))
  (define (f-term c)
    (if (remainder (/ (- c a) h)
                   2
  (* (/ (/ (- b a) n) 3)
     (sum f-term a add-h b)))

(define (simpsons f a b n)
  (define (sum-ext

; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (factorial n)
  (product identity 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter (term a) 1))

(define (pi-product n)
  (define (pi-term x)
    (if (= 1 (remainder x 2))
        (/ (+ x 1) (+ x 2))
        (/ (+ x 2) (+ x 1))))
  (* (product pi-term 1 inc n)
     4))

; 1.32
(define (accumulator combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulator combiner null-value term (next a) next b))))

(define (accumulator-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; 1.33
(define (filtered-accumulate combiner null-value term a next b filterp)
  (if (> a b)
      null-value
      (if (filterp a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b filterp))
          (combiner null-value
                    (filtered-accumulate combiner null-value term (next a) next b filterp)))))

;a a=1 b=5
(filtered-accumulate + 0 cube 1 inc 5 prime?)

;b n=7
(filtered-accumulate * 1 identity 1 inc 7 rel-prime?)

; Section 1.3.3












; 1.35
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ (/ 1 x) 1)) 2.0)

; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 10)

(define (average x y)
  (* (+ x y)
     (/ 1 2)))

(define (exp-fixed x)
  (fixed-point (lambda (y) (average y (/ (log x) (log y))))

               10))

; 1.37
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i k)
        (/ (n i) (d i))
        (+ result (iter (+ i 1)

(define (cont-frac n d k)
  (if (= 1 k)
      (/ (n 1) (d 1))


























; Section 1.3.4










; Newton's Method
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
                                         




















(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

 (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; 1.40
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
(newtons-method (cubic 0 0 1) 1)

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
(define (inc x)
  (+ x 1))
(((double (double double)) inc) 5)

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
((compose square inc) 6)

; 1.43
(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (- n 1)))))
((repeated square 2) 5)

; 1.44
(define (smooth f)
  (lambda (x)
    (/
     (+ (f (- x dx))
	(f x)
	(f (+ x dx)))
     3)))
(define (n-fold-smooth f n)
  (repeated smooth n) f)

; 1.45
; Calculate arbitrary nth root of a number using the fixed-point
; method and average damping to ensure convergence.
(define (3-root x)
  (fixed-point (lambda (y) (/ x (square y)))
	       1.0))
(define (3-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (4-root x)
  (fixed-point (lambda (y) (/ x (cube y)))
	       1.0))
(define (4-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y)))))
	       1.0))
(define (4-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y))))
	       1.0))

(define (5-root x)
  (fixed-point (lambda (y) (/ x (* y y y y)))
	       1.0))
(define (5-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y y))))
	       1.0))

(define (6-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y y y))))
	       1.0))

(define (7-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y y y y))))
	       1.0))

(define (8-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (* y y y y y y y))))
	       1.0))

(define (12-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (* y y y y y y y y y y y))))
	       1.0))

(define (16-root x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (* y y y y y y y y y y y y y y y))))
	       1.0))

(define (log-base x base)
  (/ (log x) (log base)))
(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log-base n 2)))
		(lambda (y) (/ x
			       (expt y (- n 1)))))
		1.0))

; 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
	guess
	(improve guess))))
