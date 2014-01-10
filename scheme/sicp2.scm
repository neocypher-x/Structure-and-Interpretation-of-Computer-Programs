; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0)) (cons (/ (- n) g)
				       (/ (- d) g)))
	  ((and (> n 0) (> d 0)) (cons (/ n g)
				       (/ d g)))
	  ((and (< n 0) (> d 0)) (cons (/ n g)
				       (/ d g)))
	  ((and (> n 0) (< d 0)) (cons (/ (- n) g)
				       (/ (- d) g))))))

; 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b)
  (cons a b))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))
(define (x-point z)
  (car z))
(define (y-point z)
  (cdr z))

(define (midpoint-segment l)
  (let ((p1 (start-segment l))
	(p2 (end-segment l)))
    (let ((x1 (x-point p1))
	  (x2 (x-point p2))
	  (y1 (y-point p1))
	  (y2 (y-point p2)))
    (make-point (/ (+ x1 x2) 2)
		(/ (+ y1 y2) 2)))))

(define a (make-point 1 1))
(define b (make-point 4 4))
(define l (make-segment a b))

; 2.3
; w: width, h:height, x,y: coordinates of
; point of bottom left corner
(define (make-rect w h x y)
  (cons (cons w h) (make-point x y)))
(define (width x)
  (car (car x)))
(define (height x)
  (cdr (car x)))
(define (pos x)
  (cdr (cdr x)))

(define (perimeter rect)
  (+ (* 2 (width rect))
     (* 2 (height rect))))
(define (area rect)
  (* (width rect)
     (height rect)))
; bl: bottom-left point, tr: top right point
(define (make-rect blx bly trx try)
  (cons (make-point blx bly) (make-point trx try)))
(define (bl rect)
  (car rect))
(define (tr rect)
  (cdr rect))
(define (width rect)
  (- (x-point (tr rect))
     (x-point (bl rect))))
(define (height rect)
  (- (y-point (tr rect))
     (y-point (bl rect))))
; same procedures as above for width and height

; 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; 2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (logbase b n)
  (/ (log n)
     (log b)))
; returns number of times n is divisible by div
(define (recurdiv n div)
  (if (not (= 0 (remainder n div)))
      0
      (+ 1 (recurdiv (/ n div) div))))
; returns the result of dividing div out of n
(define (divideout n div)
  (if (not (= 0 (remainder n div)))
      n
      (divideout (/ n div) div)))
(define (car c)
  (logbase 2 (divideout c 3)))
(define (cdr c)
  (logbase 3 (divideout c 2)))

; returns int, more accurate than the previous one
(define (logbase b n)
  (define (recur x k)
    (if (< (/ x b) b)
	(+ k 1)
	(recur (/ x b) (+ k 1))))
  (recur n 0))

; 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

; 2.9
(define (width-interval x)
  (abs (/ (- (lower-bound x) (upper-bound x)) 2)))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

; Our starting point:
(define (width-sum x y)
  (width-interval (add-interval x y)))
; Substituing the two above function bodies gives:
(define (width-sum x y)
  (abs (/ (-
	     (+ (lower-bound x) (lower-bound y))
	     (+ (upper-bound x) (upper-bound y)))
	  2)))
; which gives:
(define (width-sum x y)
  (+ (abs (/ (- (lower-bound x) (upper-bound x))))
     (abs (/ (- (lower-bound y) (upper-bound y))))))
; which is equivalent to:
(define (width-sum x y)
  (+ (width-interval x)
     (width-interval y)))
; A similar argument holds for subtraction.
; Counter-example for multiplication: 
; Multiplying [0,1] with [0,1] produces [0,1],
; but multiplying 0.5 with 0.5 produces 0.25 \= 0.5
; Counter-example for division:
; dividing [0,1] by [1,2] gives [0,1],
; but dividing 0.5 with 0.5 gives 1 \= 0.5

; 2.10
(define (contain0? y)
  (and (<= (lower-bound y) 0)
       (>= (upper-bound y) 0)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (contain0? y)
      (error "The second interval may not contain 0")
      (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y))))))

; 2.11
; a  b  c  d
; +  +  +  + (a*c) (b*d)
; +  +  +  - does not exist (DNE)
; +  +  -  + (b*c) (b*d)
; +  +  -  - (b*c) (a*d)
; +  -  +  + DNE
; +  -  +  - DNE
; +  -  -  + DNE
; +  -  -  - DNE
; -  +  +  + (a*d) (b*d)
; -  +  +  - DNE
; -  +  -  + 4 multiplications, use min and max
; -  +  -  - (b*c) (a*c)
; -  -  +  + (a*d) (b*c)
; -  -  +  - DNE
; -  -  -  + (a*d) (a*c)
; -  -  -  - (a*c) (b*d) ===duplicate

(define (mul-interval x y)
  (define (pos? x)
    (>= x 0))
  (define (neg? x)
    (<= x 0))
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((or (and (>= a 0) (>= b 0) (>= c 0) (>= d 0))
	       (and (<= a 0) (<= b 0) (<= c 0) (<= d 0)))
	   (make-interval (* a c) (* b d)))
	  ((and (pos? a) (pos? b) (neg? c) (pos? d))
	   (make-interval (* b c) (* b d)))
	  ((and (pos? a) (pos? b) (neg? c) (neg? d))
	   (make-interval (* b c) (* a d)))
	  ((and (neg? a) (pos? b) (pos? c) (pos? d))
	   (make-interval (* a d) (* b d)))
	  ((and (neg? a) (pos? b) (neg? c) (neg? d))
	   (make-interval (* b c) (* a c)))
	  ((and (neg? a) (neg? b) (pos? c) (pos? d))
	   (make-interval (* a d) (* b c)))
	  ((and (neg? a) (neg? b) (neg? c) (pos? d))
	   (make-interval (* a d) (* a c)))
	  ((and (neg? a) (pos? b) (neg? c) (pos? d))
	   (let ((p1 (* a c))
		 (p2 (* a d))
		 (p3 (* b c))
		 (p4 (* b d)))
	     (make-interval (min p1 p2 p3 p4)
			    (max p1 p2 p3 p4)))))))

; 2.12
; One way to do it using make-interval
(define (make-center-percent c t)
  (let* ((proportion (/ t 100.0))
	 (totalerror (abs (* c proportion))))
    (make-interval (- c totalerror) (+ c totalerror))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent x)
  (let ((c (center x)))
    (* 100.0
       (/ (abs (- (upper-bound x) c)) c))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Another way to do it using make-center-width
(define (make-center-percent c t)
  (make-center-width c (* c (/ t 100))))
(define (percent x)
  (* 100.0
     (/ (width c) (center x))))

; 2.13
; Approximation for the tolerance of product
; of two intervals
(define (tol-product x y)
  (+ (percent x) (percent y)))

; 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
(define r1 (make-center-percent 100 5))
(define r2 (make-center-percent 200 2))
(par1 r1 r2)
(par2 r1 r2)

; 2.15
(div-interval r1 r1)
; In interval arithmetic, the concept of 
; identity is not clear, ie dividing an
; interval by itself does not produce 1,
; only approximates it. This means that
; anytime an interval is reintroduced
; (usually by multiplication of the term
; (x/x), additional error is introduced
; to the equation. Hence, Eva is right.

; 2.16
; This issue is explained as the dependency
; problem in wikipedia.

; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))

; 2.18
(define (reverse_ lst)
  (if (null? lst)
      ()
      (append (reverse_ (cdr lst)) (list (car lst)))))

; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(cc 100 us-coins)
(define us-coins (list 5 10 50 25 1))
(define us-coins (list 1 5 10 25 50))

; The order of coin-values does not affect the value
; produced by cc.

; 2.20
(define (same-parity . x)
  (define (get-parity lst)
    (if (= (remainder (car lst) 2) 0)
	0
	1))
  (define (test-parity lst result parity)
    (if (null? lst)
	result
	(if (= (remainder (car lst) 2) parity)
	    (test-parity (cdr lst) (append result (list (car lst))) parity)
	    (test-parity (cdr lst) result parity))))
  (test-parity x () (get-parity x)))