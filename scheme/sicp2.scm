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
