;; CHAPTER 2

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

(define (scale-list items factor)
  (if (null? items)
      ()
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

; 2.21
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

; 2.22
; The output will be in reverse order, because the cons in
; the iteration has each consecutive item cons'd leftwards
; of result, instead of rightwards.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items ()))

; The result produced from naively switching the order of the
; cons produces an improper list, where in the base case, ()
; gets cons'd to the front of the list, instead of at the end

; 2.23
(for-each_ (lambda (x) (newline) (display x))
	 (list 57 321 88))
(define (for-each_ f lst)
  (if (null? lst)
      ()
      (begin
	(f (car lst))
	(for-each_ f (cdr lst)))))

; 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; produces (1 2 3 4 5 6)
(cons x y)   ; produces ((1 2 3) 4 5 6)
(list x y)   ; produces ((1 2 3) (4 5 6))

; 2.27
(define x (list (list 1 2) (list 3 4)))
(reverse x)
(define (deep-reverse lst)
  (if (null? lst)
      ()
      (if (pair? lst) ; pair? considers dotted lists and proper lists, whereas list? only considers proper lists
	  (if (list? (car lst))
	      (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst))))
	      (append (deep-reverse (cdr lst)) (list (car lst))))
	  ())))

(deep-reverse x)

; 2.28
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (fringe x)
  (define (iter x result)
    (cond ((null? x) result)
	  ((not (pair? x)) (cons x result))
	  (else (begin (append result (iter (car x) result))
		       (append result (iter (cdr x) result))))))
  (iter x ()))
(define (fringe x result)
  (cond ((null? x) ())
	((not (pair? x)) (begin
			   (display x)
			   (display " ")
			   (list x)))
	(else (append
	       (fringe (car x) result)
	       (fringe (cdr x) result)))))


(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe x ())
(define y (list (list 1 2) 3 4 5 (list 6) 7))
(fringe y ())

; 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
; a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
; b
; accepts a mobile, which consists of just 2 branches
; a branch consists of a length, and either a number
; or another binary  mobile
(define (total-weight mobile)
  (define (mobile? branch)
    (list? (branch-structure branch)))
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (cond ((and (not (mobile? lb))
		(not (mobile? rb)))
	   (+ (branch-structure lb)
	      (branch-structure rb)))
	  ((and (not (mobile? lb))
		(mobile? rb))
	   (+ (branch-structure lb)
	      (total-weight rb)))
	  ((and (mobile? lb)
		(not (mobile? rb)))
	   (+ (total-weight lb)
	      (branch-structure rb))
	   (else
	    (+ (total-weight lb)
	       (total-weight rb)))))))
; c
(define (balanced? mobile)
  (define (mobile? branch)
    (list? (branch-structure branch)))
  (define (torque branch)
    (* (branch-length branch) (branch-structure branch)))
  (define (torque-mob branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (cond ((and (not (mobile? lb)) ; both branches contain weights
		(not (mobile? rb)))
	   (= (* (branch-length lb)
		 (branch-structure lb))
	      (* (branch-length rb)
		 (branch-structure rb))))
	  ((and (not (mobile? lb)) (mobile? rb))
	   (and (= (torque lb) (torque-mob rb))
		(balanced? (branch-structure rb))))
	  ((and (mobile? lb) (not (mobile? rb)))
	   (and (balanced? (branch-structure lb))
		(= (torque rb) (torque-mob lb))))
	  (else ; both branches contain mobiles
	   (and (balanced? (branch-structure lb))
		(balanced? (branch-structure rb))
		(= (torque-mob lb) (torque-mob rb)))))))

(define z (make-mobile (make-branch 2 3) (make-branch 3 2)))
(balanced? z)
(define v (make-mobile (make-branch 2 (make-mobile (make-branch 3 4) (make-branch 4 3))) (make-branch 7 3)))
(balanced? v)

; d
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define zz (make-mobile (make-branch 2 3) (make-branch 3 2)))
(balanced? zz)
; list? will have to be replaced with pair?
; one could also write a conversion interface that converts
; the new interface to the old interface.

; 2.30
(define (square-tree t)
  (cond ((null? t) ())
	((not (pair? t)) (square t))
	(else (cons (square-tree (car t))
		    (square-tree (cdr t))))))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree t)
  (map (lambda (st)
	 (if (pair? st)
	     (square-tree st)
	     (square st)))
       t))

; 2.31
; f is the function, t is tree
(define (tree-map f t)
  (map (lambda (x)
	 (if (pair? x)
	     (tree-map f x)
	     (f x)))
       t))
(define (square-tree tree) (tree-map square tree))

; 2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(map square (list 1 2 3 4 5))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append '(1 2 3) '(4 5 6))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length '(1 2 3 4 5))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

; 2.35
(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
		       (enumerate-tree t))))
(count-leaves (list 1
		    (list 2 (list 3 4) 5)
		    (list 6 7)))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
	    (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; assumes the appropriate dimensions for m and v
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(matrix-*-vector '((1 2) (3 4)) '(5 6))
(define (transpose mat)
  (accumulate-n cons () mat))
(transpose '((1 2 3) (4 5 6) (7 8 9)))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col)
		  (dot-product row col))
		cols))		  
	 m)))
(matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((1 0 0) (0 1 0) (0 0 1)))

; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list () (list 1 2 3))
(fold-left list () (list 1 2 3))
; op must commute for fold-left and fold-right to match on a sequence

; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))
(reverse '(1 2 3))

; Nested Mappings
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

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
(define (enumerate-interval i j)
  (if (> i j)
      ()
      (cons i (enumerate-interval (+ i 1) j))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

; 2.41
(define (unique-triples n)
  (flatmap (lambda (x)
	     (map (lambda (pair)
		    (append (list x) pair))
		  (unique-pairs (- x 1))))
	   (enumerate-interval 1 n)))

; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

; The choice of representation for a possible solution
; will be a list of integers, where the integers represent
; the row positions. For example, the integer in the 3rd
; list element represents the row position of the k-2 column.
(define (adjoin-position new-row k rest-of-queens)
  ;(append rest-of-queens (list new-row)))
  (append (list new-row) rest-of-queens))
(define empty-board ())

; check horizontal only
(define (safe? k positions)
  (let ((kth-col (car positions)))
    (define (safe?-helper rem)
      (if (null? rem)
	  #t
	  (and (not (= kth-col (car rem)))
	       (safe?-helper (cdr rem)))))
    (safe?-helper (cdr positions))))

; horizontal + diagonal checking
; for diagonal checking, the idea is to create a counter
; that keeps track of current column distance away from
; the kth column. For a column n, the distance is
; (abs (- k n)). Add this distance to the position of
; the queen in the kth column, and check for equality.
; Equality implies there's a diagonal check.
(define (safe? k positions)
  (let ((kth-col (car positions)))
    (define (safe?-helper i rem)
      (if (null? rem)
	  #t
	  (and (not (= kth-col (car rem)))
	       (not (= kth-col (+ (car rem) i)))
	       (not (= kth-col (- (car rem) i)))
	       (safe?-helper (+ i 1) (cdr rem)))))
    (safe?-helper 1 (cdr positions))))

(define (safe? k positions)
  #t)
(queens 4)

; 2.43
; The queen-cols call is expensive, since it generates
; all the solutions for the kth column. The original
; solution for queens generates a linear recursive
; process, since queen-cols will call itself once for
; each column. However, switching the order of queen-cols
; with enumerate interval causes queen-cols to be called
; for each element in enumerate-interval. Each call to
; queen cols will in turn generate an additional call to
; queen cols for each element in
; (enumerate-interval 1 board-size) further in the
; recursive call stack. Hence, a linear recursive call
; becomes a tree recursive call. So the new running
; time will be proportal to T^board-size.

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
; 2.44
;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

; 2.45
(define (split f g)
  (define (split-helper painter n)
    (if (= n 0)
	painter
	(let ((smaller (split-helper painter (- n 1))))
	  (f painter (g smaller smaller)))))
  (lambda (painter n)
    (split-helper painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

; 2.46
(define (make-vect x y)
	   (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
	     (+ (ycor-vect u) (ycor-vect v))))
(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
	     (- (ycor-vect u) (ycor-vect v))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; 2.48
(define (make-segment u v)
  (cons u v))
(define (start-segment v)
  (car v))
(define (end-segment v)
  (cdr v))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment))))
     segment-list)))

; 2.49
; a
(define (outline-frame frame)
  (let ((a (make-vect 0 0))
	(b (make-vect 0 1))
	(c (make-vect 1 0))
	(d (make-vect 1 1)))
    (let ((segment-list (list (make-segment a c)
			      (make-segment c d)
			      (make-segment d b)
			      (make-segment b a))))
      (segments->painter segment-list))))
; b
(define (x-frame frame)
  (let ((a (make-vect 0 0))
	(b (make-vect 0 1))
	(c (make-vect 1 0))
	(d (make-vect 1 1)))
    (let ((segment-list (list (make-segment a d)
			      (make-segment c b))))
      (segments->painter segment-list))))
; c
(define (diamond-frame frame)
  (let ((a (make-vect 0.5 0))
	(b (make-vect 1 0.5))
	(c (make-vect 0.5 1))
	(d (make-vect 0 0.5)))
    (let ((segment-list (list (make-segment a b)
			      (make-segment b c)
			      (make-segment c d)
			      (make-segment d a))))
      (segments->painter segment-list))))
; d
; skip
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

; 2.50
(define (flip-horiz  painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
; 2.51
; first time in a style analogous to beside
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))
; second time in terms of beside and rotation
(define (below painter1 painter2)
  (let ((p1 (rotate270 painter1))
	(p2 (rotate270 painter2)))
    (let ((bes (beside p1 p2)))
      (rotate90 bes))))
; 2.52
; a - skip
; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left up)
	      (bottom-right right)
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
; c
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  flip-horiz identity)))
    (combine4 (corner-split painter n))))

; 2.3

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

; 2.53
; (a b c)
(list 'a 'b 'c)
; ((george))
(list (list 'george))
; ((y1 y2))
(cdr '((x1 x2) (y1 y2)))
; (y1 y2)
(cadr '((x1 x2) (y1 y2)))
; #f
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; (red shoes blue socks)
(memq 'red '(red shoes blue socks))

; 2.54
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (eq? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else
	 #f)))

; 2.55
(car ''abracadabra)
; ''~ is a list, where ' is the first element and symbol ~
; is the other element.

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (- (exponent exp) 1)))
	  (deriv (base exp) x)))
	(else
	 (error "unknown expression type -- DERIV" exp))))
(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
	((= exp 1) base)
 	(else (list '** base exp))))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (exponentiation? x)
  (and (pair? x)
       (eq? '** (car x))))
; 2.57
; implementation works thus far only for representations
; where arguments to make-sum and make-product are
; themselves sums or products, not yet for just lists
; of numbers
(define (sumOrProd? exp)
  (or (sum? exp) (product? exp)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
; deriv of sums will be nested expressions
; argments will be single numbers or expressions
(define (make-sum a1 a2)
  (if (or (sumOrProd? a1) (sumOrProd? a2))
      (cons '+ (list a1 a2))
      (list '+ a1 a2)))
(define (augend s)
  (if (pair? (cdr (cddr s)))
      (cons '+ (cddr s))
      (caddr s)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
; deriv of products should be nested expressions
(define (make-product a1 a2)
  (if (or (sumOrProd? a1) (sumOrProd? a2))
      (cons '* (list a1 a2))
      (list '* a1 a2)))
(define (multiplicand p)
  (if (pair? (cdr (cddr p)))
      (cons '* (cddr p))
      (caddr p)))

; The below two implementations were started but not
; finished, to include arguments of list of numbers

; deriv of sums will given as a single list
; arguments can be list of numbers, expressions or singles
(define (make-sum a1 a2)
  (cond ((sum? a2)
	 (cons '+ (cons a1 (cdr a2))))
	((and (pair? a1) (pair? a2))
	 (cons '+ (append a1 a2)))
	((and (pair? a1) (not (pair? a2)))
	 (cons '+ (append a1 (list a2))))
	((and (not (pair? a1)) (pair? a2))
	 (if (sumOrProd? a2)
	     (cons '+ (cons a1 (list a2)))
	     (cons '+ (cons a1 a2))))	     
	(else (list '+ a1 a2))))
(define (augend s)
  (if (pair? (cdr (cddr s)))
      (make-sum (caddr s) (cdddr s))
      (caddr s)))
; deriv of products should be a single list
(define (make-product a1 a2)
  (cond ((product? a2)
	 (cons '* (cons a1 (cdr a2))))
	((and (pair? a1) (pair? a2))
	 (cons '* (append a1 a2)))
	((and (pair? a1) (not (pair? a2)))
	 (cons '* (append a1 (list a2))))
	((and (not (pair? a1)) (pair? a2))
	 (cons '* (cons a1 (list a2))))
	(else (list '* a1 a2))))
(define (multiplicand p)
  (if (pair? (cdr (cddr p)))
      (cons '* (cddr p))
      (caddr p)))

; 2.58
; a
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (make-sum a1 a2) (list a1 '+ a2))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (make-product m1 m2) (list m1 '* m2))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
; b
; Multiplication distributes over a list expression, whereas
; addition does not. We can assume that both arguments to 
; addition will have been evaluated. We have to evaluate
; all parenthetical expressions, then products, then sums.

; Works on (x + 3 * (x + y + 2)).

; yet to develop working implementation for (x * y + z)
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (make-sum a1 a2) (list a1 '+ a2))
(define (addend s) (car s))
(define (augend s)
  (if (pair? s)
      (if (null? (cdddr s))
	  (caddr s)
	  (cddr s))
      (caddr s)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (make-product m1 m2) (list m1 '* m2))
(define (multiplier p) (car p))
(define (multiplicand p)
  (if (pair? p)
      (if (null? (cdddr p))
	  (caddr p)
	  (cddr p))
      (caddr p)))

; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
; 2.59
; if set1 is null, then set2. If set1 is not null, then set1
; if set2 is null. Otherwise, continue. two equivalent
; representations. The second implementation is more uniform.
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (if (null? set2)
	  set1
	  (if (element-of-set? (car set1) set2)
	      (union-set (cdr set1) set2)
	      (cons (car set1) (union-set (cdr set1) set2))))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))
; 2.60
; Allow for repeating elements in a set
; element-of-set? stays the same
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set)
  (append set1 set2))
; intersection-set can stay the same.

; the running time for adjoin-set goes from O(n) to O(1)
; the running time for union-set goes from O(mn) to the
; running time of append, where m and n are sizes of set1 and set2.

; The increase in performance over the cost of unique set
; representations would be useful for cases where we use
; adjoin-set a lot or union-set.

; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set-ordered-lists set1 set2)
  (if (or (null? set1) (null? set2))
       '()
       (let ((x1 (car set1)) (x2 (car set2)))
	 (cond ((= x1 x2)
		(cons x1 (intersection-set-ordered-lists (cdr set1)
							 (cdr set2))))
	       ((< x1 x2)
		(intersection-set-ordered-lists (cdr set1) set2))
	       ((< x2 x1)
		(intersection-set-ordered-lists set1 (cdr set2)))))))

; 2.61
; on average, x will be in the middle of the list so that the
; running time is half that of adjoin-set for the onordered
; set representation
(define (adjoin-set x set)
  (cond ((null? set) ())
	((< x (car set)) (cons x set))
	((= x (car set)) set)
	(else (append (list (car set)) (adjoin-set x (cdr set))))))

; 2.62
(define (union-set-ordered-lists set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1
			     (union-set-ordered-lists (cdr set1)
					(cdr set2))))
		      ((< x1 x2)
		       (cons x1
			     (union-set-ordered-lists (cdr set1)
					set2)))
		      ((> x1 x2)
		       (cons x2
			     (union-set-ordered-lists set1
					(cdr set2)))))))))
; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) ())
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(define x (make-tree 5 '() '()))
(element-of-set? 7 x)
(define x (adjoin-set 1 x))
(define x (adjoin-set 7 x))
(define x (adjoin-set 5 x))
(define x (adjoin-set 9 x))
(define x (adjoin-set 3 x))
(define x (adjoin-set 11 x))
(define x (adjoin-set 12 x))
(tree->list-1 x)
(tree->list-2 x)
; a
; They produce the same result for every tree.
; (1 3 5 7 9 11) are printed for the trees in figure 2.16

; b
; Each procedure must visit each node, so the running time is at
; least O(n). However, for each iteration or recursion level,
; the running time differs slightly. tree->list-2 only calls
; cons, which is constant time so that the total running time of
; tree->list-2 is O(n). On the other hand, tree->list-1 calls
; append which from the definition in 2.2.1 has a running time
; proportional to the size of the first list argument. The first
; argument to this append halves for each successive call, so
; that the contribution on each recursive level is log(n).
; Hence, the total running time of tree->list-1 will be O(nlogn).

; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts )
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))
; a
; Partial tree works by selecting the n/2th element as the root
; of the current tree. It then takes the left (n - 1)/2 elements
; and calls itself to form a subtree with all elements less than
; the current root element. This is possible because the list is
; ordered. It does the same with the rightmost n/2 elements but
; this time with the property that they all are elements greater
; than the current root. Since the method picks the middle
; element at each recursion level, this ensures there are equal
; elements in both subtrees. It can be shown that this process
; will produced a balanced binary tree through induction on n.
(list->tree '(1 3 5 7 9 11))
;          5
;       /     \
;      1       9
;       \     / \
;        3   7   11

; b
; Each node must be visited once, and the work done at each
; recursive call is independent of n. Hence the order of growth
; is O(n).

; 2.65
; We can combine several methods from 2.63, 2.64 and the
; section on sets as ordered lists that have running times of
; O(n) to produce O(n) implementations of union-set and
; intersection-set for sets implemented as binary trees. Given
; a binary tree, use tree->list-2 to convert it to an ordered
; list with a running time of O(n). Next, since the list is
; ordered we can use intersection-set or union-set from the
; section of sets as ordered lists to produce an ordered list
; that's an intersection or union of the two lists,
; respectively. This also has running time O(n). Finally, use
; the list->tree procedure to convert the ordered list to a
; balanced binary tree. This procedure has running time O(n).
; Hence, O(n) + O(n) + O(n) = O(n).

; Helper function to union-set and intersection-set
(define (sets-apply set1 set2 method)
  (let ((lst1 (tree->list-2 set1))
	(lst2 (tree->list-2 set2)))
    (let ((result (method lst1 lst2)))
      (list->tree result)))) 
        ; where union-set-ordered-lists is defined in 2.62
(define (union-set-binary-trees set1 set2)
  (sets-apply set1 set2 union-set-ordered-lists))
(define (intersection-set-binary-trees set1 set2)
  (sets-apply set1 set2 intersection-set-ordered-lists)) 
       ; where intersection-set-ordered-lists is
       ;intersection-set defined for sets as ordered lists
; test
(define x '(1 3 5 7 9 11))
(define x (list->tree x))
(define y '(2 4 6 8 10 12))
(define y (list->tree y))
(tree->list-2 (union-set-binary-trees x y))
(tree->list-2 (intersection-set-binary-trees x y))

; 2.66
; returns the subtree where the root matches the given-key,
; or false if no such entry matches the given-key
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((= given-key (entry set-of-records))
	 set-of-records)
	((< given-key (entry set-of-records))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (entry set-of-records))
	 (lookup given-key (right-branch set-of-records)))))
(lookup 12 x)

; Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
; Decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)	      
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))
; Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))
; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (a d a b b c a)

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
			     (encode (cdr message) tree))))
(define (encode-symbol sym tree)
  (define (element-of-set? x set) ;unordered lists, because symbols may not be ordered
    (cond ((null? set) false)
	  ((equal? x (car set)) true)
	  (else (element-of-set? x (cdr set)))))
  (define (helper sym tree result)
    (cond ((leaf? tree) (cons result ()))
	  ((element-of-set? sym (symbols (left-branch tree)))
	   (helper sym (left-branch tree) (cons 0 result)))
	  ((element-of-set? sym (symbols (right-branch tree)))
	   (helper sym (right-branch tree) (cons 1 result)))
	  (else (error "Symbol not in tree" sym))))
  (helper sym tree ()))

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge (adjoin-set
			 (make-code-tree (cadr leaves)
					 (car leaves))
			 (cddr leaves)))))
(define test-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

; 2.70
(define 8tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2)
				       (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(encode '(Get a job) 8tree)
; ((0 0 0 0 0) (1 1 0 0) (1 0 0 0 0)) 
(encode '(Sha na na na na na na na na) 8tree)
; ((1 0 0 0) (1) (1) (1) (1) (1) (1) (1) (1)) 
(encode '(Wah yip yip yip yip yip yip yip yip yip) 8tree)
; ((1 0 1 0 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0) (1 0)) 
(encode '(Sha boom) 8tree)
; ((1 0 0 0) (0 0 1 0 0)) 

; 28 + 24 + 23 + 9 = 84 bits total for the whole song
; under variable length encoding.
; For fixed length encoding, there are eight symbols 
; so that we need 3 bits for every symbol that appears.
; The song has a total of 36 symbols, and hence we
; will need 36 * 3 = 108 bits to encode the song if we
; were to use fix-length encoding.

; 2.71
(define 5tree (generate-huffman-tree '((a 1) (b 2) (c 4)
				       (d 8) (e 16))))
; tree for n = 5, where frequences of symbols are 2^(n-1)
;         (a b c d e) 31
;         /             \
;       e 16       (a b c d) 16
;                  /          \
;               d 8         (a b c) 7
;                           /        \
;                         c 4     (b a) 3
;                                 /      \
;                                b 2     a 1
; the n=10, the tree looks similar
; For general n, 1 bit will always be used to encode
; the most frequent symbol. It would take 2^(n-1)
; bits to encode the least frequent symbol.

; 2.72
; For the most frequent symbol for the relative
; frequencies given in 2.71, it would take worst case
; O(n) steps to encode, because it would have to
; search through the entire set of symbols. For the
; frequent symbol, it would take O(n^2) steps, because
; it would have to traverse the height of the tree
; which grows linearly, and search through the set
; of symbols which also grows linearly in n. Hence,
; The general order of growth for encoding a symbol
; is O(n), because of height traversal that grows
; linearly in n, and work at each level due to 
; searching through a set of symbols whose size grows
; linearly in n.

; 2.73
; a
; The deriv procedure was turned into a data driven style,
; where dispatch is performed on the deriv procedure and the
; operator.
; There is no "type tag" for expression types of number or
; same variable. Hence, the operator procedure would not 
; work on a number or variable. This means that the
; appropriate deriv procedure for number or variable types
; cannot be retrieved using (get <op> <type>).
; b + c
; the corresponding representation procedures for sums,
; products, and exponentations are in exercise 2.57.

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum terms)
    (make-sum (deriv (addend terms))
	      (deriv (augend terms))))    
  (define (deriv-product terms)
    (make-sum (make-product (deriv (multiplier terms))
			    (multiplicand terms))
	      (make-product (multiplier terms)
			    (deriv (multiplicand terms)))))
  (define (deriv-exponentiation terms)
    (make-product
     (make-product (exponent exp)
		   (make-exponentiation (base exp)
					(- (exponent exp) 1)))
     (deriv (base exp) x)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

(map (lambda(x) x)
     '(1 2 3 4 5))

; d
; The correponding changes we need to make are only in
; flipping the order of indices are swapped in the put
; procedure.

; 2.74
; a
; takes personal file and employee name and returns the
; employee's record
(define (get-record file name)
  ((get 'record file) name))
; This assumes that there is a record function that is
; indexed by the personnel file, and that this record
; function takes in an employee name and returns the
; corresponding record object.
; b
; Although the problem staetment says that only the record
; is given, we must assume the relevant personnel file is
; available as well for this data-driven approach to work.
(define (get-salary file record)
  ((get 'salary file) record))
; get should return a function salary, relevant for the
; specific personnel file or division, that takes in an
; employee record object and returns the employee's salary.
; c
(define (find-employee-record name files)
  (cond ((null? files) #f)
	(else
	 (let ((record (get-record (car files) name)))
	   (if record
	       record
	       (find-employee-record name (cdr files)))))))
; this assumes that get-record returns false if no
; such record is found. It returns the reord if found, and
; the false if no such record is found.
; d
; For each new division's personnel files, a record and
; salary function must be implemented and added to the 
; data-driven table indexed by the personnel file. This is
; done using the put method.

; Message Passing
; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
; 2.76
; Generic operations with explicit dispatch
; New types require modifications to all generic operations
; New operations will require a new function that must
; handle all supported types
; Data-directed style
; New types require a new column in the table, or new
; functions in quantity equal to the number of operations;
; New operations will require a new row, or new functions
; in quantity equal to the number of supported types.
; Message-passing style
; New types require a new procedure with an internal dispatch
; procedure that handles all supported operations.
; New operations require a modification to each of the
; internal dispatch routines of the procedures that represent
; the type.
; For a system where new types must be often added, use the
; message-passing style. For a system where new operations
; must often be added, use the generic operations with
; explicit dispatch.

; 2.5 Generic Arithmetic Operations
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y)))))

; 2.77
; The structure in 2.24 as a list is represented with
(cons 'complex (cons 'rectangular (cons 3 4)))
; calling (magnitude z) results in the one function call to
; apply-generic. Inside the this call to apply-generic,
; calling type-tags on z results in a type-tag of complex.
; The next instruction attempts to retrieve the relevant
; magnitude function defined for type complex using the get
; function. However, there is no such function and hence the
; error.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; upon adding the four methods to the complex package by way
; of 'put', the first apply-generic call retrieves type
; complex, and does a get for operation magnitude. This get
; call returns the magnitude function that was put due to
; Alyssa's fix. This magnitude function is the same as the
; one immediately above, and calls apply-generic a second
; time. This time however, the z passed into the second
; apply-generic call has its complex tag stripped off, and
; so it is left with a type of rectangular. The second
; apply-generic call then attempts to retrieve the magnitude
; operation for type rectangular. This get call returns the
; magnitude function that was put from the
t; install-rectangular-package. The resulting magnitude for
; rectangular types takes its definition from the internal
; magnitude procedure of install-rectangular package, and its
; inner calls of real-part and imag-part take their
; definitions from the internal procedures of
; install-rectangular-package as well.

; 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; 2.79
; There are two potential solutions for the complex package.
; one fits nicely with the rectangular representation for complex 
; numbers, the other involves the polar representation.
; Since the 'natural' selectors for the rectangular
; representation are defined for polar and vice-versa, both
; solutions are equally valid.
; install-scheme-number-package
(put 'equ? '(scheme-number) =)
; install-rational-package
(define (equ?-rational a b)
  (and (= (numer a) (number b))
       (= (denom a) (denom b))))
(put 'equ? '(rational) equ?-rational)
; install-complex-package
(define (equ?-complex a b)
  (and (= (real-part a) (real-part b))
       (= (imag-part a) (imag-part b))))
(define (equ?-complex a b)
  (and (= (magnitude a) (magnitude b))
       (= (angle a) (angle b))))
(put 'equ? '(complex) equ?-complex)
(define (equ? a b)
  (apply-generic 'equ? a b))

; 2.80
; For similar reasons to 2.79, there are two solutions for
; complex numbers
; install-scheme-number-package
(define (=zero?-scheme-number x)
  (= x 0))
(put '=zero? '(scheme-number) =zero?-scheme-number)
; install-rational-package
(define (=zero?-rational x) (= 0 (numer x)))
(put '=zero? '(rational) =zero?-rational)
; install-complex-package
(define (=zero?-complex x)
  (and (= (real-part x) 0)
       (= (imag-part x) 0)))
(define (=zero?-complex x)
  (= (magnitude x) 0))
(put '=zero? '(complex) =zero?-complex)
(define (=zero? x)
  (apply-generic =zero? x))

; 2.81
; a
; An infinite recursive call stack would result;
; apply generic would get called infinitely many times.

; b
; apply generic works fine as it stands. In the case
; where we pass in two arguments of the same type to
; apply generic, consider two possibilities. If the
; generic procedure exists, then we are done. If no
; such operation exists, get-coercion would not return
; a coercion procedure. This would result in apply
; generic returning error saying that no method exists
; for the two types. In both cases, the return is as
; expected.

; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
		(if (not (eq? type1 type2))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))
		    (error "No method for same types"
			   (list op type-tags))))
	      (error "No method for these types"
		     (list op type-tags)))))))

; 2.82
(define (apply-generic op. args)
  ; returns process if coercion succeeds, #f otherwise
  (define (try-coercion out-type in-types)
    (define (try-coercion-iter cur-types)
      (if (null? cur-types)
	  #t
	  (let ((coercion-result (get-coercion (car cur-types) out-type)))
	    (if coercion-result
		(and #t (try-coercion-iter (cdr cur-types)))
		#f))))
    (if (try-coercion-iter in-types)
	(get op (map (lambda (type) out-type) in-types))
	#f))
  (define (outerloop type-tags)
    (define (outerloop-iter cur-tags)
      (if (null? type-tags)
	  (error "Exhausted coercion attempts to find method")
	  (let ((type (car type-tags)))
	    (let ((proc (try-coercion type type-tags)))
	      (if proc
		  (apply proc (map contents args))
		  (outerloop-iter (cdr cur-tags)))))))
    (outerloop-iter type-tags))
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (> (length args) 1)
              (outerloop type-tags)
	      (error "No method for these types"
		     (list op type-tags)))))))

; 3 cases bring up an issue with the above implementation

; Suppose we have an operaton op that takes in 3
; arguments of type aType, bType, and cType. Suppose
; aType and bType can be coerced into each other. If
; we passed into op three arguments of the following
; respective types, bType, aType, and cType, apply
; generic as implemented above would not coerce the
; first two arguments and leave the third alone.

; For the second case, suppose we pass in three
; arguments to op with respective types dType, bType,
; cType. Suppose dType could be coerced into aType.
; apply generic as implemented above would not perform
; the conversion from dType to aType.

; For the third case, consider op which takes in
; 3 arguments of type bType, bType, and cType. Suppose
; cType can be coerced into bType, but aTYpe can be
; coerced into cType. If we pass in aType, bTYpe, cType,
; The above implementation would coerce the types to
; bType, bTYpe, cType, and exit because of the failed
; converion of cType to bTYpe.

; There is a more elegant and concise solution that
; may have a higher chance of success by fixing the
; issue with the third case.

(define (apply-generic op . args)  
  (define (try-coercion out-type)
    (lambda (arg)
      (let ((in-type (type-tag arg)))
	(let ((proc (get-coercion in-type out-type)))
	  (if proc
	      (proc arg)
	      arg)))))
  (define (outerloop type-tags)
    (define (outerloop-iter cur-tags)
      (if (null? type-tags)
	  (error "Exhausted coercion attempts to find method")
	  (let ((out-type (car type-tags)))
	    (let ((coerced-args (map (try-coercion out-type) args)))
	      (let ((proc coerced-args))
		(if proc
		    (apply proc coerced-args)
		    (outerloop-iter (cdr cur-tags))))))))
    (outerloop-iter type-tags))
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (> (length args) 1)
              (outerloop type-tags)
	      (error "No method for these types"
		     (list op type-tags)))))))

; 2.83
; accept raw value with no type information
; returns raised value with no type information
; add to scheme-number package
(put 'raise '(integer) (lambda (i)
		       (make-rational i 1)))
; add to rational package
(put 'raise '(rational) (lambda (q)
			(/ (numer q) (denom q))))
; add to real package
(put 'raise '(real) (lambda (r)
		    (make-complex-from-real-imag r 0)))
(define (raise x) (apply-generic 'raise x)))

; 2.84
; The solution below works for an arbitrary number of arguments
; Adding new types is very simple and only involves the modification
; of one raise procedure as well as an addition of a new raise procedure
; in the general case. For the general case where a new type rests in
; between two types in the tower, modify the lower type's raise
; procedure to raise the object to the newly added type, and add a
; raise procedure that'll raise an object of the newly added type to
; that of the type immediately above it in the tower.

; As a proof of concept, consider the following scenario:
; Suppose we have two types, a and b. Let's try to determine which
; of the two types is higher in the tower. Without loss of generality,
; assume that b is a higher type than a. If we try to raise a first,
; eventually we will reach type b. If we raise b first, then we will
; eventually reach the top of the tower. In such a case, we know that
; a is of the lower type.

; The summary of our procedure is as follows:
; amongst the list of types, return the highest type
; try to retrieve a procedure for the list of same types
; if that is successful, then raise all args to that type and apply
; the retrieved procedure

; Note:
; accumulate will allow us to perform compare-type between the
; largest object found so far and each successive object in args
; accumulate is defined around exercise 2.34
(define (apply-generic op . args)
  ; first compare two objects and return the object that is of a higher type
  (define (compare-type a b)
    (if (equal? (type-tag a) (type-tag b))
	a
	(let ((raise-a (get 'raise (type-tag a))))
	  (if (null? raise-a)
	      a
	      (compare-type (raise-a a) b)))))
  (define (get-type-list t args)
    (map (lambda (x) t) args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (let (top-type (type-tag (accumulate compare-type (car args) (cdr args))))
	    (let ((proc-top-type (get op (get-type-list top-type args))))
	      (if (proc-top-type)
		  (apply proc-top-type (map contents args))
		  (error
		   "No method for these types -- APPLY-GENERIC"
		   (list op type-tags)))))))))
; 2.85
; add in complex package
(put 'project '(omplex)
     (lambda (x)
       (make-scheme-number (real-part x))))
; add in scheme-number package
(put 'project '(scheme-number)
     (lambda (x)
       (let ((y (inexact->exact x)))
	 (cond ((rational? y) (make-rational (numerator y) (denominator y)))
	       ((integer? y) (make-rational y 1))
	       (else (make-rational (truncate y) 1))))))
; add to rational package	       
(put 'project '(rational)
     (lambda (x)
       (make-integer (truncate (/ (numer x) (denom x))))))
(define (drop x)
  (let ((proj (get 'project (type-tag x))))
    (if proj
	(if (equ? x (raise (proj (contents x))))
	    (drop (proj (contents x)))
	    x)
	x)))
; the idea for modifying procedure 2.84 is to simplify the
; result of applying op to transformed or non-transformed
; args contents. However, drop is only defined for one
; argument, so we must apply drop or simplify only if the
; result of applying op is a single value.

; Change 2.84 (apply proc (map contents args)) to
;
(if (list? (apply proc (map contents args)))
    (apply proc (map contents args))
    (drop (apply (proc ( map contents args)))))
; Similarly for the line containing (apply proc-top-type (map contents args))
