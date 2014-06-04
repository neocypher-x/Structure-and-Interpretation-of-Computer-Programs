; 3.1
(define (make-accumulator start)
  (let ((total start))
    (lambda (x)
      (begin (set! total (+ total x))
	     total))))
; exploiting lexical closure:
(define (make-accumulator start)
  (lambda (x)
    (begin (set! start (+ start x))
	   start)))
(define A (make-accumulator 5))
(A 10)

; 3.2
(define (make-monitored f)
  (let ((counter 0))
    (define (mf x)
      (cond ((eq? x 'how-many-calls?) counter)
	    ((eq? x 'reset-count) (set! counter 0))
	    (else (set! counter (+ counter 1)) (f x))))
    mf))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-counter)

; 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) (lambda (x) "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
    dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

; 3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops amount)
    "Calling the cops.")
  (let ((incorrect-password-attempts 0))
    (define (dispatch pass m)
      (if (eq? pass password)
	  (begin
	    (set! incorrect-password-attempts 0)
	    (cond ((eq? m 'withdraw)  withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
			       m))))
	  (begin
	    (set! incorrect-password-attempts (+ incorrect-password-attempts 1))
	    (if (> incorrect-password-attempts 7)
		call-the-cops
		(lambda (x) "Incorrect password")))))
    dispatch))
; In the above implementation, dispatch is responsible for verifying the password as well as
; keeping track of incorrect password attempts. This way, none of the actual procedures such as
; withdraw and deposit need to know about password attempts.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops amount)
    "Calling the cops.")
  (let ((incorrect-password-attempts 0))
    (define (password-verifier attempt)
      (if (eq? attempt password)
	  (begin (set! incorrect-password-attempts 0)
		 #t)
	  (begin
	    (set! incorrect-password-attempts (+ incorrect-password-attempts 1))
	    (if (> incorrect-password-attempts 7)
		call-the-cops
		(lambda (x) "Incorrect password")))))
    (define (dispatch pass m)
      (let ((password-check (password-verifier pass)))
	(if (eq? password-check #t)
	    (cond ((eq? m 'withdraw)  withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
			       m)))
	    password-check)))
    dispatch))
; In the above implementation, all the password-checking is done by password-verifier. This frees
; up dispatch to call password-verifier and then proceed to either dispatch or return the result of
; password-verifier.

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; 3.5
(define (predicate x y)
  (<= (+ (square x) (square y)) 3))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials experiment))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
