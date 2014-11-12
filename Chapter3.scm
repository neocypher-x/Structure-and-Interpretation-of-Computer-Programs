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
; In the above implementation, dispatch is responsible for 
; verifying the password as well as keeping track of 
; incorrect password attempts. This way, none of the actual 
; procedures such as withdraw and deposit need to know about 
;password attempts.

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
; In the above implementation, all the password-checking is done by 
; password-verifier. This frees up dispatch to call 
; password-verifier and then proceed to either dispatch or return 
; the result of password-verifier.

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
  (<= (+ (square x) (square y)) 1))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials experiment))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; the rectangular region has corners in each quadrant at plus or 
; minus unity. THe unit circle is centerd at the origin and 
; touches each axis at plus or minus unity. The total area of the 
; rectangle is 4 units squared. The total area of the unit circle 
; is pi. Hence, our experiment will approximate pi/4. Therefore, 
; to estimate pi we must multiply the proportion of our 
; monte-carlo experiment successes with four.
(* 4.0 (estimate-integral predicate -1.0 1.0 -1.0 1.0 10000000))
; evaluating the above gives 3.1417596

; 3.6
; for the purposes of testing, choose random-init to be 0, and rand-update as an incrementer
(define random-init 0)
(define (rand-update x)
  (+ x 1))
(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? message 'reset)
	     (lambda (new-value) (begin (set! x new-value)
					x)))))
    dispatch))

; 3.7
; Using make-account from exercise 3.3 (unchanged):
(define (make-joint base-account base-password password)
  (lambda (newpass cmd)
    (cond ((not (eq? password newpass)) (lambda (x) "Wrong joint password"))
	  (else (base-account base-password cmd)))))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
(define hacker-acc
  (make-joint peter-acc 'wrongpass 'hackerpass))
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 10)
; the hacker should fail at attempting to access peter's account
((hacker-acc 'hackerpass 'withdraw) 9000)

; 3.8
(define f
  (let ((a 0))
    (lambda (x)
      (let ((b a))
	(set! a (+ x a))
	b))))

; 3.9
;
;        parameters: n
;        body:       (if (= n 1)
;                    1
;                    (* n factorial (- n 1)))
;                          ^
;                         _|_   ___
;                        / . \ / . \__
;                        \___/ \___/  |
;                            ^        |
;              ______________|________v____________________________
; global      |              |                                     |
; env  ---->  |factorial:____|                                     |
;             |____________________________________________________|
;               ^               ^                              ^
;              _|_             _|_                            _|_
;         E1->|n:6|       E2->|n:5|            ...       E6->|n:1|
;             |___|           |___|                          |___|
;
;      (if (= n 1)             ...             ...            ...
;          1
;          (* n factorial (- n 1)))     



;
;        parameters: n                  parameters: product, counter, max-count
;        body:       (fact-iter 1 1 n)  body:       (if (> counter max-count)
;                          ^                            product
;                          |                            (fact-iter (* counter product)
;                          |                          ^            (+ counter 1)
;                          |                          |            (max-count)))
;                         _|_   ___                  _|_   ___
;                        / . \ / . \__              / . \ / . \_     
;                        \___/ \___/  |             \___/ \___/ |
;                            ^        |                 ^       |
;              ______________|________v_________________|_______v__
; global      |              |                          |          |
; env  ---->  |factorial:____|                          |          |<--------------+
;             |fact-iter:_______________________________|          |               |
;             |____________________________________________________|               |
;               ^                   ^                    ^                         |
;              _|_             _____|_____          _____|_____               _____|_____
;         E1->|n:6|       E2->|product:1  |    E3->|product:1  |         E8->|product:720|
;             |___|           |count:1    |        |count:2    |    ...      |count:7    |
;                             |max-count:6|        |max-count:6|             |max-count:6|
;      (fact-iter 1 1 n)      |___________|        |___________|             |___________|
;     
;                           body of fact-iter    body of fact-iter  ...           ...
;                          
;                              
;

; 3.10
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

; using the transformation
; (let ((<var> <exp>)) <body) -> ((lambda (<var>) <body>) <exp)
; results in
(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance)
	   "Insufficient funds")))
   initial-amount))

;=================================================================
; evaluating (define W1 (make-withdraw 100)) gives the following:
;-----------------------------------------------------------------
;        parameters: initial-amount
;        body:       ((lambda (balance)
;                       (lambda (amount)
;                         (if (>= balance amount)
;        		      (begin (set! balance (- balance amount))
;				     balance)
;	         	      "Insufficient funds")))
;                     initial-amount)
;                              ^
;                             _|_   ___
;                            / . \ / . \__
;                            \___/ \___/  |
;                                ^        |
;              __________________|________v____________________________
; global      |                  |                                     |
; env  ---->  |make-withdraw:____|                                     |
;             |W1:---+                                                 |
;             |______|_________________________________________________|
;                    |                                   ^
;                    |                                   |
;                 ___v ___      ___________      ________|_________
;                / . \/ . \--->|balance:100|--->|initial-amount:100|
;                \___/\___/    |___________|    |__________________|
;                  |                 E2                  E1
;                  |
;                  v
;        parameters: amount
;        body:       (if (>= balance amount)
;                        (begin (set! balance (- balance amount))
;                               balance)
;                        "Insufficient funds")


;=====================================================================
; evaluating (W1 50) gives the following:
; (the only real change is the addition of a new environment
; containing the amount)
;--------------------------------------------------------------------
;
;              ________________________________________________________
; global      |                                                        |
; env  ---->  |make-withdraw: ...                                      |
;             |W1:---+                                                 |
;             |______|_________________________________________________|
;                    |                                   ^
;                    |                                   |
;                 ___v ___      _____E2____      ________|_________
;                / . \/ . \--->|balance:100|--->|initial-amount:100|
;                \___/\___/    |___________|    |__________________|
;                  |                 ^                   E1
;                  |            _____|_____
;                  |           |amount:50  |E3
;                  |           |___________|
;                  v
;        parameters: amount
;        body:       (if (>= balance amount)
;                         ---

;================================================================
; evaluating (define W2 (make-withdraw 100)) gives the following:
;----------------------------------------------------------------
;
;
;              ________________________________________________________
; global      |                                                        |
; env  ---->  |make-withdraw: ...                                      |
;        +----|W1:                                                     |
;        |    |W2:-----------------------+                             |
;        |    |__________________________|_____________________________|
;        |              ^                |                  ^
;        |     _________|________        |         _________|________
;        |  E1|initial-amount:100|       |      E3|initial-amount:100|
;        |    |__________________|       |        |__________________|
;        |              ^                |                  ^
;        |         _____|_____           |             _____|_____
;        |      E2|balance:50 |          |          E4|balance:100|
;        |        |___________|          |            |___________|
;        |              ^                |                  ^
;        |        ___  _|_            ___v ___              |
;        +-----> / . \/ . \          / . \/ . \_____________|
;                \___/\___/          \___/\___/
;                  |                   |
;                  |                   |
;                  |                   |
;                  v                   v
;        parameters: amount
;        body:       (if (>= balance amount)
;                        (begin (set! balance (- balance amount))
;                        ...

; 3.11
;
; (define acc (make-account 50))
;
;              ______________________________________________
; global      |                                              |
; env  ---->  |make-balance: ...                             |
;             |acc:--+                                       |
;             |______|_______________________________________|
;                    |                      
;                    |                      
;                 ___v ___      _____________
;                / . \/ . \--->|account:50   |
;                \___/\___/    |withdraw: ...|
;                  |           |deposit:  ...|
;                  |      E1-->|dispatch: ...|
;                  |           |_____________|
;                  |
;                  v
;    parameters: amount
;    body:       ...

; ((acc 'deposit') 40)
;
;              _____________________________________
; global      |                                     |
; env  ---->  |make-balance: ...                    |
;             |acc:--+                              |
;             |______|______________________________|
;                    |                ^     
;                    |                |     
;                 ___v ___      ______|______
;                / . \/ . \--->|account:90   |
;                \___/\___/    |withdraw: ...|
;                  |           |deposit:  ...|
;                  |      E1-->|dispatch: ...|
;                  |           |_____________|
;                  |               ^     ^
;                  v               |     |
;    parameters: amount            |     |
;    body:       ...               |     |
;                                  |     |
;             _____________________|     |
;            |                           |
;       _____|____                   ____|____
; E2-->|m:'deposit|            E3-->|amount:40|
;      |__________|                 |_________|
;    call to dispatch             call to deposit


; ((acc 'withdraw) 60)
;
;
;              _____________________________________
; global      |                                     |
; env  ---->  |make-balance: ...                    |
;             |acc:--+                              |
;             |______|______________________________|
;                    |                ^     
;                    |                |     
;                 ___v ___      ______|______
;                / . \/ . \--->|account:30   |
;                \___/\___/    |withdraw: ...|
;                  |           |deposit:  ...|
;                  |      E1-->|dispatch: ...|
;                  |           |_____________|
;                  |               ^     ^
;                  v               |     |
;    parameters: amount            |     |
;    body:       ...               |     |
;                                  |     |
;             _____________________|     |
;            |                           |
;       _____|_____                  ____|____
; E2-->|m:'withdraw|           E3-->|amount:60|
;      |___________|                |_________|
;    call to dispatch             call to withdraw

; 3.12
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x)
; (b)
;      _____________      ______ ______
; x-->|      |      |    |      |    //|
;     |  o   |   o--|--->|   o  |  //  |
;     |__|___|______|    |___|__|//____|
;        |                   |
;      _ v__               __v__
;     |  a  |             |  b  |
;     |_____|             |_____|
;      _____________      ______ ______
; y-->|      |      |    |      |    //|
;     |  o   |   o--|--->|   o  |  //  |
;     |__|___|______|    |___|__|//____|
;        |                   |
;      _ v__               __v__
;     |  c  |             |  d  |
;     |_____|             |_____|
;      _____________      ______ ______      ______ ______      ______ ______
; z-->|      |      |    |      |      |    |      |      |    |      |    //|
;     |  o   |   o--|--->|   o  |   o--|--->|  o   |   o--|--->|  o   |  //  |
;     |__|___|______|    |___|__|______|    |__|___|______|    |__|___|//____|
;        |                   |                 |                  |
;      _ v__               __v__             __v__              __v__
;     |  a  |             |  d  |           |  c  |            |  d  |
;     |_____|             |_____|           |_____|            |_____|

(define w (append! x y))
(cdr x)
; (b c d)
;      _____________      ______ ______
; x-->|      |      |    |      |      |
; w-->|  o   |   o--|--->|   o  |   o--|---+
;     |__|___|______|    |___|__|______|   |
;        |                   |             |
;      _ v__               __v__           |
;     |  a  |             |  b  |          |
;     |_____|             |_____|          |
;                                          |
;        +---------------------------------+
;        |
;      __v__________      ______ ______
; y-->|      |      |    |      |    //|
;     |  o   |   o--|--->|   o  |  //  |
;     |__|___|______|    |___|__|//____|
;        |                   |
;      _ v__               __v__
;     |  c  |             |  d  |
;     |_____|             |_____|

; 3.13
; this implementation of last-pair doesn't support improper lists
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(last-pair '(a b))
; does not work
(last-pair (cons 'a 'b))
(last-pair (cons 1 2))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; trying to compute (last-pair z) results in finite recursion.
;
;
;   ___________________________________________________________________
;  |                                                                   |
;  |    ____________      ______ _____      ______ _____      ______ __|__
;  +-->|      |     |    |      |     |    |      |     |    |      |  |  |
;  z-->|  o   |  o--|--->|   o  |  o--|--->|  o   |  o--|--->|  o   |  o  |
;      |__|___|_____|    |___|__|_____|    |__|___|_____|    |__|___|_____|
;         |                  |                |                 |
;       _ v__              __v__            __v__             __v__
;      |  a  |            |  d  |          |  c  |           |  d  |
;      |_____|            |_____|          |_____|           |_____|
