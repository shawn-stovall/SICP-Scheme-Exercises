;;; To allow parallel execution of any number of thunks, for
;;; effect.  The values are discarded.

(define disallow-preempt-current-thread
  (access disallow-preempt-current-thread
	  (->environment '(runtime thread))))

(define allow-preempt-current-thread
  (access allow-preempt-current-thread
	  (->environment '(runtime thread))))

(define (kill-thread thread)
  (let ((event
	 (lambda ()
	   (exit-current-thread 'RIP))))
    (without-interrupts
     (lambda ()
       (case (thread-execution-state thread)
	 ((STOPPED) (restart-thread thread #t event))
	 ((DEAD) unspecific)
	 (else (signal-thread-event thread event)))))))


(define (parallel-execute . thunks)
  (let ((my-threads '()))
    (define (terminator)
      (without-interrupts
       (lambda ()
	 (for-each kill-thread my-threads)
	 (set! my-threads '())
	 unspecific)))
    (without-interrupts
     (lambda ()
       (set! my-threads
	     (map (lambda (thunk)
		    (let ((thread (create-thread #f thunk)))
		      (detach-thread thread)
		      thread))
		  thunks))
       unspecific))
    terminator))

#|
;;; IO system is not completely interlocked, so...

(define (try n)
  (parallel-execute
   (lambda ()
     (write-line 'hi)
     (let lp ((i 0))
       (if (< i 10000)
	   (lp (1+ i))))
     (write-line 'gjs))
   (lambda ()
     (write-line 'there)
     (let lp ((i 0))
       (if (< i n)
	   (lp (1+ i))))
     (write-line 'foo))))

(define foo (try 9188))
;Value foo

hi
there
foo
foo
gjs

(foo)
;No value
|#

(define (make-serializer)
  (let ((mutex (make-thread-mutex)))
    (define (serialized f)
      (define (serialized-f . args)
	(with-thread-mutex-locked mutex
				  (lambda ()
				    (apply f args))))
      serialized-f)
    serialized))

(define output-serialized (make-serializer))

(define write-line
  (output-serialized write-line))

(define display
  (output-serialized display))

(define write
  (output-serialized write))
#|
;;; This solves the IO interlock problem


(define (try n)
  (parallel-execute
   (lambda ()
     (write-line 'hi)
     (let lp ((i 0))
       (if (< i 10000)
	   (lp (1+ i))))
     (write-line 'gjs))
   (lambda ()
     (write-line 'there)
     (let lp ((i 0))
       (if (< i n)
	   (lp (1+ i))))
     (write-line 'foo))))

(define foo (try 9197))
;Value: foo

hi
there
gjs
foo

(define foo (try 9196))
;Value: foo

hi
there
foo
gjs
|#

(declare (usual-integrations))

;;; UNIFORM-RANDOM produces an inexact number x,    0 <= x < 1

#|
(define uniform-random
  (let* ((random-max (expt 2 23))
	 (frandom-max (exact->inexact random-max)))
    (lambda ()
      (/ (random random-max)
	 frandom-max))))
|#

(define (uniform-random) (random 1.))

(define (nonzero-uniform-random)
  (let ((x (uniform-random)))
    (if (= x 0.)
	(nonzero-uniform-random)
	x)))

;;; Given uniform random numbers, we can produce pairs of
;;; gaussian-distributed numbers, with zero mean and unit
;;; standard deviation, by the following trick:

(define 2pi (* 4.0 (atan 1.0)))

(define (gaussian-random-pair #!optional continue)
  ;; continue = (lambda (y1 y2) ...)
  (let ((continue (if (default-object? continue) cons continue))
	(x1 (uniform-random))
	(x2 (uniform-random)))
    (let ((r (sqrt (* -2.0 (log x1)))))
      (continue (* r (cos (* 2pi x2)))
		(* r (sin (* 2pi x2)))))))

(define (gaussian-random)
  (gaussian-random-pair (lambda (x y) x)))

(define (gaussian-random-list d)
  (let lp ((j d) (t '()))
    (if (fix:= j 0)
	t
	(gaussian-random-pair
	 (lambda (x1 x2)
	   (if (fix:= j 1)
	       (cons x1 t)
	       (lp (fix:- j 2) (cons x1 (cons x2 t)))))))))


;;; Makes a list of n 2-vectors of gaussian-distributed random numbers  

(define (gaussian-random-pairs n)
  (if (fix:= n 0) 
      '()
      (cons (gaussian-random-pair vector)
	    (gaussian-random-pairs (fix:- n 1)))))


;;; Makes a list of n d-vectors of gaussian-distributed random numbers  

(define (gaussian-random-tuples d n)
  (if (fix:= n 0) 
      '()
      (cons (list->vector (gaussian-random-list d))
	    (gaussian-random-tuples d (fix:- n 1)))))


;;; For adding zero-mean noise with a given standard deviation to a vector.

(define ((add-noise sigma) v)
  (list->vector (map (lambda (signal noise)
		       (+ signal (* sigma noise)))
		     (vector->list v)
		     (gaussian-random-list (vector-length v)))))

;; (define (make-account balance)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;; 	(begin (set! balance (- balance amount))
;; 	       balance)
;; 	"Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (let ((protected (make-serializer)))
;;     (define (dispatch m)
;;       (cond ((eq? m 'withdraw) (protected withdraw))
;; 	    ((eq? m 'deposit) (protected deposit))
;; 	    ((eq? m 'balance) balance)
;; 	    (else (error "Unknown request -- MAKE-ACCOUNT"
;; 			 m))))
;;     dispatch))

(define (make-account-and-serializaer balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
	(d (account 'deposit)))
    ((s d) amount)))

(define (withdraw account amount)
  (let ((s (account 'serializer))
	(w (account 'withdraw)))
    ((s w) amount)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
		       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)))
	    ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))

(define (make-semaphore n)
  (let ((c 1)
	(the-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (< c n)
		 (set! c (+ c 1))
		 (the-mutex 'acquire)))
	    ((eq? m 'release)
	     (if (< c n)
		 (set! c (- c 1))
		 (begin (the-mutex 'release)
			(set! c (- c 1)))))))
    the-semaphore))
