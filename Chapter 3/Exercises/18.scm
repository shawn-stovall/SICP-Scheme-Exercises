(define (infinity? list)
  (define (check l)
    (cond ((null? l) #f)
	  ((eq? l list) #t)
	  (else (check (cdr l)))))
  (check (cdr list)))
