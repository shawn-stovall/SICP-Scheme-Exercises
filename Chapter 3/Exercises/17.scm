(define (member-of? pair pair-list)
  (if (null? pair-list)
      #f
      (cond ((eq? pair (car pair-list)) #t)
	    (else (member-of? pair (cdr pair-list))))))

(define (count-pairs x)
  (let ((seen '()))
    (define (count x)
      (if (not (pair? x))
	  0
	  (cond ((not (member-of? x seen))
		 (set! seen (cons x seen))
		 (+ (count (car x))
		    (count (cdr x))
		    1))
		(else 0))))
    (count x)))
