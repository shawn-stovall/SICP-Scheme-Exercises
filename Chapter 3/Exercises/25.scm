(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (make-table)
  (list '*table*))

(define (lookup keys table)
  (let ((subtable (assoc (car keys) (cdr table))))
    (if subtable
	(if (null? (cdr keys))
	    (cdr subtable)
	    (lookup (cdr keys) subtable))
	false)))

(define (insert! keys value table)
  (let ((subtable (assoc (car keys) (cdr table))))
    (if subtable
	(if (null? (cdr keys))
	    (set-cdr! subtable value)
	    (insert! (cdr keys) value subtable))
	(if (null? (cdr keys))
	    (set-cdr! table
		      (cons (cons (car keys) value)
			    (cdr table)))
	    (begin (set-cdr! table
			     (cons (cons (car keys) '())
				   (cdr table)))
		   (insert! keys value table))))))
