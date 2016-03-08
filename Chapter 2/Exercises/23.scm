#lang scheme
(define (o-for-each f l)
  (cond ((null? l) #t)
        (else (f (car l))
              (o-for-each f (cdr l)))))