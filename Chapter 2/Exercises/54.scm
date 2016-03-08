#lang scheme
(define (equal? a b)
  (cond ((and (null? a)
              (null? b)) #t)
        ((or (null? a)
             (null? b)) #f)
        ((and (pair? (car a))
              (pair? (car b))) (and (equal? (car a) (car b))
                                    (equal? (cdr a) (cdr b))))
        ((or (pair? (car a))
             (pair? (car b))) #f)
        (else (and (eq? (car a) (car b))
                   (equal? (cdr a) (cdr b))))))