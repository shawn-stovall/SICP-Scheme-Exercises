#lang scheme
(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? (car l))) (cons (car l)
                                     (fringe (cdr l))))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

(define x (list (list 1 2) (list 3 4)))