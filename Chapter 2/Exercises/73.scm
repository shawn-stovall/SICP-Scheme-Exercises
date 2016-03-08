#lang scheme
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum)
  ;; internal definitions
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (make-sum x y)
    (cons x y))
  ;; external interface
  (define (tag x) (attach-tag '+))
  (put 'first-op '(+) addend)
  (put 'second-op '(+) augend)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (install-product)
  ;; internal definitions
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define (make-product x y) (cons x y))
  ;; external interface
  (define (tag x) (attach-tag '*))
  (put 'first-op '(*) multiplier)
  (put 'second-op '(*) multiplicand)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

(define (install-exponentiation-package)
  ;; internal definitions
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (make-exponentiation x y)
    (cons x y))
  ;; external interface
  (define (tag x) (attach-tag '**))
  (put 'first-op '(**) base)
  (put 'second-op '(**) exponent)
  (put 'make-exponentiation '**
       (lambda (x y) (tag (make-exponentiation x y))))
  'done)

(define (install-deriv)
  ;; internal definitions
  (define (deriv-sum exp var)
    (let ((make-sum (get 'make-sum '+))
          (addend (get 'first-op '(+)))
          (augend (get 'second-op '(+))))
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))))
  (define (deriv-product exp var)
    (let ((make-sum (get 'make-sum '+))
          (make-product (get 'make-product '*))
          (multiplier (get 'first-op '(*)))
          (multiplicand (get 'second-op '(*))))
      (make-sum
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp)))))
  (define (deriv-exponentiation exp var)
    (let ((make-product (get 'make-product '*))
          (make-exponentiation (get 'make-exponentiation '**))
          (base 'first-op '(**))
          (exponent 'second-op '(**)))
      (make-product
       (make-product (exponent exp)
                     (make-exponentiation (base expt) (- (exponent exp) 1)))
       (deriv (base exp)))))
  ;; external interface
  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-product)
  (put 'deriv '(**) deriv-exponentiation)
  'done)