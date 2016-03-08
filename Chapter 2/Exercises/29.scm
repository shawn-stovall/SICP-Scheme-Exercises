#lang scheme
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))

(define (balanced? mobile)
  (and (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))
       (= (* (branch-length (left-branch mobile))
             (branch-weight (left-branch mobile)))
          (* (branch-length (right-branch mobile))
             (branch-weight (right-branch mobile))))))

(define (branch-balanced? branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        #t
        (balanced? structure))))
    

(define mobile (make-mobile (make-branch 4
                                         (make-mobile (make-branch 5 2)
                                                      (make-branch 3 3)))
                            (make-branch 2 4)))