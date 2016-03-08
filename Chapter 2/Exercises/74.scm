#lang scheme
(define (get-record file)
  (let ((div (type-tag file)))
    (let ((retrieve-record (get 'record div)))
      (if retrieve-record
          (retrieve-record file)
          (error
           "No division of that name exists -- GET-RECORD" div)))))

(define (get-salary file)
  (let ((record (get-record file))
        (div (type-tag file)))
    (let ((retrieve-salary (get 'salary div)))
      (if retrieve-salary
          (retrieve-salary record)
          (error
           "No division of that name exists -- GET-RECORD" div)))))

(define (find-employee-record name files)
  (let ((div (type-tag files)))
    (let ((cur (get 'current div))
          (next (get 'next div))
          (get-name (get 'name div))
          (empty? (get 'emppred div))
      (define (recur n fs)
        (if get-name
            (cond ((empty? fs) #f)
                  ((eq? (get-name (cur fs)) name)
                   (get-record (cur fs)))
                  (recur n (next fs)))
            (error
             "No division of that name exists -- GET-RECORD" div))))))
  (recur n fs))