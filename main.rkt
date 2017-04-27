#lang racket/base

(provide current-db-connection
         define-query-exec
         define-query-rows
         define-query-list
         define-query-row
         define-query-maybe-row
         define-query-value
         define-query-maybe-value
         define-query-suite)

(require db
         (for-syntax "sub-range-binding-utils.rkt")
         syntax/parse
         syntax/parse/define
         racket/syntax
         racket/promise
         racket/splicing
         (for-syntax racket/syntax
                     racket/base
                     syntax/srcloc))

(define current-db-connection (make-parameter connection?))


(define-for-syntax base-name->prepared-sql-id (extend-base-name ":sql"))

(define-for-syntax base-name->prepared-query-get-prepared-statement-id
  (extend-base-name ":get-prepared-statement"))

(define-for-syntax base-name->prepared-args-tester-id (extend-base-name ":test-args"))

(begin-for-syntax
 (define-syntax-class qry-xxx-arg
  #:datum-literals (->)
  (pattern a:id #:attr f #'false->sql-null)
  (pattern (a:id -> f:expr))))

(define-syntax (define-query-xxx stx)  
  (syntax-parse
      stx
    [(_ query-xxx (base-name:id arg:qry-xxx-arg ... (opt-arg:qry-xxx-arg default:expr) ...)
        (~optional (~seq #:true-base-name true-base-name))
        sql:expr)
     (with-syntax*
         ([tbn (if (attribute true-base-name) #'true-base-name #'base-name)]
          [prepared-query-sql-id
           (add-sub-range-binding-vector
            (base-name->prepared-sql-id #'base-name #:true-base-name #'tbn
                                        #:len->span1 (λ (len1 len2) len2))
            #'sql
            #:len->start1 (λ (len1 len2) (- len1 4))
            #:len->span1 (λ (len1 len2) 4)
            #:frac-x1 0.5
            #:frac-y1 0.5
            #:len->start2 (λ (len1 len2) 0)
            #:len->span2 (λ (len1 len2) len2)
            #:frac-x2 0.5
            #:frac-y2 0.5)]
          [get-prepared-statement-id
           (base-name->prepared-query-get-prepared-statement-id #'base-name
                                                                #:true-base-name #'tbn)]
          [prepared-query-args-test-id
           (base-name->prepared-args-tester-id #'base-name #:true-base-name #'tbn)])

       #'(begin
           (splicing-let ((prepared-connections# (make-weak-hash)))
             (define (prepared-query-sql-id) sql)
           
             (define get-prepared-statement-id
               (λ () (let ((conn (current-db-connection)))
                       (cond [(let ((prep (hash-ref prepared-connections# conn #f)))
                                (and (connected? conn) prep))
                              => values]
                             [else (let ((prep (prepare conn sql)))
                                     (hash-set! prepared-connections# conn prep)
                                     prep)]))))
           
             (define (base-name arg.a ...
                                (opt-arg.a default) ...
                                #:xform-result (xform-result values))
               (xform-result
                (query-xxx (current-db-connection)
                           (get-prepared-statement-id)
                           (arg.f arg.a) ...
                           (opt-arg.f opt-arg.a) ...)))
           
             (define (prepared-query-args-test-id arg.a ... (opt-arg.a default) ...)
               (list  (arg.f arg.a) ... (opt-arg.f opt-arg.a) ...)))))]))


(define-syntax-rule (define-query-exec x ...)
  (define-query-xxx query-exec x ...))

(define-syntax-rule (define-query-rows x ...)
  (define-query-xxx query-rows x ...))

(define-syntax-rule (define-query-list x ...)
  (define-query-xxx query-list x ...))

(define-syntax-rule (define-query-row x ...)
  (define-query-xxx query-row x ...))

(define-syntax-rule (define-query-maybe-row x ...)
  (define-query-xxx query-maybe-row x ...))

(define-syntax-rule (define-query-value x ...)
  (define-query-xxx query-value x ...))

(define-syntax-rule (define-query-maybe-value x ...)
  (define-query-xxx query-maybe-value x ...))

(define-syntax (define-query-suite stx)
  (syntax-parse stx
    [(_ (base-name:id arg:qry-xxx-arg ... (opt-arg:qry-xxx-arg default:expr) ...) sql:expr)
     (define (ebn sfx) ((extend-base-name sfx) #:true-base-name #'base-name #'base-name))
     (with-syntax
         ([base-name.exec        (ebn ".exec")]
          [base-name.rows        (ebn ".rows")]
          [base-name.list        (ebn ".list")]
          [base-name.row         (ebn ".row")]
          [base-name.maybe-row   (ebn ".maybe-row")]
          [base-name.value       (ebn ".value")]
          [base-name.maybe-value (ebn ".maybe-value")]
          [base-name.suite       (ebn ".suite")])
       #'(begin
           (define-query-exec (base-name.exec arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-rows (base-name.rows arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-list (base-name.list arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-row (base-name.row arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-maybe-row (base-name.maybe-row arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-value (base-name.value arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define-query-maybe-value (base-name.maybe-value arg ... (opt-arg ... default) ...)
             #:true-base-name base-name sql)

           (define base-name.suite (list base-name.exec
                                         base-name.rows
                                         base-name.list
                                         base-name.row
                                         base-name.maybe-row
                                         base-name.value
                                         base-name.maybe-value))))]))