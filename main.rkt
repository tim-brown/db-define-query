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
         syntax/parse
         syntax/parse/define
         racket/syntax
         racket/promise
         racket/splicing
         (for-syntax racket/syntax
                     racket/base
                     syntax/srcloc
                     "private/sub-range-binding-utils.rkt"))

(define current-db-connection (make-parameter connection?))


(define-for-syntax base-name->prepared-sql-id (extend-base-name ":sql"))

(define-for-syntax base-name->prepared-query-get-prepared-statement-id
  (extend-base-name ":get-prepared-statement"))

(define-for-syntax base-name->prepared-args-tester-id (extend-base-name ":test-args"))

(begin-for-syntax
  (define-splicing-syntax-class qry-xxx-arg
    #:datum-literals (->)
    (pattern name:id #:attr f #'false->sql-null)
    (pattern (name:id -> f:expr)))

  (define-splicing-syntax-class qry-xxx-opt-arg
    (pattern (~seq (~optional kw:keyword) (a:qry-xxx-arg default:expr))
             #:with name #'a.name
             #:with f #'a.f))

  (define (arg-decl-parser stx)
    (syntax-parse stx
      [(arg:qry-xxx-arg) #'arg.name]))

  (define (arg-value-parser stx)
    (syntax-parse stx
      [(arg:qry-xxx-arg) #'(arg.f arg.name)]))

  (define (opt-arg-decl-parser stx)
    (syntax-parse stx
      [(arg:qry-xxx-opt-arg)
       (if (attribute arg.kw)
           #'(arg.kw (arg.name arg.default))
           #'((arg.name arg.default)))]))

  (define (opt-arg-value-parser stx)
    (syntax-parse stx
      [(arg:qry-xxx-opt-arg)
       #'(arg.f arg.name)])))

 (define-syntax (define-query-xxx stx)  
  (syntax-parse
      stx
    [(_ query-xxx (base-name:id arg:qry-xxx-arg ... opt-arg:qry-xxx-opt-arg ...)
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
           (base-name->prepared-args-tester-id #'base-name #:true-base-name #'tbn)]

          [(arg-decls ...) (map arg-decl-parser (syntax->list #'(arg ...)))]
          [(arg-values ...) (map arg-value-parser (syntax->list #'(arg ...)))]
          
          [(opt-arg-decls ...)
           (let ((l (syntax->list #'(opt-arg ...))))
             (if (null? l) #'()
                 (let ((m (map opt-arg-decl-parser l)))
                   (datum->syntax (car m)
                                  (apply append (map syntax-e m))))))]
          [(opt-arg-values ...)
           (map opt-arg-value-parser (syntax->list #'(opt-arg ...)))])

       #'(begin
           (splicing-let ((prepared-connections# (make-weak-hash)))
             (define (prepared-query-sql-id) sql)
             
             (define (get-prepared-statement-id (conn (current-db-connection)))
               (cond [(let ((prep (hash-ref prepared-connections# conn #f)))
                        (and (connected? conn) prep)) => values]
                     [else (let ((prep (prepare conn sql)))
                             (hash-set! prepared-connections# conn prep)
                             prep)]))
           
             (define (base-name arg-decls ... opt-arg-decls ...
                                #:connection (conn (current-db-connection))
                                #:xform-result (xform-result values))
               (xform-result
                (query-xxx conn (get-prepared-statement-id conn)
                           arg-values ... opt-arg-values ...)))
             (define (prepared-query-args-test-id arg-decls ... opt-arg-decls ...)
               (list  arg-values ... opt-arg-values ...)))))]))

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
    [(_ (base-name:id arg:qry-xxx-arg ... opt-arg:qry-xxx-opt-arg ...) sql:expr)
     (define (ebn sfx) ((extend-base-name sfx) #:true-base-name #'base-name #'base-name))
     (with-syntax
         ([base-name.exec        (ebn ".exec")]
          [base-name.rows        (ebn ".rows")]
          [base-name.list        (ebn ".list")]
          [base-name.row         (ebn ".row")]
          [base-name.maybe-row   (ebn ".maybe-row")]
          [base-name.value       (ebn ".value")]
          [base-name.maybe-value (ebn ".maybe-value")]
          [base-name.suite       (ebn ".suite")]

          [(arg-decls ...) (map arg-decl-parser (syntax->list #'(arg ...)))]
          [(opt-arg-decls ...)
           (let ((l (syntax->list #'(opt-arg ...))))
             (if (null? l) #'()
                 (let ((m (map opt-arg-decl-parser l)))
                   (datum->syntax (car m)
                                  (apply append (map syntax-e m))))))])
       #'(begin
           (define-query-exec (base-name.exec arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-rows (base-name.rows arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-list (base-name.list arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-row (base-name.row arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-maybe-row (base-name.maybe-row arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-value (base-name.value arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define-query-maybe-value (base-name.maybe-value arg-decls ... opt-arg-decls ...)
             #:true-base-name base-name sql)

           (define base-name.suite (list base-name.rows
                                         base-name.list
                                         base-name.row
                                         base-name.maybe-row
                                         base-name.value
                                         base-name.maybe-value
                                         base-name.exec))))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (define ((expand-syntax-nce n) s)
    (if (positive? n) ((expand-syntax-nce (sub1 n)) (expand-syntax-once s)) s))

  (define expand-syntax-twice (expand-syntax-nce 2))

  (define expand-syntax-thrice (expand-syntax-nce 3))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q) "SELECT 1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args) (list)))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q x) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args x) (list (false->sql-null x))))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q (x -> values)) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args x) (list (values x))))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q (x 2)) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args (x 2)) (list (false->sql-null x))))))

    (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q (x 2) (y 4)) "SELECT 1 WHERE x = $1 AND y = $2")))
   `(begin (splicing-let ,_ ... (define (Q:test-args (x 2) (y 4)) (list (false->sql-null x)
                                                                        (false->sql-null y))))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q ((x -> values) 2)) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args (x 2)) (list (values x))))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q #:x (x 2)) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args #:x (x 2)) (list (false->sql-null x))))))

  (check-match
   (syntax->datum (expand-syntax-twice
                   #'(define-query-row (Q #:x ((x -> values) 2)) "SELECT 1 WHERE x = $1")))
   `(begin (splicing-let ,_ ... (define (Q:test-args #:x (x 2)) (list (values x))))))


  (define qs (syntax->datum (expand-syntax-twice
                   #'(define-query-suite (Q #:x ((x -> values) 2)) "SELECT 1 WHERE x = $1"))))
  qs
  (check-match
   qs
   `(begin ,_ ...
            (define Q.suite (list Q.rows
                                  Q.list
                                  Q.row
                                  Q.maybe-row
                                  Q.value
                                  Q.maybe-value
                                  Q.exec)))))

