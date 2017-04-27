#lang racket

(module+ test
  (require db-define-query
           db/base
           db/sqlite3)

  (define the-db (sqlite3-connect #:database 'temporary))

  (current-db-connection the-db)
  
  (require rackunit)

   
  ;(define sql.3 "SELECT * FROM FOO WHERE x = $1 AND y = $2")

  
  ;(define-query-row (test2 a (b 42)) sql.2)

  ;test2:sql

  ;test2:get-prepared-statement
  
  ;

  (require rackunit/text-ui)

  (run-tests
   (test-suite
    "db-define-query"

    (test-suite
     "Very simple SQL (no table needed)"
     (let* ((sql.1 "SELECT 1"))
       (define-query-suite (test1) sql.1)
       (check-equal? test1.exec:sql sql.1)
  
       (check-pred procedure? test1.exec:get-prepared-statement)
       (check-pred prepared-statement? (test1.value:get-prepared-statement) "Preparation")
       (check-equal? (test1.value) 1)
       (check-match (map (λ (x) (x)) test1.suite)
                    (list (? void?)
                          (list (vector 1))
                          (list 1)
                          (vector 1)
                          (vector 1)
                          1
                          1))))

    (test-suite
     "Very simple SQL (no table needed) -maybe-... functions fail"
     (let ()
       (define-query-suite (test1) "SELECT 1 WHERE 1=0")
       (check-pred string? test1.exec:sql)
       (check-pred procedure? test1.exec:get-prepared-statement)
       (check-pred prepared-statement? (test1.value:get-prepared-statement) "Preparation")
       (check-exn exn:fail? (λ () (test1.value) 1))
       (check-match
        (map (λ (x) (with-handlers ([exn:fail? exn-message]) (x))) test1.suite)
        (list
         (? void?)
         (list)
         (list)
         (regexp #rx"query-row: query returned wrong number of rows\n  statement: \"[^\"]*\"\n  expected: 1\n  got: 0")
         #f
         (regexp #rx"query-value: query returned wrong number of rows\n  statement: \"[^\"]*\"\n  expected: 1\n  got: 0")
         #f))))

    (test-suite
     "EXEC: table creation and population"
     (check-not-exn
      (λ ()
        (define-query-exec (create-FOO) "CREATE TABLE FOO (x INTEGER, y INTEGER)")
        (create-FOO))
      "Table creation")

     (check-not-exn
      (λ ()
        (define-query-exec (populate-FOO)
          #<<$
INSERT INTO FOO (x, y) VALUES (null, 42), (17, 42), (1, 2), (1, 4), (2, 4)
$
          )
        populate-FOO:sql
        (populate-FOO))
      "Table population"))
    
    (test-suite
     "Arguments Handling"
     (let ((sql "SELECT * FROM FOO WHERE x = $1 AND y = $2"))
       (define-query-maybe-value (test2 x (y 42)) sql)
       
       (define-query-rows (test3 (a -> values) ((b -> false->sql-null) 42)) sql)
 
       (check-match (test2:test-args 1 2) '(1 2))
  
       (check-match (test2:test-args 1) '(1 42))
  
       (check-match (test2:test-args #f #f)
                    (list (? sql-null?) (? sql-null?)))
  
       (check-exn exn:fail:contract:arity? (λ () (test2:test-args)))

       (check-match (test3:test-args #f #f) (list #f (? sql-null?)))))

    ;; TODO: Use an executor to test no connection leakage in prepared-connections#



    )))