#lang info

; (define version "1.0")

(define deps (list "db"))

(define scribblings (list (list "define-query-doc/db-define-query.scrbl"
                                '()
                                '(library))))

(define collection "db-define-query")

(define test-include-paths (list "define-query-test/"))