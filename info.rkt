#lang info

; (define version "1.0")

(define deps (list "db"))

(define scribblings (list (list "doc/db-define-query.rkt"
                                '()
                                '(library))))

(define collection "db-define-query")

(define test-include-paths (list "test/"))