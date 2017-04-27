#lang scribble/manual

@title{db-define-query}
@author["Tim Brown"]

@defmodule[db-define-query]

@(require (for-label racket/base db-define-query db))

@(begin (require scribble/eval)
        (define ev (make-base-eval))
        (ev '(require db-define-query db racket/port)))

This library provides macros that produce functions for querying SQL database
connections (see @other-doc['(lib "db/scribblings/db.scrbl")]). It is intended
to help developers of software which relies on large numbers of database queries.

@section{Introduction}

When an application depends on a database of any complexity, a need arises to
manage the SQL, @secref["Prepared_Statements" #:doc '(lib "db/scribblings/db.scrbl")]
and database connections. Since each prepared statement is bound to a connection,
then managing their life cycles in the face of unreliable or pooled connections
becomes necessary.

This library attempts to relieve some of the burden associated with prepared statements
by allowing you to define functions from SQL that satisfy these needs.

@section{Overview of the @racket[define-query-...] Macros}

The @racket[define-query-...] forms follow the description of @racket[define-query-exec]
below. The difference between them is the query function that they call. These are
all documented in @secref["Simple_Queries" #:doc '(lib "db/scribblings/db.scrbl")].

@defform[#:literals (->)
         (define-query-exec
           (id arg ... (opt-arg default) ...)
           sql)
         #:grammar [(arg maybe-transformed-arg)
                    (opt-arg maybe-transformed arg)
                    (maybe-transformed-arg name (name -> racket->sql-transformer))]
         #:contracts ([sql string?]
                      [default any/c]
                      [racket->sql-transformer (-> any/c any/c)])]{
  The @racket[define-query-exec] syntax takes @var[sql]
  and defines the following names based on @var[id]:
  
  @itemlist[
            @item{@racket[id] a function of the form
                   @(racketblock (id arg ... (opt-arg default) ...
                                     #:xform-result (xform-result values)))
                   which takes at least one argument for each of the compulsory arguments @var[arg],
                   and additional optional arguments for each of @var[opt-arg]. If an optional
   argument is not provided, its @var[default] value is used.

                   Each of the arguments is transformed with @var[racket->sql-transformed], which
   defaults to @racketlink[false->sql-null @racket[false->sql-null]]. @var[default] values are also
   transformed.

   When @racket[id] is defined with @racket[define-query-exec]; it returns the same type as
   @racketlink[query-exec @racket[query-exec]], i.e. it is @racket[void?].
   The return value for @racket[define-query-rows] would be the same as for
   @racketlink[query-rows @racket[query-rows]], i.e. @racket[(listof vector?)]. Et cetera.

   The output of the function is transformed by the function @racket[xform-result]. This takes
   values of the return type of @racket[query-...] and transforms them to any other type.
   @racket[xform-result] defaults to @racket[values].}
             
            @item{@racket[id]@racketidfont{:sql}, a thunk that returns the current value of @var[sql]
  (see notes on @var[sql] below)}
            
            @item{@racket[id]@racketidfont{:get-prepared-statement},
   returns the prepared statement for
   @racketlink[current-db-connection (racket (current-db-connection))]
   from the statement cache.
  If no statement has been prepared yet, @racketlink[prepare (racket prepare)] is called on
  the current value of @var[sql]; and this is cached against @racket[current-db-connection].}
            @item{@racket[id]@racketidfont{:test-args} a function with the same input contract as
   @racket[id], which returns the arguments as they would be passed to @racket[query-exec].
   Use this for testing @var[racket->sql-transformer] and @var[default] behaviour.}
            ]}

@defparam[current-db-connection conn connection? #:value #f]{
 A parameter that defines the database connection to be used when invoking
 a query defined with this library. Prepared statements are internally cached
 in a weak hash keyed on these connections.

 @racket[conn] cannot be a @emph{virtual connection}
 (see @secref["Virtual_Connections" #:doc '(lib "db/scribblings/db.scrbl")]).
}

@subsection{About @var[sql]}

@var[sql] is an expression, which may be variable. When a statement is prepared, the current
value of @var[sql] is captured into the prepared statement; and remains captured as the query
that will be used whenever @racketlink[current-db-connection (racket (current-db-connection))]
is used. The value of @var[sql] that will be used if immediately prepared can be determined by
calling @racket[id]@racketidfont{:sql}. Of course, if @var[sql] is constant you don't need to
worry about any of this.

@subsection{Prepared Statement Templates}

Note that ``the syntax of placeholders varies depending on the database system''
(@secref["query-statements" #:doc '(lib "db/scribblings/db.scrbl")]). This library
does not address issues of portability between database systems.

@subsection{Database Types}

The types of parameters and return values may vary between database systems. Please
refer to @other-doc['(lib "db/scribblings/db.scrbl")] for system specific issues.

@subsection{The Remaining @racket[define-query-...] Forms}

@defform[(define-query-rows (id arg ... (opt-arg default) ...) sql)]{
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-rows (racket query-rows)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@defform[(define-query-list (id arg ... (opt-arg default) ...) sql)]{        
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-list (racket query-list)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@defform[(define-query-row (id arg ... (opt-arg default) ...) sql)]{
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-row (racket query-row)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@defform[(define-query-maybe-row (id arg ... (opt-arg default) ...) sql)]{
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-maybe-row (racket query-maybe-row)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@defform[(define-query-value (id arg ... (opt-arg default) ...) sql)]{
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-value (racket query-value)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@defform[(define-query-maybe-value (id arg ... (opt-arg default) ...) sql)]{
 As @racketlink[define-query-exec (racket define-query-exec)], but
 @racket[id] is bound to a function that calls
 @racketlink[query-maybe-value (racket query-maybe-value)], instead of @racket[query-exec].
                                                                 
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@section{Utilities}

@defform[(define-query-suite (id arg ... (opt-arg default) ...) sql)]{
 @racket[define-query-suite] defines a suite of query functions
 equvalent to calling:
 @(racketblock
   (define-query-exec (#,(var id).exec ...) #,(var sql))
   (define-query-list (#,(var id).list ...) #,(var sql))
   (define-query-rows (#,(var id).rows ...) #,(var sql))
   (define-query-row (#,(var id).row ...) #,(var sql))
   (define-query-maybe-row (#,(var id).maybe-row ...) #,(var sql))
   (define-query-value (#,(var id).value ...) #,(var sql))
   (define-query-maybe-value (#,(var id).maybe-value ...) #,(var sql)))

 In addition, a new name is defined: @racket[id]@racketidfont{.suite} containing:
 @racketblock[(list #,(var id).exec
                    #,(var id).list
                    #,(var id).rows
                    #,(var id).row
                    #,(var id).maybe-row
                    #,(var id).value
                    #,(var id).maybe-value)]
                                                                  
 See @racketlink[define-query-exec (racket define-query-exec)]
 for a description of the syntax.
}

@section{Examples}

We'll set up a little test bed first:

@examples[#:eval ev
          (define conn (sqlite3-connect #:database 'memory))
          (current-db-connection conn)
          (query-exec conn "CREATE TABLE FOO (x INTEGER, y INTEGER)")
          (query-exec
           conn
           "INSERT INTO FOO (x, y) VALUES (null, 42), (17, 42), (1, 2), (1, 4), (2, 4)")

           (define-query-maybe-row (foo/x&y x y)
             "SELECT * FROM FOO WHERE x = $1 AND y = $2")

          (foo/x&y:sql)

          (foo/x&y 1 2)
          (foo/x&y 100 200)
          ]

@close-eval[ev]
