#lang scribble/manual

@title{db-define-query}
@author["Tim Brown"]

@defmodule[db-define-query]

@(require (for-label racket/base db-define-query db))

@(begin (require scribble/eval)
        (define ev (make-base-eval))
        (ev '(require db-define-query db racket/port racket/match racket/pretty)))

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

The @racket[define-query-...] forms follow the description of @racket[define-query-rows]
below. The difference between them is the query function that they call. These are
all documented in @secref["Simple_Queries" #:doc '(lib "db/scribblings/db.scrbl")].

@defform[#:literals (->)
         (define-query-rows (id arg ... opt-arg ...) sql-expr)
         #:grammar [(arg maybe-transformed-arg)
                    (opt-arg (maybe-transformed-arg default-expr)
                             (code:line keyword (maybe-transformed-arg default-expr)))
                    (maybe-transformed-arg name (name -> transformer))]
         #:contracts ([sql-expr string?]
                      [default any/c]
                      [racket->sql-transformer (-> any/c any/c)])]{
  The @racket[define-query-rows] syntax takes @var[sql]
  and defines the following names based on @var[id].
  
 @(itemlist
   @item{
  @racket[id] a function of the form
  @(racketblock (id arg-name ...
                    opt-arg ...
                    #:connection (conn (current-db-connection))
                    #:xform-result (xform-result values)))

  This function creates prepared statement from @racket[sql-expr] against connection
  @racket[conn]. The template variables of the prepared statement are bound to
  @racket[arg ...] and @racket[opt-arg ...] in the order that they were declared.
  The prepared query is then run on database connection @racket[conn] using the function
  @racketlink[query-rows (racket query-rows)] (in this case).

  @racket[define-query-rows] tries to match the behaviour of @racketlink[define (racket define)];
  but with the following constraints / differences:

  @(itemlist
    @item{All keyword arguments are optional. The relationship between argument order and template
   variable order is preserved this way, hopefully keeping the behaviour more predicatble}
    @item{Keyword arguments can be interleaved with optional arguments. This is so that optional
   arguments can be skipped. Keyword arguments cannot be interleaved with required arguments.}
    @item{Curried definitions are not possible (i.e. no @bold{CVT}).
   Heads are simply @racketblock[(id args ...)]}
    @item{Arguments of the form @racket[(id -> transformer)], and optional arguments of the form
    @racket[((id -> transformer) default-expr)], are transformed by @racket[transformer]
   before being passed to @racket[query-rows] (or, more generally, the query function being used).
   The default @racket[transformer] is @racketlink[false->sql-null @racket[false->sql-null]].
   @emph{It is not @racket[values]}, be aware of this when dealing with boolean data types.
   @racket[transformer] is applied to the default values of optional arguments.})

  When @racket[id] is defined with @racket[define-query-rows]; it returns the same type as
  @racketlink[query-exec @racket[query-exec]], i.e. @racket[(listof vector?)].
  The return value for @racket[define-query-row] would be the same as for
  @racketlink[query-rows @racket[query-row]], i.e. @racket[vector?] etc.

  The output of the function is transformed by the function @racket[xform-result]. This takes
  values of the return type of @racket[query-...] and transforms them to any other type.
  @racket[xform-result] defaults to @racket[values].}
             
   @item{@racket[id]@racketidfont{:sql}, a thunk that returns the current value of @racket[sql-expr]
  (see notes on @var[sql] below)}
            
   @item{The function @racket[id]@racketidfont{:get-prepared-statement} takes one optional argument
                      @racket[conn] which defaults to
                      @racketlink[current-db-connection (racket (current-db-connection))].

                      It returns the prepared statement for @racket[conn] from the statement cache.
                      
  If no statement has been prepared yet, @racketlink[prepare (racket prepare)] is called on
  the current value of @var[sql-expr]; and this is cached against @racket[conn].}
   
   @item{@racket[id]@racketidfont{:test-args} a function with the same input contract as
  @racket[id], which returns the arguments as they would be passed to @racket[query-exec].
  Use this for testing @var[transformer] and @var[default] value behaviour.})

   @racket[sql-expr] is an expression, which may be variable. When a statement is prepared against
a connection @racket[conn], the current value of @racket[sql-expr] is captured into the prepared
statement; and remains captured as the query that will be used whenever @racket[conn] is used.

The value of @racket[sql-expr] that will be used if immediately prepared can be determined by
calling @racket[id]@racketidfont{:sql}. Of course, if @racket[sql-expr] is constant you don't
need to worry about any of this.}

@defparam[current-db-connection conn connection? #:value #f]{
 A parameter that defines the default database connection to be used when invoking
 a query defined with this library. Prepared statements are internally cached
 in a weak hash keyed on these connections.

 @racket[conn] cannot be a @emph{virtual connection}
 (see @secref["Virtual_Connections" #:doc '(lib "db/scribblings/db.scrbl")]).
}

@subsection{The Remaining @racket[define-query-...] Forms}

@(require (for-syntax racket/base
                      racket/syntax
                      syntax/srcloc))
@(define-syntax (a-bit-like-dqr stx)
   (syntax-case stx ()
     [(_ frm query-... return-contract)
        #'@defform[frm]{
        As @racketlink[define-query-rows (racket define-query-rows)], but
       @racket[id] is bound to a function that calls
       @racketlink[query-... (racket query-...)] and returns @racket[return-contract].}]))

@(a-bit-like-dqr (define-query-list (id arg ... (opt-arg default) ...) sql) query-list list?)
@(a-bit-like-dqr (define-query-row (id arg ... (opt-arg default) ...) sql) query-row vector?)
@(a-bit-like-dqr (define-query-maybe-row (id arg ... (opt-arg default) ...) sql) query-maybe-row (or/c #f vector?))
@(a-bit-like-dqr (define-query-value (id arg ... (opt-arg default) ...) sql) query-value any/c)
@(a-bit-like-dqr (define-query-maybe-value (id arg ... (opt-arg default) ...) sql) query-maybe-value (or/c #f any/c))
@(a-bit-like-dqr (define-query-exec (id arg ... (opt-arg default) ...) sql) query-exec void?)

@section{Utilities}

@defform[(define-query-suite (id arg ... (opt-arg default) ...) sql)]{
 @racket[define-query-suite] defines a suite of query functions
 equvalent to calling:
 @(racketblock
   (define-query-list (#,(var id).list ...) #,(var sql))
   (define-query-rows (#,(var id).rows ...) #,(var sql))
   (define-query-row (#,(var id).row ...) #,(var sql))
   (define-query-maybe-row (#,(var id).maybe-row ...) #,(var sql))
   (define-query-value (#,(var id).value ...) #,(var sql))
   (define-query-maybe-value (#,(var id).maybe-value ...) #,(var sql))
   (define-query-exec (#,(var id).exec ...) #,(var sql))) 

 In addition, a new name is defined: @racket[id]@racketidfont{.suite} containing:
 @racketblock[(list #,(var id).list
                    #,(var id).rows
                    #,(var id).row
                    #,(var id).maybe-row
                    #,(var id).value
                    #,(var id).maybe-value
                    #,(var id).exec)]
                                                                  
 See @racketlink[define-query-rows (racket define-query-rows)]
 for a description of the syntax.
}

@section{Database System Specifics}
@subsection{Prepared Statement Templates}

Note that ``the syntax of placeholders varies depending on the database system''
(@secref["query-statements" #:doc '(lib "db/scribblings/db.scrbl")]). This library
does not address issues of portability between database systems.

@subsection{Database Types}

The types of parameters and return values may vary between database systems. Please
refer to @other-doc['(lib "db/scribblings/db.scrbl")] for system specific issues.


@section{Examples}

We'll set up a little test bed first:

@(examples
  #:eval ev

  (define conn (sqlite3-connect #:database 'memory))
  (current-db-connection conn)
  (query-exec conn "CREATE TABLE FOO (x INTEGER, y INTEGER)")
  (query-exec
   conn
   "INSERT INTO FOO (x, y) VALUES (null, 42), (17, 42), (1, 2), (1, 4), (2, 4)"))

@racket[q1] takes no arguments.

@(examples
  #:eval ev  
  (define-query-maybe-row (q1) "SELECT * FROM FOO WHERE x IS NULL")
  (q1:sql)
  (q1))

We can use @racket[#:xform-result] to transform the result as it is returned:

@(examples #:eval ev (q1 #:xform-result (match-lambda [(vector a b) b])))

@racket[q2] takes one required argument. Note that the @racket[x] in the arguments
list and the @litchar{x} in the SQL are independent. @racket[x] will be bound to
the parameter @litchar{$1}.

@(examples
  #:eval ev  
  (define-query-maybe-row (q2 x) "SELECT * FROM FOO WHERE x = $1")
  (q2 17))

The following functions take optional and keyword arguments.

@(examples
  #:eval ev  
  (define-query-value (q3 (x 17)) "SELECT y FROM FOO WHERE x = $1")
  (q3 17)
  (q3 2)
  (q3)

  (define-query-maybe-value (q4 (x 17) (y 42)) "SELECT x+y FROM FOO WHERE x = $1 AND y = $2")
  (q4 17 42)
  (q4 2 4)
  (q4)

  (define-query-maybe-value (q5 #:x (x 22) (y 4)) "SELECT x+y FROM FOO WHERE x = $1 AND y = $2")
  (q5 #:x 17 42)
  (q5 4 #:x 2)
  (q5))

The following shows a transform function working on the @var[x] argument:

@(examples
  #:eval ev
  (define-query-maybe-value (q6 #:x ((x -> add1) 1) (y 4)) "SELECT x+y FROM FOO WHERE x = $1 AND y = $2")
  (q6 #:x 16 42)
  (q6 4 #:x 0)
  (q6))

Since that's all a bit involved, we can test the SQL and arguments that would be passed it in a kind
of ``dry run''.

@(examples
  #:eval ev
  (q6:sql)
  (q6:test-args #:x 16 42)
  (q6:test-args 4 #:x 0)
  (q6:test-args))

Let us also see the code generated from that example:

@(examples
  #:eval ev

  (define (expand-twice stx) (expand-once (expand-once stx)))

  (pretty-print
   (syntax->datum
    (expand-twice
     #'(define-query-maybe-value (q6 #:x ((x -> add1) 1) (y 4))
         "SELECT x+y FROM FOO WHERE x = $1 AND y = $2")))
   (current-output-port)
   1))

Using @racket[define-query-suite], we get longer names (because we have to descriminate between
the query types):

@(examples
  #:eval ev
  (define-query-suite (qs (x 1)) "SELECT x+y FROM FOO WHERE x = $1")
  (qs.rows 1)
  (qs.maybe-value 2)
  (qs.maybe-value 1)
  (qs.row:sql))

@close-eval[ev]
