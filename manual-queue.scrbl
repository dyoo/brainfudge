#lang scribble/manual

@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          (for-label racket)
	  (for-label racket/stxparam)
          (for-label racket/unsafe/ops)
          "scribble-helpers.rkt")


@inject-css{extra.css}
@inject-css{extra-queue.css}


@title{F*dging up a Racket}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@;; I'll need an evaluator for some small examples.
@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket
                        #:requires
                        (list (resolve-planet-path 
                               `(planet dyoo/bf/parser))))))))


@section{Introduction}

If people say that @link["http://racket-lang.org"]{Racket} is just a
@link["http://en.wikipedia.org/wiki/Scheme_(programming_language)"]{Scheme},
they are short-selling Racket a little.  It's more accurate to say
that Racket is a @link["http://docs.racket-lang.org/guide/languages.html"]{language} laboratory, with support for many different
languages.

Is that really true?  Racket does include a nice
@link["http://docs.racket-lang.org/guide/macros.html"]{macro} system,
which allows a programmer to add in new language constructs.  For
example, we can get while loops into Racket with relative ease:
    @codeblock{
    #lang racket
    (define-syntax-rule (while test body ...)
      (local [(define (loop)
                (when test
                  body ...
                  (loop)))]
        (loop)))
    ;; From this point forward, we've got while loops.
    (while (not (string=? (read-line) "quit"))
      (printf "never going to give you up\n")
      (printf "never going to let you down\n"))
    }
So we can certainly extend the language.  But this still looks just
like a Scheme.



Let's take a closer look at a Racket program.  Every Racket
program begins with a funny line at the very top that, on first
glance, looks redundant:
    @codeblock{
    #lang racket
    }
Why in the world does a Racket program need to say that it's a Racket
program?  Isn't that obvious?



We can understand the situation better by looking at another
environment on our desktop, namely the web browser.  A web browser
supports different kinds of HTML variants, since HTML is a moving
target, and browsers have come up with @link["http://en.wikipedia.org/wiki/Quirks_mode"]{crazy rules} for figuring out
how to take an arbitrary document and decide what HTML parsing rules
to apply to it.


@link["http://en.wikipedia.org/wiki/HTML5"]{HTML 5} tries to make this determination
somewhat more straightforward: we can define an HTML 5 document by
putting a DOCTYPE element at the very top of the file which
self-describes the document as being @emph{html}.

    @verbatim{
    <!DOCTYPE html>
    <html lang="en">
      <head><title>Hello world</title></head>
      <body><p>Hello world!</p></body>
    </html>
    }


Going back to the world of Racket, we see by analogy that the @litchar{#lang}
line in a Racket program is a self-description of how to treat the
rest of the program.  (Actually, the @litchar{#lang} line is quite bit more
active than this, but we'll get to this in a moment.)


The @racketmodname[racket] part in the @litchar{#lang} line isn't inevitable: the main Racket
distribution, in fact, comes bundled with several languages which can
take the place of the word @racketmodname[racket].  Many of these languages
(@racketmodname[racket/base], @racketmodname[typed/racket], @racketmodname[lazy]) still look like Racket... but some
of them don't.  Here's one example:
    @codeblock{
    #lang datalog
    ancestor(A, B) :- parent(A, B).
    ancestor(A, B) :-
      parent(A, C), D = C, ancestor(D, B).
    parent(john, douglas).
    parent(bob, john).
    ancestor(A, B)?
    }
This is an example of a @link["http://en.wikipedia.org/wiki/Datalog"]{Datalog}
program that deals with logical relations.  Neat!


What might be surprising is that the mechanism for using different
languages in Racket is wide open.  Let's expand our minds.
    @codeblock{
    #lang planet dyoo/bf
    ++++++[>++++++++++++<-]>.
    >++++++++++[>++++++++++<-]>+.
    +++++++..+++.>++++[>+++++++++++<-]>.
    <+++[>----<-]>.<<<<<+++[>+++++<-]>.
    >>.+++.------.--------.>>+.
    }
This language does not look like Racket.  It looks like line
noise.  This is
@link["http://en.wikipedia.org/wiki/Brainf*ck"]{@tt{brainf*ck}}.  Although
this language is not included in the main distribution, because it is
on @link["http://planet.racket-lang.org"]{PLaneT}, anyone with Racket
can easily play with it.


Ignoring the question of @emph{why?!!} someone would do this, let's ask another:
how do we build this?  This tutorial will cover how to build this language
into Racket from scratch.  


Let's get started!


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The view from high orbit}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

We want to teach Racket what it means when we say something like:
    @codeblock|{
    #lang planet dyoo/bf
    ,[.,]
    }|

As mentioned earlier, a @litchar{#lang} line is quite active: it tells the Racket runtime how to
convert from the surface syntax to a meaningful program.  Programs in Racket get digested
in a few stages; the process looks something like this:

@verbatim|{
                     reader        macro expansion
    surface syntax ---------> AST -----------------> core forms
    }|

When Racket sees
@litchar{#lang planet dyoo/bf}, it will look for a particular module that we call a @emph{reader};
a reader consumes surface syntax and excretes ASTs, and these ASTs are then
annotated so that Racket knows how to make sense out of them later on.
At this point, the rest of the Racket infrastructure kicks in and macro-expands the ASTs out, ultimately,
to a @link["http://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)"]{core} language.


So here's what we'll do:
@itemlist[
    @item{Capture the meaning of @tt{brainf*ck} by writing a semantics module.}
    @item{Go from the line noise of the surface syntax into a more structured form 
          by writing a parser module.}
    @item{Connect the pieces, the semantics and the surface syntax parser,
          by making a reader module.}
    @item{Profit!}]



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Flight preparations}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Since we're starting from scratch, let's first make a work directory
where we'll keep our source code.  I'll call the directory @filepath{bf/}, but you can use
whatever name you want.
    @verbatim|{
    $ mkdir bf
    }|

Ultimately, we want to put the fruit of our labor onto @link["http://docs.racket-lang.org/planet/index.html"]{PLaneT},
since that'll make it easier for others to use our work.
Let's set up a @link["http://docs.racket-lang.org/planet/Developing_Packages_for_PLaneT.html#(part._devlinks)"]{PLaneT development link} so the Racket environment knows about our work directory.  I already have an account
on PLaneT with my username @tt{dyoo}.  You can
@link["http://planet.racket-lang.org/add.ss"]{get an account} fairly easily.

If we enter the following at the command line,
@verbatim|{
   $ planet link dyoo bf.plt 1 0 bf
   }|
we'll make a development link that will associate any module path of the form @racket[(planet dyoo/bf/...)] 
to our local @filepath{bf/} directory.  Later on, when we create a package and upload it to PLaneT,
we can drop this development link, and then all the references that use @racket[(planet dyoo/bf/...)] will
immediately switch over to the one on the PLaneT server.


But does the link actually work?  Let's write a very simple module in our work directory, and
then see that Racket can find it through PLaneT.
    @verbatim|{
    $ cd bf
    ~/bf$ cat >hello.rkt
    #lang racket
    "hello world"
    }|
Ok, let's see if Racket can find our magnificent @filepath{hello.rkt} module if we use the PLaneTized version of the name. 
    @verbatim|{
    ~/bf$ racket
    Welcome to Racket v5.2.
    > (require (planet dyoo/bf/hello))
    "hello world"
    > 
    }|
If we get to this point, then we've got the PLaneT development link in place.





@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The @tt{brainf*ck} language}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

When we look at the definition of @link["http://en.wikipedia.org/wiki/Brainf*ck"]{@tt{brainf*ck}},
it's actually not too bad.  There's two bits of state,
@itemlist[
          @item{a byte array of data, and}
          @item{a pointer into that data array}
          ]
and it has only a few operations that affect this state:
@itemlist[
          @item{Increment the data pointer (@litchar{>})}
          @item{Decrement the data pointer (@litchar{<})}
          @item{Increment the byte at the data pointer (@litchar{+})}
          @item{Decrement the byte at the data pointer (@litchar{-})}
          @item{Write a byte to standard output (@litchar{.})}
          @item{Read a byte from standard input (@litchar{,})}
          @item{Perform a loop until the byte at the data pointer is zero (@litchar{[}, @litchar{]})}
          ]
Let's write a module that lets us play with such a system: let's call it @filepath{semantics.rkt}.
           
@filebox["semantics.rkt"]{
                          @codeblock|{
#lang racket

(require rackunit)                ;; for unit testing
(provide (all-defined-out))


;; Our state contains two pieces.
(define-struct state (data ptr)
  #:mutable)

;; Creates a new state, with a byte array of 30000 zeros, and
;; the pointer at index 0.
(define (new-state) 
  (make-state (make-vector 30000 0)
              0))

;; increment the data pointer
(define (increment-ptr a-state)
  (set-state-ptr! a-state (add1 (state-ptr a-state))))

;; decrement the data pointer
(define (decrement-ptr a-state)
  (set-state-ptr! a-state (sub1 (state-ptr a-state))))

;; increment the byte at the data pointer
(define (increment-byte a-state)
  (define v (state-data a-state))
  (define i (state-ptr a-state))
  (vector-set! v i (add1 (vector-ref v i))))

;; decrement the byte at the data pointer
(define (decrement-byte a-state)
  (define v (state-data a-state))
  (define i (state-ptr a-state))
  (vector-set! v i (sub1 (vector-ref v i))))

;; print the byte at the data pointer
(define (write-byte-to-stdout a-state)
  (define v (state-data a-state))
  (define i (state-ptr a-state))
  (write-byte (vector-ref v i) (current-output-port)))

;; read a byte from stdin into the data pointer
(define (read-byte-from-stdin a-state)
  (define v (state-data a-state))
  (define i (state-ptr a-state))
  (vector-set! v i (read-byte (current-input-port))))


;; we know how to do loops!
(define-syntax-rule (loop a-state body ...)
  (local [(define (loop)
            (unless (= (vector-ref (state-data a-state)
                                   (state-ptr a-state))
                        0)
              body ...
              (loop)))]
    (loop)))
}|}

Ok, that doesn't seem too bad.  But of course, we should test this; let's use
the @racketmodname{rackunit} unit testing framework and tickle this code.  Let's add
a little more to the end of @filepath{semantics.rkt}.
@filebox["semantics.rkt"]{
@codeblock|{           
;; Simple exercises.
(let ([s (new-state)])
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (increment-byte s)
  (check-equal? 2 (vector-ref (state-data s) 0))
  (decrement-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0)))

;; pointer movement
(let ([s (new-state)])
  (increment-ptr s)
  (increment-byte s)
  (check-equal? 0 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (decrement-ptr s)
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1)))

;; make sure standard input is doing something
(let ([s (new-state)])
  (parameterize ([current-input-port 
                  (open-input-bytes (bytes 3 1 4))])
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s))
  (check-equal? 3 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (check-equal? 4 (vector-ref (state-data s) 2)))


;; make sure standard output is doing something
(let ([s (new-state)])
  (set-state-data! s (vector 80 76 84))
  (let ([simulated-stdout (open-output-string)])
    (parameterize ([current-output-port simulated-stdout])
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s))
    (check-equal? "PLT" (get-output-string simulated-stdout))))


;; Let's see that we can clear.
(let ([s (new-state)])
  (set-state-data! s (vector 0 104 101 108 112 109 101 105
                            109 109 101 108 116 105 110 103))
  (set-state-ptr! s 15)
  ;; [ [-] < ]
  (loop s 
        (loop s (decrement-byte s))
        (decrement-ptr s))
  
  (check-equal? 0 (state-ptr s))
  (check-equal? (make-vector 16 0) (state-data s)))                                     
}|}
                                                                                              
Good!  Our tests, at the very least, let us know that our definitions are
doing something reasonable, and they should all pass.


However, there are a few things that we may want to fix in
the future, like the lack
of error trapping if the input stream contains @racket[eof].  And there's no bounds-checking
on the @racket[ptr] or on the values in the data.  Wow, there are quite a few things that we might want
to fix.  But at the very least, we now have a module that captures the semantics of @tt{brainf*ck}.



@section{Lisping a language}

We might even be cheeky enough to insist that people write @tt{brainf*ck} programs with s-expressions.
Let's take that route, and create a @link["http://docs.racket-lang.org/guide/module-languages.html"]{module language}
that uses our @filepath{semantics.rkt}.  We'll create such a module language in @filepath{language.rkt}.
@filebox["language.rkt"]{
                          @codeblock|{
#lang racket

(require "semantics.rkt")

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))

;; The current-state is a parameter used by the
;; rest of this language.
(define current-state (make-parameter (new-state)))

;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([current-state (new-state)])
       body ...)))

(define-syntax-rule (greater-than)
  (increment-ptr (current-state)))

(define-syntax-rule (less-than)
  (decrement-ptr (current-state)))

(define-syntax-rule (plus)
  (increment-byte (current-state)))

(define-syntax-rule (minus)
  (decrement-byte (current-state)))

(define-syntax-rule (period)
  (write-byte-to-stdout (current-state)))

(define-syntax-rule (comma)
  (read-byte-from-stdin (current-state)))

(define-syntax-rule (brackets body ...)
  (loop (current-state) body ...))
}|}


This @filepath{language.rkt} presents @tt{brainf*ck} as a s-expression-based language.
It uses the semantics we've coded up, and defines rules for handling
@racket[greater-than], @racket[less-than], etc...  We have a @link["http://docs.racket-lang.org/guide/parameterize.html"]{parameter} called @racket[current-state]
that holds the state of the @tt{brainf*ck} machine that's used through the language.

There's one piece of this language that looks particularly mysterious: what's the @racket[#%module-begin] form,
and what is it doing?  In Racket, every
module has an implicit @racket[#%module-begin] that wraps around the entirety of the module's body.
We can see this by asking Racket to show us the results of the expansion process;
here's a small example to demonstrate.
@interaction[#:eval my-evaluator
                    (syntax->datum
                     (expand '(module an-example-module '#%kernel
                                "hello"
                                "world")))
                    ]
Ignore, for the moment, the use of @racket[syntax->datum] or the funky use of @racket['#%kernel].
What we should notice
is that Racket has added in that @racket[#%module-begin] around the @racket["hello"] and @racket["world"].
So there's the implicit wrapping that Racket is doing.

It turns out that @racket[#%module-begin] can be really useful!  In particular,
we want to guarantee that every module written in @tt{brainf*ck} runs under a fresh state.  If 
we had two @tt{brainf*ck} programs running, say like this:
@racketblock[(require "my-first-bf-program.rkt")
             (require "my-second-bf-program.rkt")]
then it would be a shame to have the two programs clash just because they @tt{brainf*ck}ed each other's data!
By defining our own @racket[#%module-begin], we can ensure that each @tt{brainf*ck} module has
its own fresh version of the state, and our definition of @racket[my-module-begin] 
does this for us.



Once we've written @filepath{language.rkt}, we can use the language
like this:
@codeblock|{
#lang s-exp (planet dyoo/bf/language)

(plus)(plus)(plus)(plus)(plus) (plus)(plus)(plus)(plus)(plus)
(brackets
 (greater-than) (plus)(plus)(plus)(plus)(plus) (plus)(plus)
 (greater-than) (plus)(plus)(plus)(plus)(plus) (plus)(plus)
 (plus)(plus)(plus) (greater-than) (plus)(plus)(plus)
 (greater-than) (plus) (less-than)(less-than)(less-than)
 (less-than) (minus)) 
(greater-than) (plus)(plus) (period)
(greater-than) (plus) (period)
(plus)(plus)(plus)(plus)(plus) (plus)(plus) (period)
(period) (plus)(plus)(plus) (period)
(greater-than) (plus)(plus) (period)
(less-than)(less-than) (plus)(plus)(plus)(plus)(plus)
(plus)(plus)(plus)(plus)(plus) (plus)(plus)(plus)(plus)(plus)
(period) (greater-than) (period)
(plus)(plus)(plus) (period)
(minus)(minus)(minus)(minus)(minus)(minus)(period)
(minus)(minus)(minus)(minus)(minus)(minus)(minus)(minus)
(period)(greater-than) (plus) (period) (greater-than) (period)
}|

The @litchar{#lang} line here is saying, essentially, that the following program
is written with s-expressions, and should be treated with the module language @filepath{language.rkt}
that we just wrote up.  And if we run this program, we should see a familiar greeting.
Hurrah!


... But wait!  We can't just declare victory here.  We really do want
to allow the throngs of @tt{brainf*ck} programmers to write @tt{brainf*ck} in the surface syntax that
they deserve.
Keep @filepath{language.rkt} on hand, though.  We will reuse it by having our
parser transform the surface syntax into the forms we defined in @filepath{language.rkt}.


Let's get that parser working!

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Parsing the surface syntax}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The Racket toolchain includes a professional-strength lexer and parser
in the @link["http://docs.racket-lang.org/parser-tools/index.html"]{parser-tools} collection.
For the sake of keeping this example terse, we'll
write a simple @link["http://en.wikipedia.org/wiki/Recursive_descent_parser"]{recursive-descent parser} without using the parser-tools collection.  (But if our surface
syntax were any more complicated, we might reconsider this decision.)

The expected output of a successful parse should be some kind of abstract syntax tree.  What representation
should we use for the tree?  Although we can use s-expressions, 
they're pretty lossy: they don't record where they came from 
in the original source text.  For the case of @tt{brainf*ck}, we might not care,
but if we were to write a parser for a more professional, 
sophisticated language (like @link["http://lolcode.com/"]{LOLCODE}) we
want source locations so we can give good error messages during parsing or run-time.

As an alternative to plain s-expressions, we'll use a data structure built into Racket called a 
@link["http://docs.racket-lang.org/guide/stx-obj.html"]{syntax object}; syntax objects let
us represent ASTs, just like s-expressions, and they also carry along auxiliary
information, such as source locations.  Plus, as we briefly saw in our play with @racket[expand], syntax objects are the native data structure that Racket
itself uses during macro expansion, so we might as well use them ourselves.

For example,
@interaction[#:eval my-evaluator 
                 (define an-example-syntax-object
                   (datum->syntax #f 'hello (list "hello.rkt"
                                                  1
                                                  20
                                                  32
                                                  5)))]
The first argument that we pass into @racket[datum->syntax] lets us tell Racket any
lexical-scoping information that we know about the datum, but in this case, we don't have
any on hand, so we just give it @racket[#f].  Let's look at the structure of this syntax object.
@interaction[#:eval my-evaluator                                                      
                 an-example-syntax-object
                 (syntax? an-example-syntax-object)
                 (syntax->datum an-example-syntax-object)
                 (symbol? (syntax->datum an-example-syntax-object))
                 ]
So a syntax object is a wrapper around an s-expression, and we can get the underlying datum by using @racket[syntax->datum].
Furthermore, this object remembers where it came from, and that it was on line 1, column 20, position 32, and was five characters long:
@interaction[#:eval my-evaluator 
                 (syntax-source an-example-syntax-object)
                 (syntax-line an-example-syntax-object)
                 (syntax-column an-example-syntax-object)
                 (syntax-position an-example-syntax-object)
                 (syntax-span an-example-syntax-object)
                 ]


Now that we have some experience playing with syntax objects, let's write a parser.
Our parser will consume an @link["http://docs.racket-lang.org/reference/ports.html"]{input-port},
from which we can read in bytes with @racket[read-byte], or find out where we are with @racket[port-next-location].  We also want to store some record of where our program originated from,
so our parser will also take in a @racket[source-name] parameter.
We'll write the following into @filepath{parser.rkt}.
@filebox["parser.rkt"]{
                          @codeblock|{
#lang racket
;; The only visible export of this module will be parse-expr.
(provide parse-expr)

;; parse-expr: any input-port -> (U syntax eof)
;; Either produces a syntax object or the eof object.
(define (parse-expr src in)
  (define-values (line column position) (port-next-location in))
  (define next-char (read-char in))
  
  ;; decorate: s-expression number -> syntax
  ;; Wrap the s-expression with source location.
  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))

  (cond
    [(eof-object? next-char) eof]
    [else
     (case next-char
       [(#\<) (decorate '(less-than) 1)]
       [(#\>) (decorate '(greater-than) 1)]
       [(#\+) (decorate '(plus) 1)]
       [(#\-) (decorate '(minus) 1)]
       [(#\,) (decorate '(comma) 1)]
       [(#\.) (decorate '(period) 1)]
       [(#\[)
        ;; The slightly messy case is bracket.  We keep reading
        ;; a list of exprs, and then construct a wrapping bracket
        ;; around the whole thing.
        (define elements (parse-exprs src in))
        (define-values (l c tail-position) 
          (port-next-location in))
        (decorate `(brackets ,@elements)
                  (- tail-position position))]
       [else
        (parse-expr src in)])]))

;; parse-exprs: input-port -> (listof syntax)
;; Parse a list of expressions.
(define (parse-exprs source-name in)
  (define peeked-char (peek-char in))
  (cond
    [(eof-object? peeked-char)
     (error 'parse-exprs "Expected ], but read eof")]
    [(char=? peeked-char #\])
     (read-char in)
     empty]
    [(member peeked-char (list #\< #\> #\+ #\- #\, #\. #\[))
     (cons (parse-expr source-name in)
           (parse-exprs source-name in))]
    [else
     (read-char in)
     (parse-exprs source-name in)]))
}|}
This parser isn't anything too tricky, although there's a little bit of 
messiness because it needs to handle brackets recursively.  That part
is supposed to be a little messy anyway, since it's the capstone that builds tree structure out
of a linear character stream.  (If we were using a parenthesized language, we
could simply use @racket[read-syntax], but the whole point is to deal
with the messiness of the surface syntax!)

Let's see if this parser does anything useful:
@interaction[#:eval my-evaluator
                    (define my-sample-input-port (open-input-string ",[.,]"))
                    (port-count-lines! my-sample-input-port)
                    (define first-stx
                      (parse-expr "my-sample-program.rkt" my-sample-input-port))
                    first-stx
                    (define second-stx
                      (parse-expr "my-sample-program.rkt" my-sample-input-port))
                    second-stx
                    (parse-expr "my-sample-program.rkt" my-sample-input-port)]
Good!  So we're able to parse syntax objects out of an input stream.
@interaction[#:eval my-evaluator
                    (syntax->datum second-stx)
                    (syntax-source second-stx)
                    (syntax-position second-stx)
                    (syntax-span second-stx)]
And as we can see, we can explode the syntax object and look at its datum.  We should note
that the parser is generating syntax objects that use the same names as the defined names we
have in our @filepath{language.rkt} module language.  Yup, that's deliberate, and we'll see why in
the next section.


We mentioned that the parser wasn't too hard... but then again, we haven't written good traps
for error conditions.  This parser is a baby parser. 
If we were more rigorous, we'd probably implement it with the parser-tools collection,
write unit tests for the parser with @racketmodname[rackunit], and
make sure to produce good error messages when Bad Things happen 
(like having unbalanced brackets or parentheses.
@;; Yes, the unbalanced parentheses here is a joke.  I wonder if anyone will correct me for it.



Still, we've now got the language and a parser.  How do we tie them together?

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Crossing the wires}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This part is fairly straightforward.  We have two pieces in hand:
@itemlist[@item{A parser in @filepath{parser.rkt} for the surface syntax that produces ASTs}
          @item{A module language in @filepath{language.rkt} that provides the meaning for those ASTs.}
]
To combine these two pieces together, we want to define a @link["http://docs.racket-lang.org/guide/hash-lang_reader.html"]{reader} that associates the two.
When Racket encounters a @litchar{#lang} line of the form:
    @codeblock{
    #lang planet dyoo/bf
    }
it will look for a reader module in @filepath{lang/reader.rkt} and use it to parse the file.

Racket provides a helper module called @racketmodname[syntax/module-reader] to handle most of the 
dirty work; let's use it.  Make a @filepath{lang/} subdirectory, and create @filepath{reader.rkt}
in that subdirectory, with the following content:
@filebox["lang/reader.rkt"]{
@codeblock|{
#lang s-exp syntax/module-reader
(planet dyoo/bf/language)
#:read my-read
#:read-syntax my-read-syntax

(require "../parser.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-expr src in))
}|}
Some of this is magic, so let's step through this.  The second line of the file tells @racketmodname[syntax/module-reader] that any syntax objects that
come out are intended to take on their semantics from our language module @filepath{language.rkt}.  @racketmodname[syntax/module-reader]
is predisposed to assume that programs are read using @racket[read] and @racket[read-syntax], so we
override that default and plug in our @racket[parse-expr] function into place.


Now that we have all these pieces together, does any of this work?  Let's try it!
@verbatim|{
$ cat hello2.rkt
#lang planet dyoo/bf
++++++[>++++++++++++<-]>.
>++++++++++[>++++++++++<-]>+.
+++++++..+++.>++++[>+++++++++++<-]>.
<+++[>----<-]>.<<<<<+++[>+++++<-]>.
>>.+++.------.--------.>>+.

$ racket hello2.rkt 
Hello, World!   
           }|

Sweet, sweet words.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Landing on PLaneT}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Finally, we want to get this work onto @link["http://docs.racket-lang.org/planet/index.html"]{PLaneT} so that other people can share in the joy
of writing @tt{brainf*ck} in Racket.  Let's do it!


First, let's go back to the parent of our work directory.  Once we're there, we'll use the @tt{planet create} command.

@verbatim|{
$ planet create bf
planet create bf
MzTarring ./...
MzTarring ./lang...

WARNING:
	Package has no info.rkt file. This means it will not have a description or documentation on the PLaneT web site.

$ ls -l bf.plt
-rw-rw-r-- 1 dyoo nogroup 3358 Jun 12 19:39 bf.plt       
           }|

There are a few warnings, because we haven't defined an @filepath{info.rkt} which provides metadata
about our package.  Good, diligent citizens would  @link["http://docs.racket-lang.org/planet/Developing_Packages_for_PLaneT.html#(part._.Create_an__info_rkt__.File__.Optional_)"]{write an @filepath{info.rkt} file}, so let's write one.
@filebox["info.rkt"]{
@codeblock|{
#lang setup/infotab
(define name "bf: a brainf*ck compiler for Racket")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.1")
(define version "1.0")
(define repositories '("4.x"))
(define scribblings '())
(define primary-file "language.rkt")
(define blurb 
  '("Provides support for the brainf*ck language."))
(define release-notes
  '((p "First release")))
            }|}




Before we upload the package, let's make sure the @filepath{bf.plt} package works for us locally.  We'll simulate an installation.  First, let's break the development link.
@verbatim{
$ planet unlink dyoo bf.plt 1 0
}
If we try running our test program from before, it should fail on us.

@verbatim{
$ racket hello2.rkt 
require: PLaneT could not find the requested package: Server had no matching package: No package matched the specified criteria
}
Ok, that was expected.  Since we've dissolved the development link, and since we haven't uploaded the
package onto the PLaneT network yet, we see the error that we expect to see.

Next, let's use @tt{planet fileinject} to simulate an installation of our package from PLaneT.
@verbatim|{
$ planet fileinject dyoo bf.plt 1 0
planet fileinject dyoo bf.plt 1 0

============= Installing bf.plt on Sun, 12 Jun 2011 19:49:50 =============
raco setup: Unpacking archive from /home/dyoo/bf.plt
...
          }|
Lots and lots of output later, the package should be installed.

If we try running our test program again...
@verbatim{
$ racket hello2.rkt 
Hello, World!
          }
Good!  This simulates the situation where the package has been installed from PLaneT.


Once we're finally satisfied with the package's contents, we can finally upload it onto PLaneT.
If you log onto @link["http://planet.racket-lang.org"]{planet.racket-lang.org},
the user interface will allow
you to upload your @filepath{bf.plt} package.




@section{Acknowledgements}

An extended version of this tutorial can be found at
@url{http://hashcollision.org/brainfudge}.

Very special thanks to @link["http://www.cs.brown.edu/~sk/"]{Shriram
Krishnamurthi} for being understanding when I told him I had coded a
@tt{brainf*ck} compiler.  Basically, everyone in the Racket community
(like Mark Engelberg, Eric Hanchrow, Eli Barzilay, Matthew Flatt,
Robby Findler, and others that I'm blanking out on...) have been
wonderful.  The
@link["http://lists.racket-lang.org/users/archive/2011-June/046090.html"]{mailing
list thread} shows how many people have helped to shape this tutorial.

Guillaume Marceau, Rodolfo Carvalho, Eric Hanchrow, and Shriram helped
with grammar and spelling checks.  Casey Klein suggested a section in
the tutorial that shows how we can generate errors that point to
original sources, and Eli Barzilay pushed on including an optimization
section.  The extended tutorial includes both of these topics.

Finally, big shoutouts to the PLT group at
Brown University --- this one is for you guys.  :)
@;; Ha!  Closing parentheses.
