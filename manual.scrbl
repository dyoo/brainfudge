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

@inject-javascript|{
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-24146890-1']);
  _gaq.push(['_trackPageview']);
 
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();      
}|


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


@centered{@smaller{Source code can be found at:
@url{https://github.com/dyoo/brainfudge}.  The latest version of this
document lives in @url{http://hashcollision.org/brainfudge}.}}


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
            (unless (= (vector-ref (state-data a-state) (state-ptr a-state))
                        0)
              body ...
              (loop)))]
    (loop)))
}|}

Ok, that doesn't seem too bad.  But of course, we should test this; let's use
the @racketmodname[rackunit] unit testing framework and tickle this code.  Let's add
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
come out are intended to take on their semantics from our language module @filepath{language.rkt}.  @racket[syntax/module-reader]
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
original sources.  Eli Barzilay pushed on including an optimization
section.

Furthermore, thanks to those who commented from the
@link["http://www.reddit.com/r/programming/comments/i1slm/amazing_tutorial_demonstrating_the_power_of/"]{/r/programming}
Reddit thread: they helped isolate a performance issue regarding
parameters and further motivated the following section on
optimization.  David Van Horn pointed out how to use
@link["http://pypy.org"]{PyPy}'s JIT properly, with amazing results.
Sam Tobin-Hochstadt and Jay McCarthy provided a few optimization
suggestions, many of which have are in the main @racketmodname[(planet
dyoo/bf)] implementation.

Finally, big shoutouts to the PLT group at
Brown University --- this one is for you guys.  :)
@;; Ha!  Closing parentheses.



@section{Epilo... Optimization and polishing!}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@; Warning Will Robinson, Warning!
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@;; Just as in Puella Magi Madoka Magica, where things change in
@;; Episode 10, here we too go back to the past.
@;; We revise and revise in Chapter 10 with the hopes of making
@;; things better.
@;; Hopefully this won't be a disaster.


So we upload and release the package on PLaneT, and send our
marketeers out to spread the Word.  We kick back, lazily twiddle our
thumbs, and await the adoration of the global @tt{brainf*ck}
community.

To our dismay, someone brings up the fact that our
implementation is
@link["http://www.reddit.com/r/programming/comments/i1slm/amazing_tutorial_demonstrating_the_power_of/c20e7ka"]{slower}
than an @link["https://bitbucket.org/brownan/pypy-tutorial/src/tip/example1.py"]{interpreter} written in another language.  What?! 

But the Internet is absolutely correct.  Let's run the numbers.
We can grab another @tt{brainf*ck} implementation and try it on a
benchmarking program, like the one that
@link["https://github.com/dyoo/brainfudge/blob/master/examples/prime.rkt"]{generates
prime numbers}.  Let's see what the competition looks like:

@verbatim|{
$ echo 100 | time ~/local/pypy/bin/pypy example1.py prime.b
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
16.72user 0.24system 0:17.18elapsed 98%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+3554minor)pagefaults 0swaps
}|

Ok, about sixteen seconds.  Not bad.  We're not even using their JIT, and
they're still producing reasonable results.

Now let's look at our own performance.  We surely can't do worse, right?

@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
37.36user 0.65system 0:38.15elapsed 99%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+10259minor)pagefaults 0swaps
}|

Thirty-seven seconds.  Wow.  Ouch.


Outrageous!  Aren't interpreters supposed to be slower than
compilers?  Isn't Racket a
@link["http://docs.racket-lang.org/guide/performance.html"]{JIT-compiled
language}?  What the heck happened?


We tried to follow the creed that says @emph{Get it right, then get it
fast}... except that we didn't.  We forgot the second part about getting it fast. 
Just because something is compiled and driven by a JIT doesn't guarantee
that the generated code's going to perform particularly well, and the benchmark above
shows that something strange is happening.


So let's try our hand at optimization!  We may not get the raw 
performance of an impressive project like @link["http://pypy.org/"]{PyPy}, but we
still should be able to perform reasonably well.  Furthermore, we will
include some error handling that uses the source locations we constructed in our
parser, in order to precisely point out runtime errors in the original source.


As a warning, if you ran through the previous sections, you may want
to take a small break before continuing forward.  This optimization
section is included near the end because the changes we'll be making
require some deeper digging into Racket's language infrastructure,
expecially with
@link["http://docs.racket-lang.org/guide/macros.html"]{macros}.  Take
a relaxing walk, and then come back to this when you're ready.




@subsection{Staring into the hot-spot}

If we look a little closely into our implementation, we might notice
something funny.  Well, we might notice many things that look funny in
our @tt{brainf*ck} implementation, but there's a particular one we'll
focus on: each of the forms in @filepath{language.rkt} refer to the
@racket[current-state] parameter.  We use that parameter to make sure
the other forms in the language use the same @racket[current-state]
value.  And of course we want this kind of localized behavior, to
prevent the kind of interference that might happen if two
@tt{brainf*ck} programs run.

... But every use of the parameter appears to be a function call.
Just how bad is that?  Let's see.  We can fire up our trusty DrRacket
and try the following program in our Interactions window:

@interaction[#:eval my-evaluator
(require rackunit)
(define my-parameter (make-parameter (box 0)))
(time
 (parameterize ([my-parameter (box 0)])
   (for ([x (in-range 10000000)])
     (set-box! (my-parameter) 
               (add1 (unbox (my-parameter)))))
   (check-equal? (unbox (my-parameter)) 10000000)))]

Hmmmm...  Ok, what if we didn't have the parameter, and just accessed
the variable more directly?

@interaction[#:eval my-evaluator
(require rackunit)
(time
 (let ([my-parameter (box 0)])
   (for ([x (in-range 10000000)])
     (set-box! my-parameter 
               (add1 (unbox my-parameter))))
   (check-equal? (unbox my-parameter) 10000000)))]

In the immortal words of
@link["http://en.wikipedia.org/wiki/Neo_(The_Matrix)"]{Neo}:
@emph{Whoa}.  Ok, we've got ourselves a target!


Let's take a look again at the definition of our
@racket[my-module-begin] in @filepath{language.rkt}.

@codeblock|{
(define current-state (make-parameter (new-state)))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([current-state (new-state)])
       body ...)))}|

Let's replace the use of the @racket[parameterize] here with a simpler
@racket[let].  Now we've got something like this:

@codeblock|{
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (let ([my-fresh-state (new-state)])
       body ...)))}|

But now we have a small problem: we want the rest of the inner
@racket[body] forms to syntactically recognize and re-route any use of
@racket[current-state] with this @racket[my-fresh-state] binding.  But
we certainly can't just rewrite the whole @filepath{language.rkt} and
replace uses of @racket[current-state] with @racket[my-fresh-state],
because @racket[my-fresh-state] isn't a global variable!  What do we
do?

There's a tool in the Racket library that allows us to solve this
problem: it's called a
@link["http://docs.racket-lang.org/reference/stxparam.html"]{syntax
parameter}.  A syntax parameter is similar to the reviled parameter
that we talked about earlier, except that it works
@emph{syntactically} rather than @emph{dynamically}.  A common use of
a syntax parameter is to let us wrap a certain area in our code, and
say: ``Anywhere this identifier shows up, rename it to use this
variable instead.''

Let's see a demonstration of these in action, because all this talk
is a little abstract.  What do these syntax parameters really
do for us?  Let's play with them again a little.


@interaction[#:eval my-evaluator
(require racket/stxparam)

(define-syntax-parameter name 
  (lambda (stx)
    #'"Madoka"))

name

(define-syntax-rule (say-your-name)
  (printf "Your name is ~a\n" name))

(define (outside-the-barrier)
  (printf "outside-the-barrier says: ")
  (say-your-name))


(say-your-name)

(let ([the-hero "Homerun"])
  (syntax-parameterize 
        ([name (make-rename-transformer #'the-hero)])
     (say-your-name)
     (outside-the-barrier)))
]

It helps to keep in mind that, in Racket, macros are functions that
work during compile-time.  They take an input syntax, and produce an
output syntax.  Here, we define @racket[name] to be a macro that
expands to @racket[#'"Madoka"] by default.  When we use @racket[name]
directly, and when we use it in @racket[(say-your-name)] for the first
time, we're seeing this default in place.

However, we make things more interesting (and a little more
confusing!) in the second use of @racket[say-your-name]: we use
@racket[let] to create a variable binding, and then use
@racket[syntax-parameterize] to reroute every use of @racket[name],
syntactically, with a use of @racket[the-hero].  Within the boundary
defined at the @racket[syntax-parameterize]'s body, @racket[name] is
magically transformed!  That's why we can see @racket["Homerun"] in
the second use of @racket[(say-your-name)].


Yet, where we use it from @racket[outside-the-barrier], @racket[name]
still takes on the default.  Why?

Let's go through the macro expanding process by hand, and wherever we
see @racket[(say-your-name)], let's replace with the @racket[(printf
...)].  So when we say:
@racketblock[
(define (outside-the-barrier)
  (printf "outside-the-barrier says: ")
  (say-your-name))
]
we really mean:
@racketblock[
(define (outside-the-barrier)
  (printf "outside-the-barrier says: ")
  (printf "Your name is ~a\n" name))
]

The use of @racket[name] here is lexically outside the barrier set up
by @racket[syntax-parameterize].

And now let's look at the second expression, the one with the @racket[let].  We take:
@racketblock[
(let ([the-hero "Homerun"])
  (syntax-parameterize 
        ([name (make-rename-transformer #'the-hero)])
     (say-your-name)
     (outside-the-barrier)))
]
and after expanding it partially by hand, we get:
@racketblock[
(let ([the-hero "Homerun"])
  (syntax-parameterize 
        ([name (make-rename-transformer #'the-hero)])
     (printf "Your name is ~a\n" name)
     (outside-the-barrier)))
]

Ah!  So the use of @racket[name] that's introduced by
@racket[say-your-name] is within the lexical boundaries of the
@racket[syntax-parameterize] form.  But @racket[outside-the-barrier]
is a plain, vanilla function, and because it's not a macro, it doesn't
inline itself into the @racket[syntax-parameterize]'s body.  We can
compare this with the more dynamic behavior of @racket[parameterize],
and see that this difference is what makes
@racket[syntax-parameterize] different from @racket[parameterize].
Well, we could tell that they're different just from the names... but
the behavior we're seeing here makes it more clear just what that
difference is.



Whew!  Frankly, all of this is a little magical.  But the hilarious
thing, despite all this verbiage about syntax parameters, is that the
implementation of the language looks almost exactly the same as
before.  Here's a version of the language that uses these syntax
parameters; let's save it into @filepath{language.rkt} and replace the
previous contents.

@filebox["language.rkt"]{
                          @codeblock|{
#lang racket

(require "semantics.rkt"
         racket/stxparam)

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))

;; The current-state is a syntax parameter used by the
;; rest of this language.
(define-syntax-parameter current-state #f)

;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (let ([fresh-state (new-state)])
       (syntax-parameterize 
            ([current-state
              (make-rename-transformer #'fresh-state)])
           body ...))))

(define-syntax-rule (greater-than)
  (increment-ptr current-state))

(define-syntax-rule (less-than)
  (decrement-ptr current-state))

(define-syntax-rule (plus)
  (increment-byte current-state))

(define-syntax-rule (minus)
  (decrement-byte current-state))

(define-syntax-rule (period)
  (write-byte-to-stdout current-state))

(define-syntax-rule (comma)
  (read-byte-from-stdin current-state))

(define-syntax-rule (brackets body ...)
  (loop current-state body ...))
}|}


What effect does this change alone make to our performance on
@tt{brainf*ck} prime generation?  Let's cross our fingers!

@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
6.38user 0.09system 0:06.63elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+10121minor)pagefaults 0swaps
}|


Now that's more like it!  Down from thirty-seven seconds to about six
and a half.  Nice.  When we compare this versus the previous
implementation of the language, we might laugh ruefully: we just got
rid of a few more parentheses and typed in a few symbols.  But of
course, that's not what we truly did.  What in the world just
happened?


Let's summarize what we did: earlier, we had used
@racket[parameterize] to maintain some shared local state within the
dynamic extent of our module's body.  However, on reflection, we see
that we don't need the full power of dynamic scope: a simpler (and
cheaper!) lexical scoping mechanism is sufficient here.  We now use
@racket[syntax-parameterize] as our mechanism for sharing that state
with the rest of the language.  And if we ever see
@racket[parameterize] in a tight inner loop again, we shudder
instinctively.


But now ambition rears its head and whispers to us: can we make the
code go faster?  At some point, we'll hit diminishing returns, but let's see
what other obvious things we can do, and observe what happens to the
benchmark results as we optimize.



@subsection{Macros, macros everywhere}

One trivial thing we can do is revisit our @filepath{semantics.rkt}
file, and transform all of the exported function definitions into
macros.  This allows Racket's compiler to inline the definitions for
each use.  That is, right now, Racket processes and expands our
@tt{brainf*ck} programs up to the function definitions in the
@filepath{semantics.rkt}, but does no intra-module optimizations.  If
we modify those functions into macros, maybe that will help
performance.

Basically, we go in and replace each
@racket[define] with a @racket[define-syntax-rule].
Here's what @filepath{semantics.rkt} looks like after this change:
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
(define-syntax-rule (new-state)
  (make-state (make-vector 30000 0)
              0))
 
;; increment the data pointer
(define-syntax-rule (increment-ptr a-state)
  (set-state-ptr! a-state (add1 (state-ptr a-state))))
 
;; decrement the data pointer
(define-syntax-rule (decrement-ptr a-state)
  (set-state-ptr! a-state (sub1 (state-ptr a-state))))
 
;; increment the byte at the data pointer
(define-syntax-rule (increment-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (add1 (vector-ref v i)))))
 
;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (sub1 (vector-ref v i)))))
 
;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (write-byte (vector-ref v i) (current-output-port))))
 
;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (read-byte (current-input-port)))))
 
;; we know how to do loops!
(define-syntax-rule (loop a-state body ...)
  (let loop ()
    (unless (= (vector-ref (state-data a-state)
                           (state-ptr a-state))
               0)
      body ...
      (loop))))
}|}

What effect does this have on our benchmark?
@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
3.78user 0.10system 0:03.96elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+10101minor)pagefaults 0swaps
}|

Ok, inlining each of the definitions of the semantics gives us a
little more performance, at the cost of some code expansion.  But not a large one.



@subsection{Structures?  Where we're going, we won't need structures...}

While we have our eye on @filepath{semantics.rkt}, we might wonder:
how much is it costing us to access the @racket[data] and @racket[ptr]
fields of our state?  The use of the structure introduces an indirect
memory access.  Maybe we can eliminate it, by saying that the
@emph{state} of our language consists of two pieces, rather than one
aggregate piece.  So one proposal we can consider is to remove the
structure, and have each of the rules in our semantics deal with both
pieces of the state.

The editing for this will be somewhat non-local: we'll need to touch
both @filepath{semantics.rkt} and @filepath{language.rkt} because each
form in the semantics will take in two pieces, and each language
construct in the language must provide those two pieces.  Let's see
what this looks like for both files.


@filebox["semantics.rkt"]{
@codeblock|{
#lang racket
 
(provide (all-defined-out))
 
;; Provides two values: a byte array of 30000 zeros, and
;; the pointer at index 0.
(define-syntax-rule (new-state)
  (values (make-vector 30000 0)
          0))
 
;; increment the data pointer
(define-syntax-rule (increment-ptr data ptr)
  (set! ptr (add1 ptr)))
 
;; decrement the data pointer
(define-syntax-rule (decrement-ptr data ptr)
  (set! ptr (sub1 ptr)))
 
;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (vector-set! data ptr (add1 (vector-ref data ptr))))
 
;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (vector-set! data ptr (sub1 (vector-ref data ptr))))
 
;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout data ptr)
  (write-byte (vector-ref data ptr) (current-output-port)))
 
;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin data ptr)
  (vector-set! data ptr (read-byte (current-input-port))))
 
;; we know how to do loops!
(define-syntax-rule (loop data ptr body ...)
  (let loop ()
    (unless (= (vector-ref data ptr)
               0)
      body ...
      (loop))))
}|}



@filebox["language.rkt"]{
@codeblock|{
#lang racket
 
(require "semantics.rkt"
         racket/stxparam)
 
(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))
 
;; The current-data and current-ptr are syntax parameters used by the
;; rest of this language.
(define-syntax-parameter current-data #f)
(define-syntax-parameter current-ptr #f)
 
;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (let-values ([(fresh-data fresh-ptr) (new-state)])
       (syntax-parameterize
            ([current-data
              (make-rename-transformer #'fresh-data)]
             [current-ptr
              (make-rename-transformer #'fresh-ptr)])
           body ...))))
 
(define-syntax-rule (greater-than)
  (increment-ptr current-data current-ptr))
 
(define-syntax-rule (less-than)
  (decrement-ptr current-data current-ptr))
 
(define-syntax-rule (plus)
  (increment-byte current-data current-ptr))
 
(define-syntax-rule (minus)
  (decrement-byte current-data current-ptr))
 
(define-syntax-rule (period)
  (write-byte-to-stdout current-data current-ptr))
 
(define-syntax-rule (comma)
  (read-byte-from-stdin current-data current-ptr))
 
(define-syntax-rule (brackets body ...)
  (loop current-data current-ptr body ...))
}|}


Ok, so this change is pretty mechanical.  However, it does have a
consequence: it means that our use of the semantics is a bit more
restricted.  We give each form (@racket[increment-ptr],
@racket[decrement-ptr], ...) an identifier for the @racket[ptr],
because some of the rules will @racket[set!] the value of the
@racket[ptr] identifier.  That requires us to first bind
the state variables,
@racketblock[(let-values ([(data ptr) (new-state)]) 
                ...)]
and then use @racket[data] and @racket[ptr] bindings
consistently with the semantics forms.  In a sense, the semantics now treat
its arguments as reference variables.


In any case, what does our benchmark tell us about this optimization?

@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
1.13user 0.09system 0:01.30elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+10095minor)pagefaults 0swaps
}|


That seems like a worthwhile optimization.  Ok, we're down to about a
second plus a little more.



@subsection{Strapping on the safety goggles}

Let's pause for a moment.  We should ask ourselves: is our language
actually doing The Right Thing?  We might consider the following
situations:

@itemlist[

@item{The program may try to read a byte from the standard input port,
and encounter @racket[eof] instead.}

@item{A program may try to increment the value at the pointer beyond
the boundaries of a byte.}

@item{The machine might be instructed to shift the pointer @bold{*clunk*} off
the data array.}
]

What happens in our current implementation when these situations arise?

Oh dear.  We should have looked at this earlier!  How shameful!  None
of these are directly addressed by our current implementation.  We'd
better correct these flaws before continuing forward, before anyone
else notices.  And even if this costs us a few milliseconds in
performance, it's certainly worth knowing exactly what should happen
in these situations.



@subsubsection{@racket[eof]}

According to the
@link["http://www.muppetlabs.com/~breadbox/bf/standards.html"]{Portable
Brainf*ck} guide, 

@nested[#:style 'inset]{
If a program attempts to input a value when there is no more data in
the input stream, the value in the current cell after such an
operation is implementation-defined. (The most common choices are to
either store 0, or store -1, or to leave the cell's value
unchanged. This is frequently the most problematic issue for the
programmer wishing to achieve portability.)}


Let's choose to treat the reading of @racket[eof] as a zero.  We can
change the definition of @racket[read-byte-from-stdin] in
@filepath{semantics.rkt} to do this.

@codeblock{
;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin data ptr)
  (vector-set! data ptr
               (let ([a-value (read-byte (current-input-port))])
                 (if (eof-object? a-value)
                     0
                     a-value))))
}


@subsubsection{@racket[out-of-range byte mutation]}

Next, let's look at what the portability guide says about what happens
when we increment or decrement a byte past certain limits.

@nested[#:style 'inset]{
The range of values a single cell can contain is
implementation-defined. (The range need not be consistent, either:
consider the case of a "bignum" implementation, whose cells' ranges
would be limited only by currently available resources.) However, the
range of every cell shall always at least include the values 0 through
127, inclusive.)

If a program attempts to either decrement the value of a cell below
its documented minimum value, if any, or increment the value of a cell
beyond its documented maximum value, if any, then the value in the
cell after such an operation is implementation-defined. (Most
implementations choose to let the value wrap around in a fashion
typical to C integers, but this is not required.)}


So it looks like we have a little leeway here.  We've implicitly been
using an vector of bytes, since we've been using @racket[read-byte]
and @racket[write-byte] on the values of the @racket[data] vector.
Since
@link["http://docs.racket-lang.org/guide/bytestrings.html#(tech._byte)"]{bytes}
range between @racket[0] and @racket[255], let's keep our cells in
that range too.  One simple tool we can use is @racket[modulo], which
allows us to keep the values in that range.  Let's use it.

@codeblock|{
;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (vector-set! data ptr (modulo (add1 (vector-ref data ptr)) 256)))
 
;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (vector-set! data ptr (modulo (sub1 (vector-ref data ptr)) 256)))
}|



@subsubsection{@racket[out-of-bounds pointer movement]}

What does the portability guide say about moving the tape out-of-bounds?

@nested[#:style 'inset]{
If a program attempts to move the pointer below the first array cell,
or beyond the last array cell, then that program's behavior is
undefined. (A few implementations cause the pointer to wrap around,
but many, perhaps most, implementations behave in a manner consistent
with a C pointer wandering off into arbitrary memory.)}

Wait.  Stop right there.  It is absolutely unacceptable for us to
just have the pointer wander out-of-bounds like that.  
Even we @tt{brainf*ck} programmers must have our standards.
Instead, let's make it a guaranteed runtime error that halts
evaluation.  Moreover, let's make sure the error message points
directly at the offending instruction in the source text.


How do we get our errors to highlight in DrRacket?  Racket, like many
languages, provides exceptions as structured values.  In particular,
DrRacket will cooperate when it sees an exception that provides source
location.


Let's look at a short and quick example of error highlighting in
action.  Open up DrRacket and run the following program:
@codeblock|{
#lang racket

;; We create a structure that supports the
;; prop:exn:srcloc protocol.  It carries
;; with it the location of the syntax that
;; is guilty.
(define-struct (exn:fail:he-who-shall-not-be-named
                exn:fail)
  (a-srcloc)
 #:property prop:exn:srclocs
            (lambda (a-struct)
              (match a-struct
                [(struct exn:fail:he-who-shall-not-be-named
                         (msg marks a-srcloc))
                 (list a-srcloc)])))

;; We can play with this by creating a form that
;; looks at identifiers, and only flags specific ones.
(define-syntax (skeeterize stx)
 (syntax-case stx ()
   [(_ expr)
    (cond
      [(and (identifier? #'expr)
            (eq? (syntax-e #'expr) 'voldemort))
       (quasisyntax/loc stx
         (raise (make-exn:fail:he-who-shall-not-be-named
                 "oh dear don't say his name"
                 (current-continuation-marks)
                 (srcloc '#,(syntax-source #'expr)
                         '#,(syntax-line #'expr)
                         '#,(syntax-column #'expr)
                         '#,(syntax-position #'expr)
                         '#,(syntax-span #'expr)))))]
      [else
       ;; Otherwise, leave the expression alone.
       #'expr])]))

(define (f x)
 (* (skeeterize x) x))

(define (g voldemort)
 (* (skeeterize voldemort) voldemort))


;; Examples:
(f 7)
(g 7)  ;; The error should highlight the use
       ;; of the one-who-shall-not-be-named
       ;; in g.
}|

When we create a @racket[make-exn:fail:he-who-shall-not-be-named], we
provide it a @racket[srcloc] from the originating syntax objects.
Furthermore, we tell the Racket runtime that this structure is a good
source for source locations, by annotating the structure's definition
with @racket[prop:exn:srclocs].  This allows the runtime system to
cooperate with the DrRacket editor, so that when a
@racket[make-exn:fail:he-who-shall-not-be-named] does get raised at
runtime, the editor can nicely highlight the offending party.


When we were looking at parsing, we were careful enough to produce
syntax objects with source locations.  It would be a shame to waste
that effort.  Here's what we'll do: we'll adjust the semantics of
@racket[increment-ptr] and @racket[decrement-ptr] to take in one more
argument: a representation of the source location.  If we see that the
pointer's going to fall off, we can then raise an exception that's
annotated with @racket[srcloc] information.  That should give the
DrRacket environment the information it needs to highlight
tape-movement errors at runtime.

We'll need to change the definition of @racket[greater-than] and
@racket[less-than] in @filepath{language.rkt} to pass along the source
locations to the semantics forms, and we need to change the semantics
to use that location information whenever bad things happen.  Here's
what @racket[greater-than] will look like:
@codeblock|{
(define-syntax (greater-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (increment-ptr current-data current-ptr 
                      (srcloc '#,(syntax-source stx)
                              '#,(syntax-line stx)
                              '#,(syntax-column stx)
                              '#,(syntax-position stx)
                              '#,(syntax-span stx))))]))}|

One small complication is that we need the ability to talk about the
source location of the syntax object being fed to the
@racket[greater-than] macro, so we switched from using
@racket[define-syntax-rule] to the more low-level @racket[syntax-case]
macro definer.

Let's look at the corresponding changes we need to make to
@racket[increment-ptr]; assuming we have a definition for an
@racket[exn:fail:out-of-bounds] exception, the code for
@racket[increment-ptr] will look like this.

@codeblock|{
(define-syntax-rule (increment-ptr data ptr loc)
  (begin
    (set! ptr (add1 ptr))
    (when (>= ptr (vector-length data))
      (raise (make-exn:fail:out-of-bounds 
              "out of bounds"
              (current-continuation-marks)
              loc)))))}|





Our @filepath{semantics.rkt} and @filepath{language.rkt} now look like
the following:

@filebox["semantics.rkt"]{
@codeblock|{
#lang racket

(provide (all-defined-out))
 
;; We use a customized error structure that supports
;; source location reporting.
(define-struct (exn:fail:out-of-bounds exn:fail)
  (srcloc)
  #:property prop:exn:srclocs
             (lambda (a-struct)
               (list (exn:fail:out-of-bounds-srcloc a-struct))))

;; Provides two values: a byte array of 30000 zeros, and
;; the pointer at index 0.
(define-syntax-rule (new-state)
  (values (make-vector 30000 0)
          0))
 
;; increment the data pointer
(define-syntax-rule (increment-ptr data ptr loc)
  (begin
    (set! ptr (add1 ptr))
    (when (>= ptr (vector-length data))
      (raise (make-exn:fail:out-of-bounds 
              "out of bounds"
              (current-continuation-marks)
              loc)))))

;; decrement the data pointer
(define-syntax-rule (decrement-ptr data ptr loc)
  (begin
    (set! ptr (sub1 ptr))
    (when (< ptr 0)
      (raise (make-exn:fail:out-of-bounds 
              "out of bounds"
              (current-continuation-marks)
              loc)))))
 
;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (vector-set! data ptr 
               (modulo (add1 (vector-ref data ptr)) 256)))
 
;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (vector-set! data ptr 
               (modulo (sub1 (vector-ref data ptr)) 256)))
 
;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout data ptr)
  (write-byte (vector-ref data ptr) (current-output-port)))
 
;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin data ptr)
  (vector-set! data ptr 
               (let ([a-value (read-byte (current-input-port))])
                 (if (eof-object? a-value)
                     0
                     a-value))))
 
;; we know how to do loops!
(define-syntax-rule (loop data ptr body ...)
  (let loop ()
    (unless (= (vector-ref data ptr)
               0)
      body ...
      (loop))))
}|}



@filebox["language.rkt"]{
@codeblock|{
#lang racket
 
(require "semantics.rkt"
         racket/stxparam)
 
(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))
 
;; The current-data and current-ptr are syntax parameters used by the
;; rest of this language.
(define-syntax-parameter current-data #f)
(define-syntax-parameter current-ptr #f)
 
;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (let-values ([(fresh-data fresh-ptr) (new-state)])
       (syntax-parameterize
            ([current-data
              (make-rename-transformer #'fresh-data)]
             [current-ptr
              (make-rename-transformer #'fresh-ptr)])
           body ...))))
 
(define-syntax (greater-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (increment-ptr current-data current-ptr 
                      (srcloc '#,(syntax-source stx)
                              '#,(syntax-line stx)
                              '#,(syntax-column stx)
                              '#,(syntax-position stx)
                              '#,(syntax-span stx))))]))
 
(define-syntax (less-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (decrement-ptr current-data current-ptr
                      (srcloc '#,(syntax-source stx)
                              '#,(syntax-line stx)
                              '#,(syntax-column stx)
                              '#,(syntax-position stx)
                              '#,(syntax-span stx))))]))
 
(define-syntax-rule (plus)
  (increment-byte current-data current-ptr))
 
(define-syntax-rule (minus)
  (decrement-byte current-data current-ptr))
 
(define-syntax-rule (period)
  (write-byte-to-stdout current-data current-ptr))
 
(define-syntax-rule (comma)
  (read-byte-from-stdin current-data current-ptr))
 
(define-syntax-rule (brackets body ...)
  (loop current-data current-ptr body ...))
}|}




And if we try running the following grumpy-looking program,
@verbatim|{
#lang planet dyoo/bf

  ／人◕ ‿‿ ◕人＼

   ***********
  *           *
  *  o>    <o  *
  *            *
  *  <<<<<<<<  *
   *          *
     ********
}|
@;; Poor Mami.

DrRacket will properly highlight the second @litchar{<} at the left
edge of the face's mouth.  Hurrah!


At the same time, we do incur a runtime cost for these safety checks.
@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
1.56user 0.08system 0:01.71elapsed 95%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+8678minor)pagefaults 0swaps
}|
but let's accept the hit here.





@subsection{Running with scissors}

There's one obvious thing we haven't done yet: we haven't taken a look at
@racketmodname[racket/unsafe/ops].  That module provides
functions that act like @racket[+], @racket[vector-ref], and
many of the the other functions we've used in @filepath{semantics.rkt}.  However, unlike
their @emph{safe} equivalents, the ones in @racketmodname[racket/unsafe/ops] don't
do type tests on their inputs.

This can reduce some runtime costs.  We're already making sure that
the pointer lies within the boundaries of the data, with our last set
of changes, so having Racket do the same kind of internal check is
redundant.

As an example, we can take @racket[increment-byte]:
@racketblock[
(define-syntax-rule (increment-byte data ptr)
  (vector-set! data ptr 
               (modulo (add1 (vector-ref data ptr))
                       256)))]
and modify it to use the unsafe version of the operators, to get:
@racketblock[
(define-syntax-rule (increment-byte data ptr)
  (unsafe-vector-set! data ptr 
                      (unsafe-fxmodulo
                       (unsafe-fx+ (unsafe-vector-ref data ptr) 1)
                       256)))]


If we're very careful, we can use @racket[unsafe-fx+] and
@racket[unsafe-vector-ref] on our @racket[data] and @racket[ptr]
values: since we're explicitly managing the state of our
@tt{brainf*ck} machine, we know all the input types... as long as we
don't mess it up.  The flip side is that it's easy to mess up.  For
example, if our mind wanders to that pleasant afternoon hiking in the
mountains, and we quickly type out: 

@codeblock{
;; WARNING WARNING DO NOT ACTUALLY EXECUTE THIS!!!
(unsafe-vector-ref ptr data)}

then it's very likely that we'll crash the Racket VM, and any program
running under the VM at the time.  That would make our @tt{brainf*ck}
users unhappy with us, to say the least.

So we need to tread very, very carefully.

Here's what the @filepath{semantics.rkt} look like when we use the unsafe operations.
@filebox["semantics.rkt"]{
@codeblock|{
#lang racket

;; unsafe operations for speed.
;; But be very careful!
(require racket/unsafe/ops)  

(provide (all-defined-out))
 
;; We use a customized error structure that supports
;; source location reporting.
(define-struct (exn:fail:out-of-bounds exn:fail)
  (srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (list (exn:fail:out-of-bounds-srcloc a-struct))))

;; Provides two values: a byte array of 30000 zeros, and
;; the pointer at index 0.
(define-syntax-rule (new-state)
  (values (make-vector 30000 0)
          0))
 
;; increment the data pointer
(define-syntax-rule (increment-ptr data ptr loc)
  (begin
    (set! ptr (unsafe-fx+ ptr 1))
    (when (unsafe-fx>= ptr (unsafe-vector-length data))
      (raise (make-exn:fail:out-of-bounds 
              "out of bounds"
              (current-continuation-marks)
              loc)))))

;; decrement the data pointer
(define-syntax-rule (decrement-ptr data ptr loc)
  (begin
    (set! ptr (unsafe-fx- ptr 1))
    (when (unsafe-fx< ptr 0)
      (raise (make-exn:fail:out-of-bounds 
              "out of bounds"
              (current-continuation-marks)
              loc)))))
 


;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (unsafe-vector-set! data ptr
                      (unsafe-fxmodulo
                       (unsafe-fx+
                        (unsafe-vector-ref data ptr) 1)
                       256)))

;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (unsafe-vector-set! data ptr
                      (unsafe-fxmodulo
                       (unsafe-fx-
                        (unsafe-vector-ref data ptr) 1)
                       256)))
 
;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout data ptr)
  (write-byte (unsafe-vector-ref data ptr)
              (current-output-port)))
 
;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin data ptr)
  (unsafe-vector-set! data ptr 
                      (let ([a-value (read-byte
                                      (current-input-port))])
                        (if (eof-object? a-value)
                            0
                            a-value))))
 
;; we know how to do loops!
(define-syntax-rule (loop data ptr body ...)
  (let loop ()
    (unless (unsafe-fx= (unsafe-vector-ref data ptr)
                        0)
      body ...
      (loop))))
            }|
}




@section{A final accounting}

We can see the net effect of applying the combination of all these optimizations.


@verbatim|{
$ raco make prime.rkt && (echo 100 | time racket prime.rkt)
Primes up to: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
1.24user 0.05system 0:01.41elapsed 91%CPU (0avgtext+0avgdata 0maxresident)k
0inputs+0outputs (0major+8692minor)pagefaults 0swaps
}|

And that's not too bad of a result.  We've gone from thirty-seven seconds to
just over one.


What we have in hand isn't the world's
@link["http://code.google.com/p/esotope-bfc/wiki/Comparison"]{fastest}
@tt{brainf*ck} implementation.  Ours isn't horrible, mind you, but it
doesn't win speed records.  What we do have, though, is an
implementation that's fairly straightforward, and integrates well within
the umbrella of the other languages and tools in Racket.


It's one with which we can easily run experiments.  What if
we wanted to run @tt{brainf*ck} programs in
@link["http://docs.racket-lang.org/reference/places.html"]{parallel}?
What if we want to run these programs under a restrictive
@link["http://docs.racket-lang.org/reference/Sandboxed_Evaluation.html"]{sandbox}?
Would using a
@link["http://docs.racket-lang.org/ts-guide/index.html"]{type system}
allow us to remove all those messy unsafe annotations in our
semantics, while still removing the redundant type checks?

And what if we want to look at other languages besides @tt{brainf*ck}?
Now that we have a better understanding about how the Racket language
toolchain works, how easy is it to implement a more realistic
language?


There's much more content about
@link["http://docs.racket-lang.org/guide/languages.html"]{building
languages} in the
@link["http://docs.racket-lang.org/guide/index.html"]{Racket Guide};
hopefully, this tutorial helps other hackers who'd love to try their
hand at language design and implementation.

Also, please feel free to ask questions on the
@link["http://lists.racket-lang.org/users/"]{Racket Users mailing
list}; we'll be happy to talk!
