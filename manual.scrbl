#lang scribble/manual

@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          (for-label racket/base))

@title{F*dging up a Racket}
@author+email["Danny Yoo" "dyoo@cs.wpi.edu"]


@;; I'll need an evaluator for some small examples.
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base)))



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
      (let loop ()
         (when test
            body ...
            (loop))))   
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


@link["http://diveintohtml5.org/"]{HTML 5} tries to make this determination
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
    #lang planet dyoo/brainfudge
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


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Flight preparations}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Since we're starting from scratch, let's first make a work directory
where we'll keep our source code.  I'll call the directory @filepath{bf/}, but you can use
whatever name you want.
    @verbatim|{
    dyoo@thunderclap:~$ mkdir bf
    }|

Ultimately, we want to put the fruit of our labor onto @link["http://docs.racket-lang.org/planet/index.html"]{PLaneT},
since that'll make it easier for others to use our work.
Let's set up a @link["http://docs.racket-lang.org/planet/Developing_Packages_for_PLaneT.html#(part._devlinks)"]{PLaneT development link} so the Racket environment knows about our work directory.  I already have an account
on PLaneT with my username @tt{dyoo}.  You can
@link["http://planet.racket-lang.org/add.ss"]{get an account} fairly easily.

If we enter the following at the command line,
@verbatim|{
   dyoo@thunderclap:~$ planet link dyoo bf.plt 1 0 bf
   }|
we'll make a development link that will associate any module path of the form @racket[(planet dyoo/bf/...)] 
to our local @filepath{bf/} directory.  Later on, when we create a package and upload it to PLaneT,
we can drop this development link, and then all the references that use @racket[(planet dyoo/bf/...)] will
immediately switch over to the one on the PLaneT server.


But does the link actually work?  Let's write a very simple module in our work directory, and
then see that Racket can find it through PLaneT.
    @verbatim|{
    dyoo@thunderclap:~$ cd bf
    dyoo@thunderclap:~/bf$ cat >hello.rkt
    #lang racket
    "hello world"
    }|
Ok, let's see if Racket can find our magnificant @filepath{hello.rkt} module if we use the PLaneTized version of the name. 
    @verbatim|{
    dyoo@thunderclap:~/bf$ racket
    Welcome to Racket v5.1.1.
    > (require (planet dyoo/bf/hello))
    "hello world"
    > 
    }|
If we get to this point, then we've got the PLaneT development link in place.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The view from high orbit}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


We want to teach Racket what it means when we say something like:
    @codeblock|{
    #lang planet dyoo/bf
    ,[.,]
    }|

As mentioned earlier, a @litchar{#lang} line is quite active: it tells the Racket runtime how to
convert from the surface syntax to an meaningful program.  Programs in Racket get digested
in a few stages; the process looks something like this:

@verbatim|{
                     reader          macro expansion
    surface syntax ----------> AST ------------------>  core forms
    }|

When Racket sees
@litchar{#lang planet dyoo/bf}, it'll look for a module called @tt{lang/reader.rkt} in our @tt{bf}
directory; the contents of a reader module will drive the rest of the process, consuming surface syntax
and excreting ASTs.
These AST will be annotated so that Racket knows how to make sense out of them later on.
At this point, the rest of the Racket infrastructure kicks in and macro-expands out ultimately
to a @link["http://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)"]{core} language.


So here's what we'll do:
@itemlist[
    @item{Capture the meaning of @tt{brainf*ck} by writing a semantics module.}
    @item{Write a parser module to go from the line noise of the surface syntax into a more structured form.}
    @item{Connect the pieces, the semantics and the surface syntax parser, by making a reader module.}
    @item{Profit!}]



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
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (add1 (vector-ref v i)))))

;; decrement the byte at the data pointer
(define (decrement-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (sub1 (vector-ref v i)))))

;; print the byte at the data pointer
(define (write-byte-to-stdout a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (write-byte (vector-ref v i) (current-output-port))))

;; read a byte from stdin into the data pointer
(define (read-byte-from-stdin a-state)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some tests follow:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (set-state-data! s (vector 0 29 92 14 243 1 6 92))
  (set-state-ptr! s 7)
  ;; [ [-] < ]
  (loop s 
        (loop s (decrement-byte s))
        (decrement-ptr s))
  
  (check-equal? 0 (state-ptr s))
  (check-equal? (vector 0 0 0 0 0 0 0 0) (state-data s)))                                     
                                       }|
                           }
                                                                                              
Ok, that wasn't too bad.  We've used the @racketmodname[rackunit] unit-testing
framework to make sure 
our definitions are doing something reasonable, and all the tests should pass.

However, there are a few things that we may want to fix in
the future, like the lack
of error trapping if the input stream contains @racket[eof].  And there's no bounds-checking
on the @racket[ptr] or on the values in the data.  Wow, there are quite a few things that we might want
to fix.  But at the very least, we now have a module that captures the semantics of @tt{brainf*ck}.



@section{Lisping a language}

We might even be cheeky enough to insist that people write @tt{brainf*ck} programs with s-expressions.
We can take that route, and create a @link["http://docs.racket-lang.org/guide/module-languages.html"]{module language}
that uses our @filepath{semantics.rkt}.  Let's create such a module language in @filepath{language.rkt}.
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
         #%module-begin)

(define *THE-STATE* (new-state))

(define-syntax-rule (greater-than)
  (increment-ptr *THE-STATE*))

(define-syntax-rule (less-than)
  (decrement-ptr *THE-STATE*))

(define-syntax-rule (plus)
  (increment-byte *THE-STATE*))

(define-syntax-rule (minus)
  (decrement-byte *THE-STATE*))

(define-syntax-rule (period)
  (write-byte-to-stdout *THE-STATE*))

(define-syntax-rule (comma)
  (read-byte-from-stdin *THE-STATE*))

(define-syntax-rule (brackets body ...)
  (loop *THE-STATE* body ...))
}|}


This @filepath{language.rkt} presents @tt{brainf*ck} as a s-expression language.  It uses the semantics we've coded up, and defines rules for handling
@racket[greater-than], @racket[less-than], etc...  The @racket[#%module-begin] thing
is an @link["http://docs.racket-lang.org/guide/module-languages.html#(part._implicit-forms)"]{implicit form}
that an module language needs to provide,
but we'll just borrow the one that comes from @racketmodname[racket].

Once we've written @filepath{language.rkt}, we can use the language
like this:
@codeblock|{
#lang s-exp (planet dyoo/bf/language)
(plus)(plus)(plus)(plus)(plus) (plus)(plus)(plus)(plus)(plus)
(brackets
 (greater-than) (plus)(plus)(plus)(plus)(plus) (plus)(plus)
 (greater-than) (plus)(plus)(plus)(plus)(plus) (plus)(plus)(plus)(plus)(plus)
 (greater-than) (plus)(plus)(plus)
 (greater-than) (plus)
 (less-than)(less-than)(less-than)(less-than) (minus))           
(greater-than) (plus)(plus) (period)
(greater-than) (plus) (period)
(plus)(plus)(plus)(plus)(plus) (plus)(plus) (period)
(period)
(plus)(plus)(plus) (period)
(greater-than) (plus)(plus) (period)
(less-than)(less-than) (plus)(plus)(plus)(plus)(plus)
(plus)(plus)(plus)(plus)(plus) (plus)(plus)(plus)(plus)(plus) (period)
(greater-than) (period)
(plus)(plus)(plus) (period)
(minus)(minus)(minus)(minus)(minus) (minus) (period)
(minus)(minus)(minus)(minus)(minus) (minus)(minus)(minus) (period)
(greater-than) (plus) (period)
(greater-than) (period)
}|

The @litchar{#lang} line here is saying, essentially, that the following program
is written with s-expressions, and should be treated with the module language @filepath{language.rkt}
that we just wrote up.  And if we run this program, we should see a familiar greeting.
Hurrah!


... but of course we shouldn't just declare victory here.  We really do want
to let people write @tt{brainf*ck} in the surface syntax that they deserve.
Let's keep @filepath{language.rkt} on hand, though.  We will reuse it by having our
parser transform the surface syntax into s-expressions.


Let's get that parser working!

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Parsing the surface syntax}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The Racket toolchain includes a professional-strength lexer and parser
in the @link["http://docs.racket-lang.org/parser-tools/index.html"]{parser-tools} collection.
For the sake of keeping this example simple, we'll
write a simple recursive-descent parser without using parser-tools.  (But if our surface
syntax were any more complicated, we might reconsider this decision.)  The input to our parser will be an @link["http://docs.racket-lang.org/reference/ports.html"]{input-port},
from which we can read in bytes.
The expected output should be some kind of abstract syntax tree.

What representation
should we use for the tree?  Although we can use s-expressions, 
they're pretty lossy: they don't record where they came from 
in the original source text.  For the case of @tt{brainf*ck}, we might not care,
but if we were to write a parser for a more professional, 
sophisticated language (like @link["http://lolcode.com/"]{LOLCODE}) we
want source locations so we can give good error messages during parsing or run-time.

As an alternative to plain s-expressions, we'll use a data structure built into Racket called a 
@link["http://docs.racket-lang.org/guide/stx-obj.html"]{syntax object}; syntax objects let
us represent ASTs---just like s-expressions---and they also carry along auxiliary
information, such as source locations.  (Plus, they're the native data structure that Racket
itself uses during macro expansion, so we might as well use them ourselves.)

For example,
@interaction[#:eval my-evaluator 
                 (define an-example-syntax-object
                   (datum->syntax #f 'hello (list "hello.rkt"
                                                  1
                                                  20
                                                  32
                                                  5)))
                                                      
                 an-example-syntax-object
                 (syntax? an-example-syntax-object)
                 (syntax->datum an-example-syntax-object)
                 (symbol? (syntax->datum an-example-syntax-object))
                 ]
This object remembers that it was on line 1, column 20, position 32, and was five characters
long.  We can query this:
@interaction[#:eval my-evaluator 
                 (syntax-line an-example-syntax-object)
                 (syntax-column an-example-syntax-object)
                 (syntax-position an-example-syntax-object)
                 (syntax-span an-example-syntax-object)
                 ]
The first argument that we pass into @racket[datum->syntax] lets us tells Racket any
lexical-scoping information that we know about the datum, but in this case, we don't have
any on hand, so we just give it @racket[#f].

Ok, let's write a parser.  We'll write the following into @filepath{parser.rkt}.
@filebox["parser.rkt"]{
                          @codeblock|{
}|}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Crossing the wires}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Make a module language that re-exports the semantics.  Use syntax/module-reader
to tie together the parser and the semantics, and try some examples.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Landing on PLaneT}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Getting the work onto PLaneT.  Creating the package.  Trying it out with fileinject.
Finally upload it onto PLaneT.

