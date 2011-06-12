#lang scribble/manual

@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          (for-label racket/base))

@title{F*dging up a Racket}
@author+email["Danny Yoo" "dyoo@cs.wpi.edu"]

@section{Introduction}

If people say that @link["http://racket-lang.org"]{Racket} is just a
@link["http://en.wikipedia.org/wiki/Scheme_(programming_language)"]{Scheme},
they are short-selling Racket a little.  It's more accurate to say
that Racket is a @link["http://docs.racket-lang.org/guide/languages.html"]{language} laboratory, with support for many different
languages.

Is that really true?  Racket does include a nice
@link["http://docs.racket-lang.org/guide/macros.html"]{macro} system ,
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
where we'll keep our source code.  I'll call the directory @tt{bf}, but you can use
whatever name you want.
    @verbatim|{
    dyoo@thunderclap:~$ mkdir bf
    }|

Ultimately, we want to put the fruit of our labor onto @link["http://docs.racket-lang.org/planet/index.html"]{PLaneT},
since that'll make it easier for others to use our work.
Let's set up a @link["http://docs.racket-lang.org/planet/Developing_Packages_for_PLaneT.html#(part._devlinks)"]{PLaneT development link} so the Racket environment knows about our work directory.  I already have an account
on PLaneT with my username @tt{dyoo}.  You can
@link["http://planet.racket-lang.org/add.ss"]{get an account} fairly easily.
    @verbatim|{
   dyoo@thunderclap:~$ planet link dyoo bf.plt 1 0 bf
   }|
Here, we're making a development link that will associate @racket[(planet dyoo/bf/...)] 
to the @tt{bf} directory we made earlier.  Later on, when we create a package and upload it to PLaneT,
we can drop this development link, and then references using @racket[(planet dyoo/bf/...)] will
immediately switch over to the package on the PLaneT server.


But does the link actually work?  Let's write a very simple module in our work directory, and
then see that Racket can find it through PLaneT.
    @verbatim|{
    dyoo@thunderclap:~$ cd bf
    dyoo@thunderclap:~/bf$ cat >hello.rkt
    #lang racket
    "hello world"
    }|
Ok, let's see if Racket can find our magnificant @tt{hello.rkt} module if we use the PLaneTized version of the name. 
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

Quickly explain, at a high level, what we're going to do:

[what happens with #lang?]

[readers] 

what the syntax/module-reader does for us,

and what we're going to do: define the semantics, define the surface syntax,
and connect those two in a language and reader.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The @tt{brainf*ck} language}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Talk about the semantics of @tt{brainf*ck}, code it up, and write test cases
to make sure we're doing the right thing.

Use the language with the s-exp reader.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Parsing the surface syntax}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Write a quick-and-dirty parser.
[Just use simple code for this; don't use parser-tools yet.]



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Lisping a language}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Make a module language that re-exports the semantics.  Use syntax/module-reader
to tie together the parser and the semantics, and try some examples.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Landing on PLaneT}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Getting the work onto PLaneT.  Creating the package.  Trying it out with fileinject.
Finally upload it onto PLaneT.

