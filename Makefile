doc:
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest-name index.html manual.scrbl

doc-queue:
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest-name index-queue.html manual-queue.scrbl

publish: doc
	scp * hashcollision.org:webapps/htdocs/brainfudge
