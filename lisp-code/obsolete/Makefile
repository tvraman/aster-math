PRINTER = rumor
 #Make etags on all the lisp directories:
 tags: etags latex-parser/*lisp read-aloud/*lisp  afl/*lisp synthesizer/*.lisp  \
gries-book/*lisp rz-book/*lisp vanloan-book/*lisp vavasis-book/*lisp \
cs611-notes/*lisp afl/sound-audio/*lisp browse/*lisp
count: 
	wc ./*.lisp latex-parser/*lisp synthesizer/*.lisp   \
lexer/lispify.l \
	clos-helper/*lisp \
 system-definitions/*.system  emacs-interface/*.el  \
	read-aloud/*lisp afl/*lisp afl/pronounce/*lisp  \
 afl/sound-audio/*lisp afl/sound-audio/*.c \
 afl/total-space/*lisp \
gries-book/*lisp 	rz-book/*lisp vanloan-book/*lisp vavasis-book/*lisp  \
 browse/*lisp \
  cs611-notes/*lisp norvig-book/*lisp dennis-math-books/*lisp
alltags:
	etags read-aloud/*lisp latex-parser/*lisp afl/*lisp \
 afl/total-space/*lisp afl/sound-audio/*lisp \
	clos-helper/*lisp \
	afl/pronounce/*lisp \
gries-book/*lisp rz-book/*lisp vanloan-book/*lisp vavasis-book/*lisp \
cs611-notes/*lisp afl/sound-audio/*lisp browse/*lisp
	mv TAGS ALLTAGS
	make tags
	make petags
#make petags 
petags:
		        petags -f clos-reading-rules -o PETAGS \
	`ls read-aloud/*lisp afl/*lisp latex-parser/*lisp \
	afl/pronounce/*lisp \
 afl/total-space/*lisp afl/sound-audio/*lisp \
	clos-helper/*lisp \
gries-book/*lisp 	rz-book/*lisp vavasis-book/*lisp vanloan-book/*lisp  \
		cs611-notes/*lisp  browse/*lisp`
printout:
	psify -c lexer/indent-lispify.l | lpr -P$(PRINTER)
	@echo "printed lexer" >> print-log
	psify -lisp latex-parser/*.lisp  \
	latex-parser/parser.system  | lpr -P$(PRINTER) 
	@echo "Printed recognizer" >> print-log
		psify  -lisp afl/*.lisp afl/afl.system  \
  | lpr -P$(PRINTER)
		@echo "Printed speech component of afl. " >> print-log
		psify -lisp  afl/total-space/*.lisp  | lpr -P$(PRINTER) 
		@echo "Printed total audio space. " >> print-log
		psify afl/sound-audio/*.lisp | lpr -P$(PRINTER)
		@echo "Printed sound component. " >> print-log
		psify afl/sound-audio/*.c | lpr -P$(PRINTER)
		@echo "Printed C code from sound component. " >> print-log
		psify afl/pronounce/*.lisp | lpr -P$(PRINTER) 
		@ECHO "pRINTED PRONUNCIATION COMPONENT. " >> print-log
		psify -lisp  read-aloud/*.lisp read-aloud/read-aloud.system \
 | lpr -P$(PRINTER)
		@echo "Printed reading rules component. " >> print-log
		psify -lisp  browse/*.lisp browse/browse.system \
 | lpr -P$(PRINTER) 
		@echo "Printed browser. " >> print-log
	psify -lisp emacs-interface/*.el | lpr -P$(PRINTER) 
	@echo "Printed emacs  interface to system "
