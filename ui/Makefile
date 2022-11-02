# $Author: tv.raman.tv $
# Description:  Makefile for Aster Emacs Interface
# Keywords: Emacs, AsTeR
# {{{make rules

.POSIX:
# what emacs is called on your system
EMACS = emacs
SRC="../lisp"
# How to run in batch mode
BATCH = -batch -q -no-site-file     \
-f package-initialize --eval '(setq load-source-file-function  nil)' 
COMPILE =  -f batch-byte-compile
# How to compile
%.elc:  %.el 
	$(EMACS) $(BATCH) $(COMPILE) $<

# }}}
# {{{Targets:
all: aster.elc

# }}}
# {{{ end of file

#local variables:
#mode: makefile
#fill-column: 90
#folded-file: t
#end:

# }}}
