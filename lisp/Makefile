all:
	sbcl --eval '(require :asdf)' --load ./aster.lisp --eval '(uiop:quit)'
	cd lexer && make
clean:
	find . -name '*.fasl' -exec \rm {} \;
