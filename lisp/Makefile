all:
	sbcl --script aster.lisp
	cd lexer && make
clean:
	find . -name '*.fasl' -exec \rm {} \;

count:
	find . -name '*.lisp' | xargs wc -l
