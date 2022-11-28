all:
	@sbcl --script aster.lisp 2>&1 > /dev/null
	@echo "Compiled lisp"
	@cd lexer && make --no-print-directory
	@echo "Setup Slime in Emacs, then load  ui/aster.el and run M-x aster"
clean:
	find . -name '*.fasl' -exec \rm {} \;

count:
	find . -name '*.lisp' | xargs wc -l
