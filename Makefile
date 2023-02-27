.PHONY = run test test-args test-file test-repl

SBCL = sbcl --load '${HOME}/.sbclrc' --script

run : test

test : test-args test-file test-repl

test-args : cllox.lisp
	$(SBCL) cllox.lisp st.lox b

test-file : cllox.lisp st.lox
	$(SBCL) cllox.lisp st.lox

test-repl : cllox.lisp
	$(SBCL) cllox.lisp
