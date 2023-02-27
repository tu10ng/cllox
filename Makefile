.PHONY = run test test-args test-file test-repl

run : test

test : test-args test-file test-repl

test-args : cllox.lisp
	sbcl --script cllox.lisp st.lox b

test-file : cllox.lisp st.lox
	sbcl --script cllox.lisp st.lox

test-repl : cllox.lisp
	sbcl --script cllox.lisp
