.PHONY = run test build

LISP ?= sbcl

# the default path of executable after running `make build'
EXEC = ./bin/cllox



run : build
	@$(EXEC) $(RUN_ARGS)

test : build
	@$(EXEC) st.lox 2>/dev/null

build : $(EXEC)

$(EXEC) : cllox.asd cllox.lisp
	$(LISP) --eval '(asdf:load-asd (merge-pathnames "cllox.asd" (uiop:getcwd)))' \
            --eval '(ql:quickload :cllox)' \
            --eval '(asdf:make :cllox)' \
            --eval '(quit)'



# pass arguments to `make run'
# https://stackoverflow.com/questions/2214575/passing-arguments-to-make-run
ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif
