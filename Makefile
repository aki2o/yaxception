EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" yaxception.el

test:
	ret=0 ; \
	outfile=/tmp/.elisp-test-result ; \
	for f in $$(find test -type f -name "*.el"); do \
	    test -f $$outfile && rm -f $$outfile ; \
	    echo "Test $$f ..." ; \
		${CASK} exec ${EMACS} -Q --batch -L . -l $$f -f ert-run-tests-batch-and-exit $$outfile || ret=1 ; \
	    test -f $$outfile && cat $$outfile ; \
	done ; \
	test $$ret -eq 0

clean:
	rm -f yaxception.elc

.PHONY: all compile test clean
