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
	# ${CASK} exec ${EMACS} -Q --batch -L . $$(for f in $$(find test -type f -name "*.el"); do echo " -l $$f"; done) -f batch-expectations
	for f in $$(find test -type f -name "*.el"); do
		${CASK} exec ${EMACS} -Q --batch -L . -l $$f -f batch-expectations;
	done

clean:
	rm -f yaxception.elc

.PHONY: all compile test clean
