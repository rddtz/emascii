EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) --batch -Q -L . -L test -l test/emascii-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile emascii.el emascii-gallery.el

clean:
	rm -f *.elc test/*.elc
