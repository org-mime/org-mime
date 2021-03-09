SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

deps:
	[ -f test/htmlize.el ] || curl -L https://raw.githubusercontent.com/hniksic/emacs-htmlize/HEAD/htmlize.el > test/htmlize.el

# Run tests.
test: clean deps
	$(EMACS) -batch -Q -l org-mime.el -l test/htmlize.el -l test/org-mime-tests.el
