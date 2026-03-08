EMACS=emacs

.PHONY: byte-compile clean

clean:
	@rm -f *.elc

byte-compile: clean
	@$(EMACS) -Q -L . --batch -f batch-byte-compile *.el
