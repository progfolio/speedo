.POSIX:
#To test with a different Emacs version run:
#$ export EMACS=<desired Emacs executable>
#$ make -e
EMACS = emacs

.PHONY: all
all:  clean compile check
compile: *.elc
check: *.elc
	$(EMACS) -Q --batch -L . -L ./test \
					 -l ert -l speedo-ert-print-hack -l speedo-test \
           --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"
clean:
	rm -f */*.elc
.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . --batch --eval "(byte-recompile-directory \"./\" 0 t)"
