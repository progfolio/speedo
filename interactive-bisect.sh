#!/bin/bash

emacs --eval '(speedo-load-file "/tmp/Test-Game")' \
      --eval "(defun speedo--ask-to-save () (ignore))" \
      --eval "(defun good () (interactive) (kill-emacs 0))" \
      --eval "(defun bad  () (interactive) (kill-emacs 1))" \
      --eval "(defun skip () (interactive) (kill-emacs 125)"
