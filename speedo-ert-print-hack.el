;; -*- lexical-binding: t; -*-
(progn
  (defun +without-print-limits (fn &rest args)
    "Advice around functions which print in `ert' with hardcoded levels."
    (let (print-level print-length)
      (apply fn args)))
  (advice-add #'ert-string-for-test-result :around #'+without-print-limits)
  (advice-add #'ert--pp-with-indentation-and-newline :around #'+without-print-limits))

(provide 'speedo-ert-print-hack)
