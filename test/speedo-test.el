;;; speedo-test.el --- test suite  -*- lexical-binding: t;  eval: (add-hook 'after-save-hook (lambda () (when-let ((dir (locate-dominating-file default-directory "makefile")) (default-directory dir)) (recompile))) nil 'local) -*-

;; Copyright (C) 2021 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for speedo.el

;;; Code:
(require 'speedo)
(require 'cl-lib)
(require 'ert)

(defvar speedo-test-mock-db
  '( :title "Mock" :category "test"
     :segments
     (( :name "A")
      ( :name "B")
      ( :name "C"))
     :attempts
     (( :start "2021-07-09 22:09:33" :splits nil :reset "9.120")
      ( :start "2021-07-09 22:11:02"
        :splits
        (( :segment "One" :duration "10")
         ( :segment "Two" :duration "9")
         ( :segment "Three" :duration "8")))
      ( :start "2021-07-09 22:11:05"
        :splits
        (( :segment "One" :duration "7")
         ( :segment "Two" :duration "6")
         ( :segment "Three" :duration "5")))
      ( :start "2021-07-09 22:11:06"
        :splits
        (( :segment "One" :duration "4")
         ( :segment "Two" :duration "3")
         ( :segment "Three" :duration "2"))))
     :runs ( :pb 3)))

(eval-and-compile
  (defun speedo-test--replace-symbol (target form replacement &optional splice)
    "Replace TARGET symbol in FORM with REPLACEMENT.
If SPLICE is non-nil and REPLACEMENT is a list, it is spliced in place."
    (let (result)
      (while form
        (let ((token (pop form)))
          (cond
           ((and token (listp token))
            (push (speedo-test--replace-symbol target token replacement splice) result))
           ((eq token target) (if (and splice replacement (listp replacement))
                                  (dolist (el replacement) (push el result))
                                (push replacement result)))
           (t (push token result)))))
      (nreverse result))))

(defmacro speedo-test-given (env &rest whenthen)
  "`eval' WHENTHEN pairs in ENV.
Anywhere the symbol `in' appears in ENV,
the WHEN form is spliced in.
The entire ENV is then spliced into the `cadr' of THEN."
  (declare (indent 1))
  (unless (cl-evenp (length whenthen))
    (error "Uneven When Then pair given"))
  `(progn
     ,@(cl-loop for (wh th) on whenthen by #'cddr
                collect (speedo-test--replace-symbol
                         'out th
                         (speedo-test--replace-symbol 'in env wh 'splice)))))

(defmacro speedo-test-with-transput (template &rest transputs)
  "Return test list with TRANSPUTS in TEMPLATE.
TRANSPUTS are an input object followed by an output object.
They are referred to in TEMPLATE by the symbols `in` and `out`."
  (declare (indent defun))
  `(progn
     ,@(cl-loop
        for (i o) on transputs by #'cddr
        for applied = (speedo-test--replace-symbol 'in template i 'splice)
        collect (speedo-test--replace-symbol 'out applied o))))

(defmacro speedo-test-with-input (template &rest inputs)
  "Retrun test list with INPUTS spliced into TEMPLATE.
Anywhere the symbol `in` appears in the TEMPLATE it is replaced."
  (declare (indent 1))
  `(progn ,@(cl-loop for input in inputs collect (speedo-test--replace-symbol
                                                  'in template input 'splice))))

(ert-deftest speedo--plist-get* ()
  "Returns VAL along PATH."
  :tags '(internal)
  (speedo-test-with-input (should-not (speedo--plist-get* in))
    ('() :a)
    ('(:b t))
    ('(:c t) :d))
  (should (equal (speedo--plist-get* '(:d (:e (:f t))) :d :e :f) t)))

(ert-deftest speedo--plist-put* ()
  "Returns a copy of PLIST with VAL set along PATH."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--plist-put* in) out))
    (t nil :one :two :three)  '(:one (:two (:three t)))
    (2 '(:one nil) :one :two) '(:one (:two 2)))
  (should-error (speedo--plist-put* t '(:one nil) nil)))

(ert-deftest speedo--plist-remove ()
  "Return a copy of PLIST with KEYS removed."
  :tags '(internal)
  (speedo-test-with-transput (let ((original '(:one 1 :two 2)))
                               (should (equal (speedo--plist-remove original in) out)))
    (:not-found) original
    (nil)        original
    (:one)       '(:two 2)
    ((progn (speedo--plist-remove original :one) :two)) '(:one 1)))

(ert-deftest speedo--database-p ()
  "Return t if OBJ is a well formed database object.
It must be a non-empty plist with at least the following keys:
  - :title
  - :segments"
  :tags '(internal)
  (speedo-test-with-input (should-not (speedo--database-p in))
    (nil)
    (2)
    ('(:title))
    ('(:segments))
    ('(:title :segments)))
  (speedo-test-with-input (let ((db speedo-test-mock-db))
                            (ignore db) ;pacify byte compiler
                            (should (speedo--database-p in)))
    (db)
    ('(:title nil :segments))))

(ert-deftest speedo--read-file ()
  "Read a valid DB file into an elisp object."
  :tags '(internal)
  (should-error (speedo--read-file "./nonexistant") :type 'user-error)
  (cl-letf (((symbol-function #'insert-file-contents) (symbol-function #'insert)))
    (should (equal (speedo--read-file "(:title nil :segments)") '(:title nil :segments)))
    (should-error (speedo--read-file "()") :type 'user-error)))

(ert-deftest speedo--data-modified-p ()
  "Return t if `speedo--data' and `speedo--data-file' are not `equal'."
  :tags '(stub)
  (skip-unless nil))

(ert-deftest speedo--sub-hour-formatter ()
  :tags '(internal)
  (should-error (speedo--sub-hour-formatter nil nil nil nil) :type 'wrong-type-argument)
  (speedo-test-with-transput (should (string= (speedo--sub-hour-formatter in) out))
    (nil 0 0 0) "0:00.0"
    (0 0 0 0)   "0:00.0"
    (0 0 0 100) "0:00.1"
    (0 0 59 0)  "0:59.0"
    (0 59 0 0)  "59:00.0"))

(ert-deftest speedo--parse-time-string ()
  "Convert TIME-STRING into list of form: (milliseconds seconds minutes hours)."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--parse-time-string in) out))
    "1"         '(0 1 0 0)
    "::"        '(0 0 0 0)
    "::::"      '(0 0 0 0)
    "1:"        '(0 0 1 0)
    "1::"       '(0 0 0 1)
    "::1"       '(0 1 0 0)
    ":1:"       '(0 0 1 0)
    "1:1:1.001" '(1 1 1 1)
    "1:1:1.1"   '(100 1 1 1)
    "1:1:1.999" '(999 1 1 1)
    "1:1:90"    '(0 90 1 1)
    "1:1.0:90"  '(0 90 1 1)))

(ert-deftest speedo--time-string-to-ms ()
  "Convert TIME to ms."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--time-string-to-ms in) out))
    "1"         1000
    "::"        0
    "::::"      0
    "1:"        60000
    "1::"       3600000
    "::1"       1000
    ":1:"       60000
    "1:1:1.001" 3661001
    "0" 0))

(declare-function format-time-string@force-UTC "speedo-test") ;;pacify compiler
(ert-deftest speedo--ms-to-date ()
  "Convert MS into human readable date string."
  :tags '(internal)
  (define-advice format-time-string (:filter-args (args) "force-UTC")
    "Force UTC so dev timezone does not interfere with `speedo-ms-to-date' test."
    (list (car args) (cadr args) t))
  (unwind-protect
      (speedo-test-with-transput
        (should (equal (speedo--ms-to-date in) (format "1970-01-01 %s" out)))
        0       "00:00:00"
        1       "00:00:00"
        1000    "00:00:01"
        60000   "00:01:00"
        3661001 "01:01:01")
    (advice-remove 'format-time-string #'format-time-string@force-UTC)))

(ert-deftest speedo--date-to-ms ()
  "Convert ISO 8601 DATE string to milliseconds."
  :tags '(internal)
  (speedo-test-with-transput
    (should (equal (speedo--date-to-ms (format "1970-01-01 %s+00:00" in)) out))
    "00:00:00" 0
    "00:00:01" 1000
    "00:01:00" 60000
    "01:01:01" 3661000))

(provide 'speedo-test)
;;; speedo-test.el ends here
