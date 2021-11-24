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

(defmacro speedo-test-template (template &rest bindings)
  "Return a TEMPLATE with BINDINGS replaced in each.
BINDINGS are similar to a `let*' varlist, except they are strictly paired.
This means you must explicitly bind symbols in BINDINGS to nil.
However, it means you needn't pair elements in parens.
For example: (a 1 b 2 c 3).

References in TEMPLATE (which should not be backqouted) are replaced
via the same rules as a backquoted list. e.g. `,` to replace, `,@` to splice
list elements."
  (declare (indent 1))
  (let (body)
    (dolist (env bindings)
      (unless (mod (length env) 2) (error "Uneven binding list: %S" env))
      (let (e)
        (cl-loop for (var val) on env by #'cddr
                 do (push (cons var (eval `(quote ,val) e)) e))
        (push (eval (list '\` template) e) body)))
    `(progn ,@(nreverse body))))

(defmacro speedo-test-template* (template vars &rest bindings)
  "Return TEMPLATEs with elements of BINDINGS destructured to VARS."
  "For example: with VARS = (in out) BINIDNGS = (in odd-elements out even-elements)."
  (declare (indent 1))
  (let ((unbound (mod (length bindings) (length vars))))
    (unless (zerop unbound) (error "Unven binding list: %S" (last bindings unbound)))
    `(speedo-test-template ,template
       ,@(eval `(cl-loop for ,vars on ',bindings
                         by (lambda (l) (nthcdr ,(length vars) l))
                         collect (apply #'append (cl-mapcar #'list ',vars (list ,@vars))))))))

(defmacro speedo-test-with-transput (template &rest io)
  "Anaphoric `speedo-test-template*' with IO bound to `in` and `out` in TEMPLATE."
  (declare (indent 1))
  `(speedo-test-template* ,template (in out) ,@io))

(defmacro speedo-test-with-input (template &rest inputs)
  "Anaphoric `speedo-test-template*' with INPUTS bound to `in` in TEMPLATE."
  (declare (indent 1))
  `(speedo-test-template* ,template (in) ,@inputs))

(ert-deftest speedo--plist-get* ()
  "Returns VAL along PATH."
  :tags '(internal)
  (speedo-test-with-input (should-not (speedo--plist-get* ,@in))
    ('()     :a)
    ('(:b t) nil)
    ('(:c t) :d))
  (should (equal (speedo--plist-get* '(:d (:e (:f t))) :d :e :f) t)))

(ert-deftest speedo--plist-put* ()
  "Returns a copy of PLIST with VAL set along PATH."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--plist-put* ,@in) ,out))
    (t  nil        :one :two :three)  '(:one (:two (:three t)))
    (2 '(:one nil) :one :two)         '(:one (:two 2))))

(ert-deftest speedo--plist-remove ()
  "Return a copy of PLIST with KEYS removed."
  :tags '(internal)
  (speedo-test-with-transput (let ((original '(:one 1 :two 2)))
                               (should (equal (speedo--plist-remove original ,in) ,out)))
    nil        original
    :missing   original
    :one       '(:two 2)
    (progn (speedo--plist-remove original :one) :two) '(:one 1)))

(ert-deftest speedo--database-p ()
  "Return t if OBJ is a well formed database object.
It must be a non-empty plist with at least the following keys:
  - :title
  - :segments"
  :tags '(internal)
  (speedo-test-with-input (should-not (speedo--database-p ,in))
    nil
    2
    '(:title)
    '(:segments)
    '(:title :segments))
  (let ((db speedo-test-mock-db)) (should (speedo--database-p db)))
  (should (speedo--database-p '(:title nil :segments))))

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

(ert-deftest speedo--formatter-sub-hour ()
  :tags '(internal)
  (should-error (speedo--formatter-sub-hour nil nil nil nil) :type 'wrong-type-argument)
  (speedo-test-with-transput (should (string= (speedo--formatter-sub-hour ,@in) ,out))
    (nil 0 0 0) "0:00.0"
    (0 0 0 0)   "0:00.0"
    (0 0 0 100) "0:00.1"
    (0 0 59 0)  "0:59.0"
    (0 59 0 0)  "59:00.0"))

(ert-deftest speedo--parse-time-string ()
  "Convert TIME-STRING into list of form: (milliseconds seconds minutes hours)."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--parse-time-string ,in) ',out))
    "1"         (0 1 0 0)
    "::"        (0 0 0 0)
    "::::"      (0 0 0 0)
    "1:"        (0 0 1 0)
    "1::"       (0 0 0 1)
    "::1"       (0 1 0 0)
    ":1:"       (0 0 1 0)
    "1:1:1.001" (1 1 1 1)
    "1:1:1.1"   (100 1 1 1)
    "1:1:1.999" (999 1 1 1)
    "1:1:90"    (0 90 1 1)
    "1:1.0:90"  (0 90 1 1)))

(ert-deftest speedo--time-string-to-ms ()
  "Convert TIME to ms."
  :tags '(internal)
  (speedo-test-with-transput (should (equal (speedo--time-string-to-ms ,in) ,out))
    "::"        0
    "::::"      0
    "0"         0
    "::1"       1000
    "1"         1000
    "1:"        60000
    ":1:"       60000
    "1::"       3600000
    "1:1:1.001" 3661001))

(declare-function format-time-string@force-UTC "speedo-test") ;;pacify compiler
(ert-deftest speedo--ms-to-date ()
  "Convert MS into human readable date string."
  :tags '(internal)
  (define-advice format-time-string (:filter-args (args) "force-UTC")
    "Force UTC so dev timezone does not interfere with `speedo-ms-to-date' test."
    (list (car args) (cadr args) t))
  (unwind-protect
      (speedo-test-with-transput
          (should (equal (speedo--ms-to-date ,in) (format "1970-01-01 %s" ,out)))
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
      (should (equal (speedo--date-to-ms (format "1970-01-01 %s+00:00" ,in)) ,out))
    "00:00:00" 0
    "00:00:01" 1000
    "00:01:00" 60000
    "01:01:01" 3661000))

(ert-deftest speedo--timestamp ()
  "Return TIME since unix epoch in milliseconds."
  :tags '(internal)
  (should (eq (speedo--timestamp (date-to-time "1970-01-01 00:01:00+00:00")) 60000)))

(ert-deftest speedo--formatter-compact ()
  "Return shortest time string from H M S MS."
  :tags '(internal)
  (speedo-test-with-transput
      (should (string= (speedo--formatter-compact ,@in) ,out))
    (0 0 0 0)   "0.0"
    (0 0 0 1)   "0.0" ;below tenth of a second discarded
    (0 0 0 100) "0.1"
    (0 0 1 0)   "1.0"
    (0 0 1 1)   "1.0"
    (0 0 1 100) "1.1"
    (0 0 59 0)  "59.0"
    (0 1 0 0)   "1:00.0"
    (1 0 0 0)   "1:00:00.0"
    (1 1 1 1)   "1:01:01.0"
    (1 1 1 100) "1:01:01.1"))

(ert-deftest speedo--format-ms ()
  "Format N milliseconds with `speedo--time-formatter'."
  :tags '(internal)
  (speedo-test-template*
   (let* ((speedo--time-formatter ',formatter))
     (should (string= (speedo--format-ms ,in) ,out)))
   (formatter                     in    out)
   nil                            60000 "1:00.0"
   speedo--formatter-compact      60000 "1:00.0"
   speedo--formatter-sub-hour     60000 "1:00.0"))

(provide 'speedo-test)
;;; speedo-test.el ends here
