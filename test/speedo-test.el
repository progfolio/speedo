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

;;

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

(ert-deftest speedo--plist-get* ()
  "It Returns VAL along PATH."
  :tags '(internal)
  (should (eq (speedo--plist-get* '() :one) nil))
  (should (eq (speedo--plist-get* '(:one t)) nil))
  (should (eq (speedo--plist-get* '(:one t) :two) nil))
  (should (eq (speedo--plist-get* '(:one (:two (:three t))) :one :two :three) t))
  (should (eq (speedo--plist-get* '(:one (:two (:three t))) :one :three) nil)))

(ert-deftest speedo--plist-put* ()
  "Returns a copy of PLIST with VAL set along PATH."
  :tags '(internal)
  (should (equal (let ((original '(:one nil)))
                   (speedo--plist-put* 2 original :one :two)
                   original)
                 '(:one nil)))
  (should (equal (speedo--plist-put* t '() :one :two :three)
                 '(:one (:two (:three t)))))
  (should (equal (speedo--plist-put* t '(:one 1)) '(:one 1)))
  (should (equal (speedo--plist-put* 2 '(:one 1) :one :two)
                 '(:one (:two 2)))))

(ert-deftest speedo--plist-remove ()
  "Return a copy of PLIST with KEYS removed."
  :tags '(internal)
  (let ((original '(:one 1 :two 2)))
    (should (equal (speedo--plist-remove original) original))
    (should (equal (speedo--plist-remove original :one) '(:two 2)))
    (should (equal original '(:one 1 :two 2)))))

(ert-deftest speedo--database-p ()
  "Return t if OBJ is a well formed database object.
It must be a non-empty plist with at least the following keys:
  - :title
  - :segments"
  :tags '(internal)
  (let ((db speedo-test-mock-db))
    (should (eq (speedo--database-p db) t))
    (should (eq (speedo--database-p '()) nil))
    (should (eq (speedo--database-p 2) nil))
    (should (eq (speedo--database-p '(:title)) nil))
    (should (eq (speedo--database-p '(:segments)) nil))
    (should (eq (speedo--database-p '(:title :segments)) nil))
    (should (eq (speedo--database-p '(:title nil :segments)) t))))

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
  :tags '(itnernal)
  (should-error (speedo--sub-hour-formatter nil nil nil nil) :type 'wrong-type-argument)
  (should (string= (speedo--sub-hour-formatter nil 0 0 0) "0:00.0"))
  (should (string= (speedo--sub-hour-formatter 0 0 0 0)   "0:00.0"))
  (should (string= (speedo--sub-hour-formatter 0 0 0 100) "0:00.1"))
  (should (string= (speedo--sub-hour-formatter 0 0 59 0)  "0:59.0"))
  (should (string= (speedo--sub-hour-formatter 0 59 0 0)  "59:00.0")))

(ert-deftest speedo--parse-time-string ()
  "Convert TIME-STRING into list of form: (milliseconds seconds minutes hours)."
  :tags '(itnernal)
  (mapc (lambda (spec)
          (eval `(should (equal (speedo--parse-time-string ,(car spec)) ',(cadr spec)))))
        '(("1"         (0 1 0 0))
          ("::"        (0 0 0 0))
          ("::::"      (0 0 0 0))
          ("1:"        (0 0 1 0))
          ("1::"       (0 0 0 1))
          ("::1"       (0 1 0 0))
          (":1:"       (0 0 1 0))
          ("1:1:1.001" (1 1 1 1))
          ("1:1:1.1"   (100 1 1 1))
          ("1:1:1.999" (999 1 1 1))
          ("1:1:90"    (0 90 1 1))
          ("1:1.0:90"  (0 90 1 1)))))

(provide 'speedo-test)
;;; speedo-test.el ends here
