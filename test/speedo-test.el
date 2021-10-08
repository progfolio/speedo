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
  "it Returns VAL along PATH."
  (should (eq (speedo--plist-get* '() :one) nil))
  (should (eq (speedo--plist-get* '(:one t)) nil))
  (should (eq (speedo--plist-get* '(:one t) :two) nil))
  (should (eq (speedo--plist-get* '(:one (:two (:three t))) :one :two :three) t))
  (should (eq (speedo--plist-get* '(:one (:two (:three t))) :one :three) nil)))

(ert-deftest speedo--plist-put* ()
  "Returns a copy of PLIST with VAL set along PATH."
  (should (equal (let ((original '(:one nil)))
                   (speedo--plist-put* 2 original :one :two)
                   original)
                 '(:one nil)))
  (should (equal (speedo--plist-put* t '() :one :two :three)
                 '(:one (:two (:three t)))))
  (should (equal (speedo--plist-put* t '(:one 1)) '(:one 1)))
  (should (equal (speedo--plist-put* 2 '(:one 1) :one :two)
                 '(:one (:two 2)))))

(provide 'speedo-test)
;;; speedo-test.el ends here
