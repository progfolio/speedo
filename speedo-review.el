;;; speedo-review.el --- Review runs                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords:

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

(defun speedo-review--attempts ()
  "Return a list of attempts."
  (let* ((candidates
          (mapcar
           (lambda (attempt)
             (cons (string-trim
                    (string-join
                     (list
                      (format-time-string "%Y-%m-%d %H:%M%p"
                                          (/ (plist-get attempt :start) 1000))
                      (if-let ((duration (speedo--format-ms
                                          (speedo--splits-duration
                                           (plist-get attempt :splits)))))
                          duration
                        "       ")
                      (if (speedo--attempt-complete-p attempt)
                          (propertize "complete" 'face '(:weight bold))
                        "reset")
                      (mapconcat (lambda (tag) (format "%S" tag))
                                 (plist-get attempt :tags) " "))
                     " "))
                   attempt))
           (plist-get speedo--data :attempts)))
         (selections (delete-dups (completing-read-multiple "Attempts: " candidates))))
    (mapcar (lambda (selection) (alist-get selection candidates nil nil #'string=))
            selections)))

(defcustom speedo-review-include-target t
  "If non-nil, consider `speedo--comparison-target' implicit target of comparisons."
  :type  'boolean
  :group 'speedo)

(defun speedo--compare-ui-init (attempts)
  "Initialize comparison UI format for ATTEMPTS."
  (with-current-buffer (get-buffer-create speedo-buffer)
    (let ((segment-col
           (list
            "Segment"
            (max
             (floor
              (* 1.2
                 (car (cl-sort (mapcar (lambda (segment) (length (plist-get segment :name)))
                                       (plist-get speedo--data :segments))
                               #'>))))
             8)))
          (target-attempt (car attempts)))
      (setq tabulated-list-format
            (vconcat
             (list segment-col)
             (mapcar (lambda (a)
                       (let ((alias (or (speedo--plist-get* a :alias)
                                        (format-time-string
                                         "%Y-%m-%d %H:%M%p"
                                         (/ (plist-get a :start) 1000)))))
                         (when (equal a target-attempt)
                           (setq alias (propertize alias 'face '(:weight bold))))
                         (list alias (1+ (length alias)))))
                     attempts)
             (list (list "Average" 10)))
            tabulated-list-entries (speedo--compare-rows attempts)))
    ;;(speedo--compare-header)
    (tabulated-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print 'remember-pos 'update)))

;; (defun speedo--compare-header ()
;;   "Set the comparison header."
;;   (with-current-buffer speedo-buffer
;;     (setq header-line-format
;;           (list (speedo--header-game-info) " "
;;                 '(:propertize "Comparisons" face speedo-header-game-info)))))

(defun speedo--compare-rows (attempts)
  "Return table rows for ATTEMPTS."
  (let* ((segments (plist-get speedo--data :segments))
         (segment-count (length segments))
         (target (car attempts))
         (target-splits (plist-get target :splits))
         (target-duration (speedo--splits-duration target-splits))
         (segment-totals (make-list segment-count 0))
         rows)
    (dotimes (i segment-count)
      (let ((times (mapcar (lambda (a)
                             ;;@TODO: use a better time formatter
                             (let* ((splits (plist-get a :splits))
                                    (current-split (nth i splits))
                                    (duration (plist-get current-split :duration))
                                    (time (if duration
                                              (speedo--format-ms duration)
                                            speedo-text-place-holder)))
                               (cl-incf (nth i segment-totals) (or duration 0))
                               (if (or (equal a target) (not duration))
                                   time
                                 ;;@INCOMPLETE: what if target split doesn't have duration?
                                 ;;@INCOMPLETE: should be customizable via format string
                                 (concat (format "%-8s" time)
                                         " "
                                         (speedo--relative-time
                                          (plist-get (nth i target-splits) :duration)
                                          duration)))))
                           attempts))
            (name (list (propertize (plist-get (nth i segments) :name)
                                    'face
                                    (list :weight 'bold)))))
        (push (list i (vconcat name times)) rows)))
    (setq rows (mapcar
                (lambda (row)
                  (let ((id (car row)))
                    (list id (vconcat (cadr row)
                                      (list
                                       (let* ((duration (truncate
                                                         (/ (float (nth id segment-totals))
                                                            (length attempts))))
                                              (time (speedo--format-ms duration)))
                                         (concat (format "%-8s" time)
                                                 " "
                                                 (speedo--relative-time
                                                  (plist-get (nth id target-splits) :duration)
                                                  duration))))))))
                (nreverse rows))
          rows (append rows
                       (list
                        (list
                         (1+ segment-count)
                         (vconcat
                          (list "TOTAL")
                          (mapcar
                           (lambda (a)
                             (let* ((duration (speedo--splits-duration
                                               (plist-get a :splits))))
                               (concat (format
                                        "%-9s"
                                        (propertize (speedo--format-ms duration)
                                                    'face (cond
                                                           ((< duration target-duration)
                                                            'speedo-ahead)
                                                           ((> duration target-duration)
                                                            'speedo-behind)
                                                           (t 'speedo-neutral))))
                                       (unless (equal a (car attempts))
                                         (speedo--relative-time target-duration duration)))))
                           (append attempts
                                   `((:splits ((:duration ,(cl-reduce #'+ segment-totals
                                                                      :key (lambda (it) (/ it (length attempts)))))))))))))))))


(defun speedo-review (attempts)
  "Compare ATTEMPTS.
If ATTEMPTS is nil, prompt user."
  (interactive (list (speedo-review--attempts)))
  attempts)

(defun speedo-review-last (&optional n attempts)
  "Compare last N ATTEMPTS against target run."
  (interactive "N")
  (speedo-review (last (or attempts (speedo--attempts)) n)))


(provide 'speedo-review)
;;; speedo-review.el ends here
