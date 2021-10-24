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

(defcustom speedo-review-include-target t
  "If non-nil, consider `speedo--comparison-target' implicit target of comparisons."
  :type  'boolean
  :group 'speedo)

(defun speedo-review-read-attempts (&optional collection)
  "Return a list of attempts from COLLECTION.
If COLLECTION is nil, use `speedo--data' or throw an error."
  (let* ((candidates
          (mapcar
           (lambda (attempt)
             (cons (string-trim
                    (string-join
                     (list
                      (format-time-string "%Y-%m-%d %I:%M%p"
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
           (or collection (plist-get speedo--data :attempts))))
         (selections (delete-dups (completing-read-multiple "Attempts: " candidates))))
    (mapcar (lambda (selection) (alist-get selection candidates nil nil #'string=))
            selections)))

(defun speedo-review--row-data (attempts)
  "Compute row data for ATTEMPTS.
Returns a plist of form:
\(:id :name :durations :average-durations :average-relative :consistency)."
  (let* ((segments          (plist-get speedo--data :segments))
         (segment-count     (length segments))
         (target            (car attempts))
         (target-splits     (plist-get target :splits))
         (rows              nil))
    (dotimes (i segment-count)
      (let* ((segment (nth i segments))
             (target-split (nth i target-splits))
             (target-split-duration (plist-get target-split :duration)))
        (push
         (list :id i
               :name (plist-get segment :name)
               :durations
               (mapcar (lambda (attempt)
                         (plist-get (nth i (plist-get attempt :splits))
                                    :duration))
                       attempts)
               :relatives
               (when target-split-duration
                 (mapcar (lambda (attempt)
                           (ignore-errors
                             (- target-split-duration
                                (plist-get (nth i (plist-get attempt :splits))
                                           :duration))))
                         attempts)))
         rows)))
    (dolist (row rows)
      (when-let ((durations (plist-get row :durations))
                 (full-set-p (not (member nil durations))))
        (setf row (plist-put row :average-duration
                             (/ (cl-reduce #'+ durations)
                                (length durations)))))
      (when-let ((relatives (plist-get row :relatives))
                 (full-set-p (not (member nil relatives))))
        (setf row (plist-put row :average-relative
                             (/ (cl-reduce #'+ relatives)
                                (length relatives)))))
      (when-let ((average-relative (plist-get row :average-relative)))
        ;;compute mean absolute deviation to measure consistency
        (setf row (plist-put row :consistency
                             (let ((deviations
                                    (mapcar (lambda (time) (abs (- average-relative time)))
                                            (plist-get row :relatives))))
                               (/ 1.0
                                  (/ (cl-reduce #'+ deviations)
                                     (length deviations))))))))
    ;;normalize consistency values to rankings
    (setq rows (nreverse rows))
    (let* ((sorted (cl-sort (cl-remove-if-not (lambda (r) (plist-get r :consistency))
                                              (copy-tree rows))
                            #'< :key (lambda (r) (plist-get r :consistency))))
           (count (1+ (length sorted))))
      (dolist (row sorted)
        (let ((id (plist-get row :id)))
          (setf (nth id rows) (plist-put (nth id rows) :consistency (cl-decf count))))))
    rows))

(defun speedo-review--rows (attempts)
  "Return table rows for ATTEMPTS."
  (let ((rows (speedo-review--row-data attempts)))
    (mapcar
     (lambda (r)
       (let ((id (plist-get r :id)))
         (list id
               (vconcat (list (number-to-string (1+ id)))
                        (list (plist-get r :name))
                        (let ((times)
                              (durations (plist-get r :durations))
                              (relatives (plist-get r :relatives)))
                          (dotimes (i (length durations))
                            (push (concat
                                   (format "%-8s"
                                           (if-let ((duration (nth i durations)))
                                               (speedo--format-ms duration)
                                             speedo-text-place-holder))
                                   (unless (zerop i)
                                     (if-let ((relative (nth i relatives)))
                                         (concat " "  (speedo--relative-time relative 0)))))
                                  times))
                          (nreverse times))
                        (list
                         (concat
                          (format "%-8s"
                                  (if-let ((average-duration (plist-get r :average-duration)))
                                      (speedo--format-ms average-duration)
                                    speedo-text-place-holder))
                          (when-let ((average-relative (plist-get r :average-relative)))
                            (speedo--relative-time average-relative 0))))
                        (list
                         (number-to-string
                          (if-let ((consistency (plist-get r :consistency)))
                              consistency
                            -1)))))))
     rows)))

(defun speedo-review--sort-consistencies (a b)
  "Sort table rows A and B by consistency."
  (let ((a (cadr a))
        (b (cadr b)))
    (< (string-to-number (aref a (1- (length a))))
       (string-to-number (aref b (1- (length b)))))))

(defun speedo-review--ui-init (attempts)
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
             (list (list "ID" 4 (lambda (a b) (< (car a) (car b)))))
             (list segment-col)
             (mapcar (lambda (a)
                       (let ((alias (or (speedo--plist-get* a :alias)
                                        (format-time-string
                                         "%Y-%m-%d %I:%M%p"
                                         (/ (plist-get a :start) 1000)))))
                         (when (equal a target-attempt)
                           (setq alias (propertize alias 'face '(:weight bold))))
                         (list alias (1+ (length alias)))))
                     attempts)
             (list (list "Average" 20))
             (list (list "Consistency" 20 #'speedo-review--sort-consistencies)))
            tabulated-list-entries (speedo-review--rows attempts)))
    ;;(speedo--review-header)
    (tabulated-list-mode)
    (tabulated-list-init-header)
    (let ((tabulated-list-use-header-line nil))
      (tabulated-list-print 'remember-pos 'update))))

;; (defun speedo--review-header ()
;;   "Set the comparison header."
;;   (with-current-buffer speedo-buffer
;;     (setq header-line-format
;;           (list (speedo--header-game-info) " "
;;                 '(:propertize "Comparisons" face speedo-header-game-info)))))



(defun speedo-review (attempts)
  "Compare ATTEMPTS.
If ATTEMPTS is nil, prompt user."
  (interactive (list (speedo-review-read-attempts)))
  (speedo-review--ui-init attempts)
  (display-buffer speedo-buffer))

(defun speedo-review-last (&optional n attempts)
  "Compare last N ATTEMPTS against target run."
  (interactive "N")
  (let ((attempts (last (or attempts (speedo--attempts)) n)))
    (speedo-review attempts)))

(defun speedo-review-top-runs (&optional n attempts)
  "Compare top N complete ATTEMPTS."
  (interactive "N")
  (let ((runs (cl-sort (or attempts (speedo--attempts #'speedo--attempt-incomplete-p))
                       #'<
                       :key (lambda (a) (speedo--splits-duration (plist-get a :splits))))))
    (speedo-review (cl-subseq runs 0 (min n (length runs))))))

(provide 'speedo-review)
;;; speedo-review.el ends here
