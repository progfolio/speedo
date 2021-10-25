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

(defvar speedo-review--header nil
  "Used to set custom `header-line-format' during comparisons.")
(defvar speedo-review--totals-data nil "Workaround for post sorting data insertion.")
(defvar speedo-review-buffer   (concat speedo-buffer "<review>"))
(defvar speedo-review-mode-map (make-sparse-keymap) "Keymap for `speedo-review-mode'.")

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
           (or collection (plist-get speedo--data :attempts)
               (user-error (if speedo--data "No attempts in speedo DB." "Speedo DB loaded.")))))
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
                                    (mapcar (lambda (time)
                                              (abs (- average-relative time)))
                                            (plist-get row :relatives))))
                               (/ 1.0 (/ (cl-reduce #'+ deviations)
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

(defun speedo-review--rows (data)
  "Return table rows from DATA."
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
   data))

(defun speedo-review--sort-consistencies (a b)
  "Sort table rows A and B by consistency."
  (let ((a (cadr a))
        (b (cadr b)))
    (< (string-to-number (aref a (1- (length a))))
       (string-to-number (aref b (1- (length b)))))))

(defvar speedo-review-include-totals t
  "When non-nil, include total attempt durations at bottom of data table.")

(defun speedo-review--insert-totals (rows)
  "Insert totals run for ROWS in review buffer."
  (with-current-buffer speedo-review-buffer
    (save-excursion
      (with-silent-modifications
        (goto-char (point-max))
        (end-of-line)
        (let* ((totals
                (apply
                 #'cl-mapcar
                 `((lambda (&rest durations) (apply #'+ (or (delq nil durations) -1)))
                   ,@(mapcar (lambda (row) (plist-get row :durations)) rows))))
               (basis (car totals))
               (props (save-excursion
                        (forward-line -1)
                        (end-of-line)
                        (let ((current-line (line-number-at-pos))
                              (on-same-line-p t)
                              p prop)
                          (while (and on-same-line-p
                                      (setq prop (text-property-search-backward 'display)))
                            (if (eq (line-number-at-pos) current-line)
                                (push (prop-match-value prop) p)
                              (setq on-same-line-p nil)))
                          p)))
               (average-total
                (ignore-errors
                  (cl-reduce
                   #'+ rows
                   :key (lambda (r) (plist-get r :average-duration))))))
          (setq totals (append totals (list average-total)))
          (insert (propertize " " 'display (pop props))
                  (propertize "Totals" 'face '(:weight bold)))
          (dotimes (i (length totals))
            (let ((total (nth i totals)))
              (insert (propertize " " 'display (pop props)))
              (insert (if (< total 0) ;;no durations
                          speedo-text-place-holder
                        (concat
                         (format "%-8s"
                                 (propertize (speedo--format-ms total)
                                             'face (cond
                                                    ((> total basis) 'speedo-behind)
                                                    ((< total basis) 'speedo-ahead)
                                                    (t 'speedo-neutral))))
                         (unless (or (zerop i) (null basis))
                           (speedo--relative-time basis (nth i totals)))))))))))))

(defun speedo-review--ui-init (attempts)
  "Initialize comparison UI format for ATTEMPTS."
  (with-current-buffer (get-buffer-create speedo-review-buffer)
    (let* ((segment-col
            (list
             "Segment"
             (max
              (floor
               (* 1.2
                  (car (cl-sort (mapcar (lambda (segment) (length (plist-get segment :name)))
                                        (plist-get speedo--data :segments))
                                #'>))))
              8)
             t))
           (target-attempt (car attempts))
           (row-data (setq speedo-review--totals-data
                           (speedo-review--row-data attempts)))
           (rows (speedo-review--rows row-data)))
      (setq tabulated-list-entries rows
            tabulated-list-format
            (vconcat
             (list (list "ID" 4 (lambda (a b) (< (car a) (car b)))))
             (list segment-col)
             (mapcar (lambda (a)
                       (let ((alias (or (speedo--plist-get* a :alias)
                                        (format-time-string
                                         "%Y-%m-%d %I:%M%p  "
                                         (/ (plist-get a :start) 1000)))))
                         (when (equal a target-attempt)
                           (setq alias (propertize alias 'face '(:weight bold))))
                         (list alias (1+ (length alias)))))
                     attempts)
             (list (list "Average" 20))
             (list (list "Consistency" 20 #'speedo-review--sort-consistencies))))
      ;;commands are responsible for setting `speedo-review--header'
      (setq tabulated-list-use-header-line nil)
      (setq header-line-format speedo-review--header
            speedo-review--header nil)
      (unless (derived-mode-p 'tabulated-list-mode) (tabulated-list-mode))
      (tabulated-list-init-header)
      (advice-add 'tabulated-list-print :after 'speedo-review--print-totals-maybe)
      (tabulated-list-print 'remember-pos))))

(defun speedo-review--print-totals-maybe (&rest _)
  "Hack to insert info into buffer post sorting.
If `tabulated-list-mode' offered a post-print hook, we could avoid this."
  (when (and (derived-mode-p 'speedo-mode) speedo-review-include-totals)
    (speedo-review--insert-totals speedo-review--totals-data)))

;;;###autoload
(defun speedo-review (attempts &optional header)
  "Compare ATTEMPTS.
If ATTEMPTS is nil, prompt user.
HEADER is shown in the review buffer."
  (interactive (list (speedo-review-read-attempts)))
  (setq speedo-review--header
        (or header
            (list (speedo--header-game-info)
                  (format " %d Attempts" (length attempts)))))
  (speedo-review--ui-init attempts)
  (display-buffer speedo-review-buffer))

;;;###autoload
(defun speedo-review-last-attempts (&optional n attempts header)
  "Compare last N ATTEMPTS against target run.
HEADER is displayed in review buffer."
  (interactive "NLast N attempts?: ")
  (let ((attempts (last (or attempts (speedo--attempts)) n)))
    (speedo-review attempts (list (speedo--header-game-info)
                                  (or header
                                      (format " Last %d Attempts" (length attempts)))))))

;;;###autoload
(defun speedo-review-last-runs (&optional n attempts header)
  "Compare last N complete ATTEMPTS.
HEADER is displayed in review buffer."
  (interactive "NLast N runs?: ")
  (let ((attempts (nreverse (last (cl-remove-if-not #'speedo--attempt-complete-p
                                                    (or attempts (speedo--attempts)))
                                  n))))
    (speedo-review attempts (list (speedo--header-game-info)
                                  (or header
                                      (format " Last %d Runs" (length attempts)))))))

;;;###autoload
(defun speedo-review-top-runs (&optional n attempts)
  "Compare top N complete ATTEMPTS."
  (interactive "NHow many runs?: ")
  (let* ((runs (cl-sort (or attempts (speedo--attempts #'speedo--attempt-incomplete-p))
                        #'<
                        :key (lambda (a) (speedo--splits-duration (plist-get a :splits)))))
         (top (cl-subseq runs 0 (min n (length runs)))))
    (speedo-review top (list (speedo--header-game-info)
                             (format " Top %d Runs" (length top))))))

(provide 'speedo-review)
;;; speedo-review.el ends here