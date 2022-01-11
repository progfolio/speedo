;;; speedo-commands.el --- Commands for speedo.el    -*- lexical-binding: t; -*-

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

;;;###autoload
(defun speedo-new-db (title &optional category dir &rest segments)
  "Write new database for game with TITLE to DIR.
DIR is optional and defaults to `speedo-directory'.
CATEGORY and SEGMENTS are added to the DB structure if provided.
When called interactivley, prompt for optional values."
  (interactive "stitle: \nscategory: ")
  (let* ((dir (expand-file-name (or dir speedo-directory default-directory)))
         (category (if (string-empty-p category) nil category))
         (default-file (let ((name
                              (replace-regexp-in-string
                               "\\(?:[[:space:]]+\\)" "-"
                               (concat title (when category (format "-%s" category)))))
                             (i 0)
                             (id ""))
                         (while (file-exists-p (expand-file-name name dir))
                           (setq name (string-remove-suffix id name)
                                 name (concat name
                                              (setq id (format "<%d>" (cl-incf i))))))
                         (concat name speedo--file-extension)))
         (file (let ((input (read-file-name "Write DB to: " dir nil nil default-file)))
                 (if (string-suffix-p speedo--file-extension input)
                     input
                   (concat input speedo--file-extension))))
         (index 0)
         segment)
    (unless (or segments (not (called-interactively-p 'interactive)))
      (while (not (string-empty-p
                   (setq segment (read-string (format "segment %d (empty to exit): "
                                                      (cl-incf index))))))
        (push segment segments))
      (setq segments (nreverse segments)))
    (speedo--write-data
     (list :title title
           :category category
           :segments (mapcar (lambda (segment) (list :name segment))
                             segments))
     file nil nil nil 'must-be-new)))

;;;###autoload
(defun speedo-import-splits (file)
  "Import splits from JSON FILE.
Assumes splits.io exchange fomat:
https://github.com/glacials/splits-io/tree/master/public/schema"
  (interactive "f")
  (if-let* ((data (ignore-errors
                    (json-parse-string
                     (with-current-buffer (find-file-noselect file 'nowarn 'raw)
                       (buffer-string))
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil)))
            (title (plist-get (plist-get data :game) :longname))
            (category (plist-get (plist-get data :category) :longname))
            (segments (mapcar (lambda (segment) (plist-get segment :name))
                              (plist-get data :segments))))
      (apply #'speedo-new-db (append (list title category nil) segments))
    (user-error "Unable to parse %S" file)))

(defun speedo-next ()
  "Start the next segment or a new attempt."
  (interactive)
  (with-current-buffer speedo-buffer
    (if (speedo--attempt-in-progress-p)
        (let ((last (1- (length (plist-get speedo--current-attempt :segments)))))
          (speedo--segment-end)
          (when (= speedo--segment-index last) (speedo--attempt-end)))
      (speedo--attempt-init))
    (when (speedo--attempt-in-progress-p) (cl-incf speedo--segment-index))
    (speedo--segment-start)
    (speedo--display-ui)
    (speedo--update-header)
    (if (speedo--attempt-in-progress-p)
        (speedo--goto-index)
      (forward-line))))

(defun speedo-previous ()
  "Select the previous segment."
  (interactive)
  (with-current-buffer speedo-buffer
    (unless (speedo--attempt-in-progress-p) (user-error "No attempt in progress"))
    (when (= speedo--segment-index 0) (user-error "No previous segment"))
    ;; clear out attempt data for this segment and the previous
    (let ((current (speedo--current-segment)))
      (setf current (plist-put current :duration nil)))
    (cl-decf speedo--segment-index)
    (let ((current (speedo--current-segment)))
      (setf current (plist-put current :duration nil)))
    (speedo--goto-index)
    (speedo--display-ui)))

(defun speedo-mistake ()
  "Record a mistake in the current segment."
  (interactive)
  (if (speedo--attempt-in-progress-p)
      (let ((current (speedo--current-segment)))
        (setf current
              (plist-put current :mistakes
                         (append  (plist-get current :mistakes)
                                  (list (- (speedo--timestamp)
                                           (plist-get speedo--current-attempt :start))))))
        (message "mistake recorded"))
    (user-error "No run in progress")))

(defun speedo-reset ()
  "Reset the current attempt if it is in progress.
If no attempt is in progress, clear the UI times."
  (interactive)
  (when (speedo--attempt-in-progress-p)
    (setq speedo--current-attempt
          (plist-put speedo--current-attempt :reset
                     (- (speedo--timestamp)
                        (plist-get speedo--current-attempt :start))))
    (speedo--attempt-end))
  (setq speedo--segment-index -1
        speedo--state 'pre
        speedo--current-attempt nil)
  (speedo--clear)
  (goto-char (point-min)))

;;;###autoload
(defun speedo-delete-attempts (attempts)
  "Delete ATTEMPTS from current DB."
  (interactive (list (speedo-read-attempt nil 'multiple)))
  (setq speedo--data (speedo--delete-attempts attempts speedo--data))
  (let ((len (length attempts)))
    (message "Deleted %d attempt%s" len (if (eq len 1) "" "s"))))

(defun speedo-quit-window ()
  "Quit the `speedo-buffer'."
  (interactive)
  (with-current-buffer speedo-buffer
    (internal-show-cursor (selected-window) t) ;;show hidden cursor
    (quit-window)))

(defun speedo-comparison-next (&optional n)
  "Compare against Nth next standard in `speedo-comparison-targets'."
  (interactive "p")
  (speedo--nth-target (or n 1)))

(defun speedo-comparison-previous (&optional n)
  "Compare against Nth next standard in `speedo-comparison-targets'."
  (interactive "p")
  (speedo--nth-target (- (or n 1))))

;;;###autoload
(defun speedo-load-file (&optional file hide)
  "Load a splits FILE.
If HIDE is non-nil, do not display `speedo-buffer' after loading."
  (interactive)
  (when (speedo--attempt-in-progress-p)
    (user-error "Cannot Load file while attempt is in progress"))
  (when (bound-and-true-p speedo-edit--in-progress)
    (user-error "Cannot Load file while attempt edit in progress"))
  (let ((file (or file (read-file-name "Splits file: " speedo-directory
                                       nil 'must-match nil
                                       #'speedo--db-file-p))))
    (when (and (speedo--data-modified-p)
               (yes-or-no-p (format "%S modified. Save before loading %s? "
                                    speedo--data-file file)))
      ;; Force because we just checked for modifications above
      (speedo-save-file 'force))
    (speedo--load-file file)
    (unless hide
      (pop-to-buffer (get-buffer-create speedo-buffer))
      (speedo-mode)
      (goto-char (point-min)))))

;;;###autoload
(defun speedo ()
  "Open the splits buffer."
  (interactive)
  (if speedo--data
      (unless (string= (buffer-name (current-buffer)) speedo-buffer)
        (pop-to-buffer (get-buffer-create speedo-buffer) nil)
        (when speedo-hide-cursor (speedo--hide-cursor))
        (unless (derived-mode-p 'speedo-mode) (speedo-mode)))
    (speedo-load-file
     (when speedo-default-splits-file
       (expand-file-name speedo-default-splits-file speedo-directory)))))

;;;###autoload
(defun speedo-save-file (&optional force)
  "Save `speedo--data' to `speedo--data-file'.
If FORCE is non-nil, save without checking if data has been modified."
  (interactive "P")
  (if (or force (speedo--data-modified-p))
      (speedo--write-data
       (speedo--convert-data speedo--data 'human)
       speedo--data-file)
    (message "(No changes need to be saved)")))

(declare-function speedo-edit-attempt "speedo-edit" (attempt))
(defun speedo-post-edit-last (attempt)
  "Update UI to display edited ATTEMPT."
  (setq speedo--current-attempt attempt
        speedo--time (speedo--segments-duration (plist-get attempt :segments)))
  (speedo)
  (speedo--display-ui)
  (speedo--update-header)
  (remove-hook 'speedo-edit-finalize-functions #'speedo-post-edit-last))

;;;###autoload
(defun speedo-edit-last-attempt ()
  "Edit most recent attempt."
  (interactive)
  (add-hook 'speedo-edit-finalize-functions #'speedo-post-edit-last)
  (if (speedo--attempt-in-progress-p)
      (user-error "Cannot edit while attempt in progress")
    (speedo-edit-attempt (speedo-target-last-attempt))))

(provide 'speedo-commands)
;;; speedo-commands.el ends here
