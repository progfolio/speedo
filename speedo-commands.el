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
;;; Commands
(require 'speedo)

(defun speedo-new-game (title &optional category dir &rest segments)
  "Write new database for game with TITLE to DIR.
DIR is optional and defaults to `speedo-directory'.
CATEGORY and SEGMENTS are added to the DB structure if provided.
When called interactivley, prompt for optional values."
  (interactive "stitle: \nscategory: ")
  (let ((file (expand-file-name
               (replace-regexp-in-string "\\(?:[[:space:]]+\\)" "-" title)
               (or dir speedo-directory)))
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

(defun speedo-next ()
  "Start the next segment or a new attempt."
  (interactive)
  (with-current-buffer speedo-buffer
    (if speedo--attempt-in-progress
        (let ((last (1- (length (plist-get speedo--current-attempt :splits)))))
          (speedo--split-end)
          (when (= speedo--segment-index last) (speedo--attempt-end)))
      (speedo--attempt-init))
    (when speedo--attempt-in-progress (cl-incf speedo--segment-index))
    (speedo--split-start)
    (speedo--display-ui)
    (speedo--update-header)
    (if speedo--attempt-in-progress
        (speedo--goto-index)
      (forward-line)
      (speedo--display-timers))))

(defun speedo-previous ()
  "Select the previous segment."
  (interactive)
  (with-current-buffer speedo-buffer
    (unless speedo--attempt-in-progress (user-error "No attempt in progress"))
    (when (= speedo--segment-index 0) (user-error "No previous segment"))
    ;; clear out attempt data for this split and the previous
    (let ((current (speedo--current-split)))
      (setf current (plist-put current :duration nil)))
    (cl-decf speedo--segment-index)
    (let ((current (speedo--current-split)))
      (setf current (plist-put current :duration nil)))
    (speedo--goto-index)
    (speedo--display-ui)))

(defun speedo-mistake ()
  "Record a mistake in the current split."
  (interactive)
  (if speedo--attempt-in-progress
      (let ((current (speedo--current-split)))
        (setf current
              (plist-put current :mistakes
                         (append  (plist-get current :mistakes)
                                  (list (- (speedo--timestamp)
                                           (plist-get speedo--current-attempt :start))))))
        (speedo--footer-mistakes)
        (message "mistake recorded"))
    (user-error "No run in progress")))

(defun speedo-reset ()
  "Reset the current attempt if it is in progress.
If no attempt is in progress, clear the UI times."
  (interactive)
  (when speedo--attempt-in-progress
    (setq speedo--current-attempt
          (plist-put speedo--current-attempt :reset
                     (- (speedo--timestamp)
                        (plist-get speedo--current-attempt :start))))
    (speedo--attempt-end))
  (setq speedo--segment-index -1
        speedo--current-attempt nil)
  (speedo--clear)
  (goto-char (point-min)))

(defun speedo-bury ()
  "Bury the `speedo-buffer'."
  (interactive)
  (with-current-buffer speedo-buffer
    (internal-show-cursor (selected-window) t) ;;show hidden cursor
    (bury-buffer)))

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
  (when speedo--attempt-in-progress
    (user-error "Cannot Load file while attempt is in progress"))
  (when (bound-and-true-p speedo-edit--in-progress)
    (user-error "Cannot Load file while attempt edit in progress"))
  (let ((file (or file (read-file-name "Splits file: " speedo-directory))))
    (when (and (speedo--data-modified-p)
               (y-or-n-p (format "%S modified. Save before loading %s? "
                                 speedo--data-file file)))
      ;; Force because we just checked for modifications above
      (speedo-save-file 'force))
    (speedo--load-file file)
    (unless hide
      (switch-to-buffer (get-buffer-create speedo-buffer))
      (speedo-mode)
      (goto-char (point-min)))))

;;;###autoload
(defun speedo ()
  "Open the splits buffer."
  (interactive)
  (if speedo--data
      (unless (string= (buffer-name (current-buffer)) speedo-buffer)
        (switch-to-buffer (get-buffer-create speedo-buffer) nil)
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

(provide 'speedo-commands)
;;; speedo-commands.el ends here
