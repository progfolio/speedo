;;; speedo-edit.el --- Editing for Speedo attempts   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: games, convenience

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
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)
(require 'cl-seq)
(require 'speedo)

(defvar speedo-edit-buffer "*speedo-edit*" "Buffer for editing attempts.")
(defvar speedo-edit-mode-map (make-sparse-keymap) "Keymap for `speedo-edit-mode'.")
(defvar speedo-edit-field-placeholder (propertize " " 'display
                                                  (propertize speedo-text-place-holder
                                                              'face 'speedo-behind)))
(defvar speedo-edit--in-progress nil "Non-nil when edit is in progress.")
(defvar-local speedo-edit--attempt nil "Buffer-local record of the attempt being edited.")

(define-widget 'speedo-field 'editable-field "A speedo edit field."
  :keymap speedo-edit-mode-map
  :help-echo nil
  :indent 0
  :offset 0
  :extra-offest 0)

(defun speedo-edit--validate-time-string (widget &rest _)
  "Validate WIDGET's time string."
  (let* ((input (replace-regexp-in-string "[^.0-:]*" "" (widget-value widget)))
         (speedo--time-formatter
          (lambda (&rest args)
            (replace-regexp-in-string
             "\\(?:^\\(?:0?0:\\(?:00:\\)?\\)\\)"
             ""
             (apply #'format "%d:%02d:%02d.%d" args))))
         (val (ignore-errors
                (speedo--format-ms (speedo--time-string-to-ms input)))))
    (widget-value-set widget
                      (cond
                       ((or (string-empty-p input) (null val))
                        speedo-edit-field-placeholder)
                       (t val)))))

(defun speedo-edit--validate-mistakes (widget &rest _)
  "Validate WIDGET's mistakes string."
  (widget-value-set
   widget
   (string-join
    (mapcar (lambda (time)
              (if-let ((valid (ignore-errors
                                (speedo--time-string-to-ms
                                 (replace-regexp-in-string "[^.0-:]*" "" time)))))
                  (speedo--format-ms valid)
                (propertize
                 time
                 'invalid t
                 'display (propertize
                           time
                           'face
                           '(:inherit speedo-behind :weight extrabold)))))
            (split-string (widget-value widget) "," 'omit-nulls "[[:space:]]+"))
    ", ")))

(defun speedo-edit--validate-alias (widget &rest _)
  "Validate WIDGET's alias string."
  (let ((input (string-trim (widget-value widget))))
    (widget-value-set widget
                      (cond
                       ((or (string-empty-p input) (null input))
                        speedo-edit-field-placeholder)
                       (t input)))))

(defun speedo-edit--validate-start-time (widget &rest _)
  "Validate WIDGET's start time."
  (widget-value-set
   widget
   (let ((input
          (replace-regexp-in-string "[[:space:]]+" " "
                                    (string-trim (widget-value widget)))))
     (if (string-match-p (rx bol
                             (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
                             " "
                             (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                         input)
         input
       (speedo--ms-to-date (plist-get speedo-edit--attempt :start))))))

(declare-function speedo-delete-attempts "speedo-commands")
(declare-function speedo-review--repeat-command "speedo-review")
(defvar speedo-review--last-command)
(defun speedo-edit-delete ()
  "Delete current attempt."
  (interactive)
  (unless (string= (buffer-name) speedo-edit-buffer)
    (user-error "Not in speedo editing buffer"))
  (when (yes-or-no-p "Delete current attempt?")
    (speedo-delete-attempts (list speedo-edit--attempt))
    (setq speedo-edit--in-progress nil)
    (kill-this-buffer)
    (when speedo-review--last-command
      (speedo-review--repeat-command speedo-review--last-command))))

;;;###autoload
(defun speedo-edit-new ()
  "Add a new attempt to currently loaded Speedo DB."
  (interactive)
  (speedo--ensure-data)
  (speedo-edit-attempt (list :start (speedo--timestamp))))

(defvar speedo--attempt-in-progress)
;;;###autoload
(defun speedo-edit-attempt (attempt)
  "Edit ATTEMPT."
  (interactive (list (speedo-read-attempt)))
  (when speedo--attempt-in-progress
    (user-error "Cannot edit while attempt in progress"))
  (setq speedo-edit--in-progress t)
  (switch-to-buffer speedo-edit-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let* ((segments (plist-get (speedo--ensure-data) :segments))
         (longest-name (car
                        (cl-sort
                         (mapcar (lambda (s) (length (plist-get s :name))) segments)
                         #'>)))
         (splits (plist-get attempt :splits))
         (total 0))
    (widget-create 'speedo-field
                   :type 'start
                   :action #'speedo-edit--validate-start-time
                   :format "Start: %v "
                   (speedo--ms-to-date (plist-get attempt :start)))
    (widget-insert "\n ")
    (dotimes (i (length segments))
      (let ((split (nth i splits)))
        (widget-create 'speedo-field
                       :type 'segment
                       :index i
                       :action #'speedo-edit--validate-time-string
                       :format (concat
                                (format
                                 (concat "%-" (number-to-string longest-name) "s")
                                 (propertize (plist-get (nth i segments) :name)
                                             'read-only t
                                             'segment-name t))
                                " %v ")
                       (if-let ((duration (plist-get split :duration)))
                           (speedo--format-ms (setq total (+ total duration)))
                         speedo-edit-field-placeholder))))
    (widget-insert "\n")
    (widget-create 'speedo-field
                   :type 'alias
                   :action #'speedo-edit--validate-alias
                   :format "Alias: %v "
                   (or (plist-get attempt :alias)
                       speedo-edit-field-placeholder))
    (widget-create 'speedo-field
                   :type 'tags
                   :format "Tags: %v "
                   (if-let ((tags (plist-get attempt :tags)))
                       (string-join tags ", ")
                     speedo-edit-field-placeholder))
    (widget-insert "\n")
    (widget-create 'speedo-field
                   :type 'mistakes
                   :action #'speedo-edit--validate-mistakes
                   :format "Mistakes: %v "
                   (string-join
                    (apply #'append
                           (mapcar (lambda (split)
                                     (mapcar #'speedo--format-ms
                                             (plist-get split :mistakes)))
                                   splits))
                    ", "))
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)
    (speedo-edit-mode)
    (setq-local speedo-edit--attempt attempt)
    (setq header-line-format
          (propertize
           (format "Speedo Editing: %s"
                   (or (plist-get speedo-edit--attempt :alias)
                       (speedo--ms-to-date (plist-get speedo-edit--attempt :start))))
           'face 'speedo-header-game-info))))

(defun speedo-edit--clear-field-maybe ()
  "Clear widget field if it's a placeholder when focused."
  (let* ((widget (widget-at (point)))
         (val (widget-value widget)))
    (when (string-empty-p (string-trim val))
      (widget-value-set widget ""))))

(defun speedo-edit--focus (direction n)
  "Focus Nth DIRECTION field."
  (save-excursion (when (widget-at (point)) (widget-field-activate (point))))
  (funcall (eval `(function ,(intern (format "widget-%s" direction)))) n)
  (speedo-edit--clear-field-maybe))

(defun speedo-edit-forward (&optional n)
  "Move forward N fields."
  (interactive "p")
  (speedo-edit--focus 'forward n))

(defun speedo-edit-backward (&optional n)
  "Move backward N fields."
  (interactive "p")
  (speedo-edit--focus 'backward n))

(defun speedo--edit-replace-or-append-attempt (data target new)
  "Return copy of DATA with TARGET replacing NEW if it is found.
Else append NEW to DATA.
Return DATA."
  (setq data (copy-tree data))
  (let ((attempts (plist-get data :attempts)))
    (if-let ((index (cl-position target attempts :test #'equal)))
        (setf (nth index (plist-get data :attempts)) new)
      (setq data (plist-put data :attempts
                            (append attempts (list new))))))
  data)

(declare-function speedo-review "speedo-review" (&optional attempts header))
(defun speedo-edit-finalize ()
  "Finalize the edit."
  (interactive)
  (unless (derived-mode-p 'speedo-edit-mode) (user-error "Not in speedo edit buffer"))
  (let ((attempt nil)
        (splits nil)
        (segments (plist-get speedo--data :segments))
        (total 0)
        (not-end t))
    (save-excursion
      (goto-char (point-min))
      (while not-end
        (end-of-line)
        ;;@DECOMPOSE: logic in speedo.el which creates a template attempt?
        (when-let ((w (widget-at (point)))
                   (type (widget-get w :type))
                   (val (progn (save-excursion (widget-field-activate (point)))
                               (string-trim (widget-value w)))))
          (pcase type
            ('segment
             (unless (string-empty-p val)
               ;; Convert absoulte time into duration
               (let ((duration (- (speedo--time-string-to-ms val) total)))
                 (setq total (+ total duration))
                 (push (list :segment
                             (plist-get (nth (widget-get w :index) segments)
                                        :name)
                             :duration duration)
                       splits))))
            ('start (setq attempt (plist-put attempt :start
                                             (speedo--date-to-ms val))))
            ('alias (setq attempt (plist-put attempt :alias (unless (string-empty-p val) val))))
            ('tags  (setq attempt (plist-put attempt :tags
                                             (mapcar #'string-trim
                                                     (split-string val ",")))))
            ('mistakes (let ((mistakes (mapcar #'speedo--time-string-to-ms
                                               (split-string val "," 'omit-nulls "[[:space:]]")))
                             (total 0))
                         (dolist (split (reverse splits))
                           (when-let ((duration (plist-get split :duration))
                                      (mistakes
                                       (progn
                                         (setq total (+ total duration))
                                         (cl-remove-if (lambda (mistake)
                                                         (or (> mistake total)
                                                             (<= mistake (- total duration))))
                                                       mistakes))))
                             (setf split (plist-put split :mistakes mistakes))))))
            (_ (error "Uknown widget type!"))))
        (setq not-end (zerop (forward-line 1)))))
    (setq attempt (plist-put attempt :splits (reverse splits)))
    (setq speedo--data (speedo--edit-replace-or-append-attempt speedo--data speedo-edit--attempt attempt))
    (setq speedo-edit--in-progress nil)
    (message "Attempt saved in memory.")
    (kill-buffer)
    (if speedo-review--last-command
        (speedo-review--repeat-command speedo-review--last-command)
      (speedo-review (list attempt) "Last Edit"))))

(defun speedo-edit-abort ()
  "Abort the current edit."
  (interactive)
  (when (buffer-live-p (get-buffer speedo-edit-buffer))
    ;; Since `speedo-edit--attempt' is buffer-local, we shouldn't have to reset it.
    (kill-buffer speedo-edit-buffer)
    (setq speedo-edit--in-progress nil)
    (message "Speedo edit aborted")))

(define-derived-mode speedo-edit-mode fundamental-mode "speedo-edit"
  "Major mode for editing speedo attempts.

\\{speedo-edit-mode-map}"
  (when speedo-highlight-line
    (face-remap-set-base 'hl-line nil)
    (face-remap-add-relative 'hl-line 'speedo-hl-line)
    (hl-line-mode))
  (setq buffer-face-mode-face 'speedo-default
        default-directory (file-name-directory speedo--data-file))
  (add-hook 'kill-emacs-hook  #'speedo--ask-to-save)
  (add-hook 'kill-buffer-hook #'speedo--confirm-kill-buffer nil t)
  (buffer-face-mode))

(define-key speedo-edit-mode-map (kbd "<tab>")     'speedo-edit-forward)
(define-key speedo-edit-mode-map (kbd "<backtab>") 'speedo-edit-backward)
(define-key speedo-edit-mode-map (kbd "C-c C-c")   'speedo-edit-finalize)
(define-key speedo-edit-mode-map (kbd "C-c C-k")   'speedo-edit-abort)
(define-key speedo-edit-mode-map (kbd "C-c C-d")   'speedo-edit-delete)

(provide 'speedo-edit)
;;; speedo-edit.el ends here
