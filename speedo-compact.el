;;; speedo-compact.el --- compact split display minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicholas Vollmer

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

(defun speedo--compact-filter (splits)
  "Filter SPLITS for compact mode."
  (let* ((split-count (length (plist-get speedo--data :segments)))
         (limit (min (or speedo-compact-segment-limit 10) split-count))
         (start (min
                 (max (- speedo--segment-index (- limit 2)) 0)
                 (- split-count limit)))
         (end (min (+ start (1- limit)) split-count)))
    (append (cl-subseq splits start end)
            (when (< end split-count) (last splits)))))

(defun speedo--compact-last-split-separator ()
  "Insert `speedo-compact-last-split-separator'."
  (when-let ((it speedo-compact-last-split-separator))
    (with-current-buffer speedo-buffer
      (save-excursion
        (with-silent-modifications
          (goto-char (point-max))
          (if-let ((last (text-property-search-backward 'tabulated-list-id)))
              (progn
                (goto-char (1- (point)))
                (insert "\n"
                        (cond
                         ((characterp it)
                          (make-string (- (line-end-position) (line-beginning-position))
                                       it))
                         ((stringp it)  it)
                         ((functionp it) (funcall it))
                         (t (signal 'wrong-type-error `((stringp functionp) ,it))))))
            (error "Unable to find last split")))))))

;;;###autoload
(define-minor-mode speedo-compact-mode
  "Minor mode to display a compacted list of splits."
  :lighter " spc"
  (if speedo-compact-mode
      (progn
        (add-hook 'speedo-post-ui-display-hook #'speedo--compact-last-split-separator)
        (advice-add 'speedo--timer-rows :filter-return #'speedo--compact-filter))
    (remove-hook 'speedo-post-ui-display-hook #'speedo--compact-last-split-separator)
    (advice-remove 'speedo--timer-rows #'speedo--compact-filter))
  (speedo--display-ui)
  (speedo--display-timers))

(provide 'speedo-compact)

;;; speedo-compact.el ends here
