;;; speedo-compact.el --- compact split display minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Nicholas Vollmer

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

(defun speedo--compact-filter (segments)
  "Filter SEGMENTS for compact mode."
  (let* ((segment-count (length (plist-get speedo--data :segments)))
         (limit (min (or speedo-compact-segment-limit 10) segment-count))
         (start (min
                 (max (- speedo--segment-index (- limit 2)) 0)
                 (- segment-count limit)))
         (end (min (+ start (1- limit)) segment-count)))
    (append (cl-subseq segments start end)
            (when (< end segment-count) (last segments)))))

(defun speedo--compact-last-segment-separator ()
  "Insert `speedo-compact-separator'."
  (when-let ((it speedo-compact-separator))
    (with-current-buffer speedo-buffer
      (save-excursion
        (with-silent-modifications
          (goto-char (point-max))
          (if-let ((last (text-property-search-backward 'tabulated-list-id)))
              (let ((len (car
                          (cl-sort (mapcar (lambda (s) (length (plist-get s :name)))
                                           (plist-get speedo--data :segments))
                                   #'>))))
                (dotimes (i (length tabulated-list-format))
                  ;; Column widht specs
                  (setq len (+ len (cadr (aref tabulated-list-format i)))))
                (goto-char (1- (point)))
                (insert "\n"
                        (cond
                         ((characterp it) (make-string len it))
                         ((stringp it)    it)
                         ((functionp it)  (funcall it))
                         (t (signal 'wrong-type-error `((stringp functionp) ,it))))))
            (error "Unable to find last segment")))))))

;;;###autoload
(define-minor-mode speedo-compact-mode
  "Minor mode to display a compacted list of segments."
  :lighter " spc"
  (if speedo-compact-mode
      (progn
        (add-hook 'speedo-post-ui-display-hook #'speedo--compact-last-segment-separator)
        (advice-add 'speedo--timer-rows :filter-return #'speedo--compact-filter))
    (remove-hook 'speedo-post-ui-display-hook #'speedo--compact-last-segment-separator)
    (advice-remove 'speedo--timer-rows #'speedo--compact-filter))
  (speedo--display-ui))

(provide 'speedo-compact)

;;; speedo-compact.el ends here
