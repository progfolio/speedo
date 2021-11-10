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
(defvar speedo-review-include-mistakes t
  "When non-nil, include mistake counts for each segment.")
(defvar speedo-review-include-relative-times t
  "When non-nil, include relative-times for each segment.")
(defvar speedo-review-include-totals-row t
  "When non-nil, include total attempt durations at bottom of data table.")
(defvar speedo-review-include-average-column t
  "When non-nil, include Averages column in review.")
(defvar speedo-review-include-id-column t
  "When non-nil, include ID column in review.")
(defvar speedo-review-include-consistency-column t
  "When non-nil, include Consistency column in review.")
(defvar speedo-review-mode-map (make-sparse-keymap) "Keymap for `speedo-review-mode'.")
(defvar speedo-review-include-relative-split-times nil
  "When non-nil, split durations are displayed relative to start of the run.
Otherwise they are abolute times.")
(defvar speedo-review--attempts nil
  "Used to store attempts when manipulating views.")

(defun speedo-review--row-data (attempts)
  "Compute row data for ATTEMPTS.
Returns a plist of form:
\(:id :name :mistakes :durations :average-durations :average-relative :consistency)."
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
               :mistakes (mapcar (lambda (attempt)
                                   (length
                                    (plist-get (nth i (plist-get attempt :splits))
                                               :mistakes)))
                                 attempts)
               :durations (mapcar (lambda (attempt)
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
      (let ((mistakes (plist-get row :mistakes)))
        (setf row (plist-put row :average-mistakes (/ (cl-reduce #'+ mistakes)
                                                      (length mistakes)))))
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
             (vconcat
              (when speedo-review-include-id-column
                (list (number-to-string (1+ id))))
              (list (plist-get r :name))
              (let ((times)
                    (durations (plist-get r :durations))
                    (relatives (plist-get r :relatives)))
                (dotimes (i (length durations))
                  (push (concat
                         (format "%8s"
                                 (if-let ((duration (nth i durations)))
                                     (speedo--format-ms
                                      (if speedo-review-include-relative-split-times
                                          (speedo--splits-duration
                                           (cl-subseq
                                            (plist-get (nth i speedo-review--attempts)
                                                       :splits)
                                            0 (1+ id)))
                                        duration))
                                   speedo-text-place-holder))
                         (when (and speedo-review-include-relative-times
                                    (not (zerop i)))
                           (if-let ((relative (nth i relatives)))
                               (format  " %9s"  (speedo--relative-time relative 0))))
                         (when speedo-review-include-mistakes
                           (when-let ((mistakes (plist-get r :mistakes))
                                      (count (nth i mistakes))
                                      (basis (car mistakes)))
                             (format " %3s" (propertize
                                             (number-to-string count)
                                             'face
                                             (cond
                                              ((< count basis) 'speedo-ahead)
                                              ((> count basis) 'speedo-behind)
                                              (t 'speedo-neutral)))))))
                        times))
                (nreverse times))
              (when speedo-review-include-average-column
                (list
                 (concat
                  (format "%8s"
                          (if-let ((average-duration (plist-get r :average-duration)))
                              (speedo--format-ms
                               (if speedo-review-include-relative-split-times
                                   (cl-reduce
                                    #'+
                                    (cl-subseq data 0 (1+ id))
                                    :key (lambda (r) (or (plist-get r :average-duration) 0)))
                                 average-duration))
                            speedo-text-place-holder))
                  (when speedo-review-include-relative-times
                    (when-let ((average-relative (plist-get r :average-relative)))
                      (format " %9s" (speedo--relative-time average-relative 0))))
                  (when speedo-review-include-mistakes
                    (when-let ((average-mistakes (plist-get r :average-mistakes)))
                      (let ((basis (or (car (plist-get r :mistakes)) 0)))
                        (format " %3s"
                                (propertize
                                 (number-to-string average-mistakes)
                                 'face
                                 (cond
                                  ((< average-mistakes basis) 'speedo-ahead)
                                  ((> average-mistakes basis) 'speedo-behind)
                                  (t 'speedo-neutral))))))))))
              (when speedo-review-include-consistency-column
                (list
                 (if-let ((consistency (plist-get r :consistency)))
                     (number-to-string consistency)
                   speedo-text-place-holder)))))))
   data))

(defun speedo-review--sort-consistencies (a b)
  "Sort table rows A and B by consistency."
  (let* ((a (cadr a))
         (b (cadr b))
         (consistency-a (aref a (1- (length a))))
         (consistency-b (aref b (1- (length b)))))
    (cond
     ;; Ensure place holders are put at end of list by default
     ((string= consistency-a speedo-text-place-holder) nil)
     ((string= consistency-b speedo-text-place-holder) t)
     (t (< (string-to-number consistency-a)
           (string-to-number consistency-b))))))

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
                 `((lambda (&rest durations) (apply #'+ (or (delq nil durations)
                                                            (list -1))))
                   ,@(mapcar (lambda (row) (plist-get row :durations)) rows))))
               (want-average-p (and speedo-review-include-average-column
                                    (> (length totals) 1)))
               (mistakes
                (let ((m (apply
                          #'cl-mapcar
                          `((lambda (&rest mistakes) (apply #'+ mistakes))
                            ,@(mapcar (lambda (row) (plist-get row :mistakes))
                                      rows)))))
                  (setq m (append m
                                  (when want-average-p (list (/ (cl-reduce #'+ m)
                                                                (length m))))))
                  (let ((basis (car m)))
                    (mapcar (lambda (n)
                              (propertize (number-to-string n)
                                          'face (cond
                                                 ((< n basis) 'speedo-ahead)
                                                 ((> n basis) 'speedo-behind)
                                                 (t 'speedo-neutral))))
                            m))))
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
                (when want-average-p
                  (ignore-errors
                    (cl-reduce
                     #'+ rows
                     :key (lambda (r) (plist-get r :average-duration)))))))
          (when want-average-p (setq totals (append totals (list average-total))))
          (when speedo-review-include-id-column
            (insert (propertize " " 'display (pop props))))
          (insert (propertize "Totals" 'face '(:weight bold)))
          (dotimes (i (length totals))
            (let ((total (nth i totals))
                  ;; Skip ID column
                  (col (car (aref tabulated-list-format (1+ i)))))
              (insert (propertize " " 'display (pop props)
                                  'tabulated-list-column-name col))
              (insert (propertize
                       (if (or (null total) (< total 0)) ;;no durations
                           speedo-text-place-holder
                         (concat
                          (format "%8s"
                                  (propertize (speedo--format-ms total)
                                              'face (cond
                                                     ((> total basis) 'speedo-behind)
                                                     ((< total basis) 'speedo-ahead)
                                                     (t 'speedo-neutral))))
                          (when (and speedo-review-include-relative-times
                                     basis
                                     (> i 0))
                            (format " %9s"
                                    (speedo--relative-time basis (nth i totals))))
                          (when speedo-review-include-mistakes
                            (format " %3s" (nth i mistakes)))))
                       'tabulated-list-column-name col)))))))))

(defun speedo-review--sort-attempt-column (a b)
  "Sort attempt column rows A and B by split durations."
  (let* ((name (car tabulated-list-sort-key))
         (col-index
          (cl-position name (cl-coerce tabulated-list-format 'list)
                       :test #'string= :key #'car))
         (a (speedo--time-string-to-ms (aref (cadr a) col-index)))
         (b (speedo--time-string-to-ms (aref (cadr b) col-index))))
    (cond
     ;; Duration of 0 (incomplete attempt) is moved to end of list
     ((zerop a) nil)
     ((zerop b) t)
     (t (< a b)))))

(defun speedo-review--attempt-columns (attempts)
  "Return list of columns for ATTEMPTS."
  (let ((target-attempt (car attempts)))
    (mapcar
     (lambda (a)
       (let ((alias (or (speedo--plist-get* a :alias)
                        (format-time-string
                         "%Y-%m-%d %I:%M%p"
                         (/ (plist-get a :start) 1000)))))
         (when (equal a target-attempt)
           (setq alias (propertize alias 'face '(:weight bold))))
         (list (propertize alias 'speedo-attempt a)
               27 #'speedo-review--sort-attempt-column)))
     attempts)))

(defun speedo-review--segment-col-length ()
  "Return length of longest segment name with padding."
  (let ((longest (car
                  (cl-sort (mapcar (lambda (segment) (length (plist-get segment :name)))
                                   (plist-get speedo--data :segments))
                           #'>))))
    (max (floor (* 1.2 longest)) (length "Segment"))))

(defun speedo-review--columns (attempts)
  "Return Column format for ATTEMPTS.
Used as `tabulated-list-format'."
  (vconcat
   (when speedo-review-include-id-column
     (list (list "ID" 4 (lambda (a b) (< (car a) (car b))))))
   (list (list "Segment" (speedo-review--segment-col-length) t))
   (speedo-review--attempt-columns attempts)
   (when speedo-review-include-average-column
     (list (list "Average" 27 #'speedo-review--sort-attempt-column)))
   (when speedo-review-include-consistency-column
     (list (list "Consistency" 20 #'speedo-review--sort-consistencies)))))

(defun speedo-review--ui-init (attempts &optional cache)
  "Initialize comparison UI format for ATTEMPTS.
If CACHE is non-nil, the attempts are saved in `speedo-review--attempts'."
  (with-current-buffer (get-buffer-create speedo-review-buffer)
    (when cache (setq speedo-review--attempts attempts))
    (let* ((more-than-one-attempt-p (> (length  attempts) 1))
           (speedo-review-include-average-column
            (and more-than-one-attempt-p speedo-review-include-average-column))
           (speedo-review-include-consistency-column
            (and more-than-one-attempt-p speedo-review-include-consistency-column))
           (row-data (setq speedo-review--totals-data
                           (speedo-review--row-data attempts)))
           (rows (speedo-review--rows row-data)))
      ;;commands are responsible for setting `speedo-review--header'
      (unless (derived-mode-p 'speedo-review-mode) (speedo-review-mode))
      (setq tabulated-list-entries rows
            tabulated-list-format (speedo-review--columns attempts)
            tabulated-list-use-header-line nil)
      ;; Clear the sort-key if the column it refrences has been removed.
      (unless (cl-member (car tabulated-list-sort-key)
                         (cl-coerce tabulated-list-format 'list)
                         :test #'string= :key #'car)
        (setq tabulated-list-sort-key nil))
      (tabulated-list-init-header)
      (advice-add 'tabulated-list-print :after 'speedo-review--print-totals-maybe)
      (tabulated-list-print 'remember-pos)
      (setq header-line-format speedo-review--header))))

(defun speedo-review--print-totals-maybe (&rest _)
  "Hack to insert info into buffer post sorting.
If `tabulated-list-mode' offered a post-print hook, we could avoid this."
  (when (and (derived-mode-p 'speedo-review-mode) speedo-review-include-totals-row)
    (speedo-review--insert-totals speedo-review--totals-data)))

;;;###autoload
(defun speedo-review (&optional attempts header)
  "Compare ATTEMPTS.
If ATTEMPTS is nil, prompt user.
HEADER is shown in the review buffer."
  (interactive)
  (let ((attempts (or attempts (speedo-read-attempt nil 'multiple))))
    (setq speedo-review--header
          (or header
              (list (speedo--header-game-info)
                    (let ((len (length attempts)))
                      (propertize (format " %d Attempt%s" len
                                          (if (> len 1) "s" ""))
                                  'face 'speedo-neutral)))))
    (speedo-review--ui-init attempts 'cache)
    (display-buffer speedo-review-buffer)))

;;;###autoload
(defun speedo-review-last-attempts (&optional n attempts header)
  "Compare last N ATTEMPTS.
If N is positive, ATTEMPTS are sorted most recent first.
If N is negative, they are sorted most recent last.
HEADER is displayed in review buffer."
  (interactive "p")
  (speedo--ensure-data)
  (let ((attempts (last (or attempts (speedo--attempts)) (abs n))))
    (when (> n 0) (setq attempts (reverse attempts)))
    (speedo-review attempts
                   (list (speedo--header-game-info)
                         (or header
                             (propertize
                              (let ((len (length attempts)))
                                (if (eq len 1)
                                    "Last Attempt"
                                  (format " Last %d Attempts" len)))
                              'face 'speedo-ahead))))))

;;;###autoload
(defun speedo-review-last-runs (&optional n attempts header)
  "Compare last N complete ATTEMPTS.
If N is positive, ATTEMPTS are sorted most recent first.
If N is negative, they are sorted most recent last.
HEADER is displayed in review buffer."
  (interactive "p")
  (speedo--ensure-data)
  (let ((attempts (last (cl-remove-if-not #'speedo--attempt-complete-p
                                          (or attempts (speedo--attempts)))
                        (abs n))))
    (when (> n 0) (setq attempts (reverse attempts)))
    (speedo-review attempts (list (speedo--header-game-info)
                                  (or header
                                      (propertize
                                       (let ((len (length attempts)))
                                         (if (eq len 1)
                                             " Last Run"
                                           (format " Last %d Runs" len)))
                                       'face 'speedo-ahead))))))

;;;###autoload
(defun speedo-review-top-runs (&optional n attempts)
  "Compare top N complete ATTEMPTS.
If N is positive, ATTEMPTS are sorted most recent first.
If N is negative, they are sorted most recent last."
  (interactive "p")
  (speedo--ensure-data)
  (let* ((runs (cl-sort (or attempts (speedo--attempts #'speedo--attempt-incomplete-p))
                        #'<
                        :key (lambda (a) (speedo--splits-duration (plist-get a :splits)))))
         (top (cl-subseq runs 0 (min (abs n) (length runs)))))
    (when (< n 0) (setq top (reverse top)))
    (speedo-review top (list (speedo--header-game-info)
                             (propertize
                              (let ((len (length top)))
                                (if (eq len 1)
                                    " Personal Best"
                                  (format " Top %d Runs" len)))
                              'face 'speedo-ahead)))))

(defmacro speedo-review-def-col-toggle (name)
  "Define a toggle command for column represented by NAME."
  (declare (indent 1))
  (setq name (downcase name))
  (let ((var (intern (format "speedo-review-include-%s-column" name))))
    `(defun ,(intern (concat "speedo-review-toggle-" name "-column")) ()
       ,(format "Toggle display of the %s column in `speedo-review-buffer'."
                (upcase name))
       (interactive)
       (setq ,var (not ,var))
       (speedo-review--ui-init speedo-review--attempts))))

(defmacro speedo-review-def-col-format-toggle (name)
  "Define a toggle command for column format element represented by NAME."
  (declare (indent 1))
  (setq name (downcase name))
  (let ((var (intern (format "speedo-review-include-%s" name))))
    `(defun ,(intern (concat "speedo-review-toggle-" name)) ()
       ,(format "Toggle display of %s in attempt columns."
                (upcase name))
       (interactive)
       (setq ,var (not ,var))
       (speedo-review--ui-init speedo-review--attempts))))

(dolist (el '("mistakes" "relative-times" "relative-split-times"))
  (eval `(speedo-review-def-col-format-toggle ,el)))

(defun speedo-review--sort-col (name)
  "Toggle sorting of column with NAME."
  (save-excursion
    (goto-char (point-min))
    (if (text-property-search-forward 'tabulated-list-column-name name t)
        (progn (backward-char) (tabulated-list-sort))
      (user-error "Could not find %S column" name))))

(defun speedo-review-forward-col (&optional n)
  "Move forward N columns."
  (interactive "p")
  (dotimes (_ n)
    (text-property-search-forward 'tabulated-list-column-name)
    (when (eolp) (forward-line 1))))

(defun speedo-review-backward-col (&optional n)
  "Move backward N columns."
  (interactive "p")
  (dotimes (_ n)
    ;;@HACK: For whatever reason tabulated-list-search-backward will skip a
    ;; column when searching backward from first col. Maybe it has something to
    ;; do with the text property being contiguous between lines over the line
    ;; break?. In any case, we just account for this case by searching forward.
    (let ((first-col-p
           (string= (get-text-property (point) 'tabulated-list-column-name)
                    (car (aref tabulated-list-format 0)))))
      (text-property-search-backward 'tabulated-list-column-name)
      (when first-col-p (speedo-review-forward-col 1)))))

(defmacro speedo-review-def-col-sorter (name)
  "Define a column sorting command for column with NAME."
  (declare (indent 1))
  `(defun ,(intern (format "speedo-review-sort-%s" (downcase name))) ()
     ,(format "Toggle the sorting of the %s column." (upcase name))
     (interactive)
     (speedo-review--sort-col ,name)))

(dolist (colname '("Average" "Consistency" "ID" "Segment"))
  (eval `(progn (speedo-review-def-col-toggle ,colname)
                (speedo-review-def-col-sorter ,colname))))

(declare-function speedo-edit-attempt "speedo-edit" (attempt))
(defun speedo-review-edit-attempt ()
  "Edit attempt at current column."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (goto-char (point-min))
      (move-to-column col)
      (if-let ((attempt (get-text-property (point) 'speedo-attempt)))
          (speedo-edit-attempt attempt)
        (user-error "No attempt found in current column")))))

(define-derived-mode speedo-review-mode tabulated-list-mode "speedo-review"
  "Major mode for reviewing speedo attempts.

\\{speedo-review-mode-map}"
  ;; (when speedo-hide-cursor
  ;;   (when (bound-and-true-p blink-cursor-mode) (blink-cursor-mode -1))
  ;;   (speedo--hide-cursor)
  ;;   (add-hook 'quit-window-hook #'speedo--show-cursor nil 'local))
  (when speedo-highlight-line
    (face-remap-set-base 'hl-line nil)
    (face-remap-add-relative 'hl-line 'speedo-hl-line)
    (hl-line-mode))
  (setq buffer-face-mode-face 'speedo-default
        default-directory (file-name-directory speedo--data-file))
  (add-hook 'kill-emacs-hook  #'speedo--ask-to-save)
  (add-hook 'kill-buffer-hook #'speedo--confirm-kill-buffer nil t)
  (buffer-face-mode))

;;;; Key bindings
(define-key speedo-review-mode-map (kbd "A") 'speedo-review-toggle-average-column)
(define-key speedo-review-mode-map (kbd "C") 'speedo-review-toggle-consistency-column)
(define-key speedo-review-mode-map (kbd "E") 'speedo-review-edit-attempt)
(define-key speedo-review-mode-map (kbd "I") 'speedo-review-toggle-id-column)
(define-key speedo-review-mode-map (kbd "M") 'speedo-review-toggle-mistakes)
(define-key speedo-review-mode-map (kbd "R") 'speedo-review-toggle-relative-times)
(define-key speedo-review-mode-map (kbd "T") 'speedo-review-toggle-relative-split-times)
(define-key speedo-review-mode-map (kbd "a") 'speedo-review-sort-average)
(define-key speedo-review-mode-map (kbd "c") 'speedo-review-sort-consistency)
(define-key speedo-review-mode-map (kbd "i") 'speedo-review-sort-id)
(define-key speedo-review-mode-map (kbd "lr") 'speedo-review-last-runs)
(define-key speedo-review-mode-map (kbd "la") 'speedo-review-last-attempts)
(define-key speedo-review-mode-map (kbd "r") 'speedo-review)
(define-key speedo-review-mode-map (kbd "s") 'speedo-review-sort-segment)
(define-key speedo-review-mode-map (kbd "t") 'speedo-review-top-runs)
(define-key speedo-review-mode-map (kbd "+") 'speedo-edit-new)
(define-key speedo-review-mode-map (kbd "<tab>") 'speedo-review-forward-col)
(define-key speedo-review-mode-map (kbd "<backtab>") 'speedo-review-backward-col)

(provide 'speedo-review)
;;; speedo-review.el ends here
