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
(require 'bookmark)
(bookmark-maybe-load-default-file)

(defcustom speedo-review-include-target t
  "If non-nil, consider `speedo--comparison-target' implicit target of comparisons."
  :type  'boolean
  :group 'speedo)

(defvar speedo-review--header nil
  "Used to set custom `header-line-format' during comparisons.")
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
(defvar speedo-review-include-accumulative-times nil
  "When non-nil, segment durations are displayed relative to start of the run.
Otherwise they are relative to the start of each segment.")
(defvar speedo-review--attempts nil
  "Used to store attempts when manipulating views.")
(defvar speedo-review--last-command nil "Last review command and its args.")
(defvar speedo-review-include-other-runners nil
  "When non-nil, include runs from other runners.")

(defun speedo-review--row-data (attempts)
  "Compute row data for ATTEMPTS.
Returns a plist of form:
\(:id :name :mistakes :durations :average-duration :average-relative :consistency)."
  (let* ((segments          (plist-get speedo--data :segments))
         (segment-count     (length segments))
         (target            (car attempts))
         (target-segments   (plist-get target :segments))
         (rows              nil))
    (dotimes (i segment-count)
      (let* ((segment                 (nth i segments))
             (target-segment          (nth i target-segments))
             (target-segment-duration (plist-get target-segment :duration)))
        (push
         (list :id       i
               :name     (plist-get segment :name)
               :mistakes (mapcar (lambda (attempt)
                                   (length
                                    (plist-get (nth i (plist-get attempt :segments))
                                               :mistakes)))
                                 attempts)
               :durations (mapcar (lambda (attempt)
                                    (plist-get (nth i (plist-get attempt :segments))
                                               :duration))
                                  attempts)
               :relatives (when target-segment-duration
                            (mapcar
                             (lambda (attempt)
                               (ignore-errors
                                 (- target-segment-duration
                                    (plist-get (nth i (plist-get attempt :segments))
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

(defun speedo-review--row-averages (data row id)
  "Return a list of average times for each ROW in DATA by ID."
  (when speedo-review-include-average-column
    (list
     (if-let ((average-duration (plist-get row :average-duration)))
         (speedo--format-ms
          (if speedo-review-include-accumulative-times
              (cl-reduce
               #'+
               (cl-subseq data 0 (1+ id))
               :key (lambda (r) (or (plist-get r :average-duration) 0))
               :initial-value 0)
            average-duration))
       speedo-text-place-holder)
     (when speedo-review-include-relative-times
       (when-let ((average-relative (plist-get row :average-relative)))
         (speedo--relative-time
          (if speedo-review-include-accumulative-times
              (cl-reduce
               #'+
               (cl-subseq data 0 (1+ id))
               :key (lambda (r) (or (plist-get r :average-relative) 0)))
            average-relative)
          0)))
     (when speedo-review-include-mistakes
       (when-let ((average-mistakes (plist-get row :average-mistakes)))
         (let ((basis (or (car (plist-get row :mistakes)) 0)))
           (propertize
            (number-to-string average-mistakes)
            'face
            (cond
             ((< average-mistakes basis) 'speedo-ahead)
             ((> average-mistakes basis) 'speedo-behind)
             (t 'speedo-neutral)))))))))

(defun speedo-review--row-times (data row column)
  "Return ROW times from DATA by COLUMN."
  (let* ((times)
         (durations (plist-get row :durations))
         (relatives (plist-get row :relatives)))
    (dotimes (i (length durations))
      (push (list
             (if-let ((duration (nth i durations))
                      (time-string (speedo--format-ms
                                    (if speedo-review-include-accumulative-times
                                        (speedo--segments-duration
                                         (plist-get (nth i speedo-review--attempts) :segments)
                                         0 (1+ column))
                                      duration))))
                 (let ((basis-duration
                        (if speedo-review-include-accumulative-times
                            (speedo--segments-duration
                             (plist-get (nth 0 speedo-review--attempts) :segments)
                             0 (1+ column))
                          (nth 0 durations))))
                   (if (and basis-duration (not (zerop i)))
                       (speedo--colorize basis-duration duration time-string)
                     time-string))
               speedo-text-place-holder)
             (when (and speedo-review-include-relative-times
                        (not (zerop i)))
               (when-let ((relative (nth i relatives)))
                 (speedo--relative-time
                  (if speedo-review-include-accumulative-times
                      (cl-reduce #'+
                                 (cl-subseq data 0 (1+ column))
                                 :key (lambda (r) (nth i (plist-get r :relatives)))
                                 :initial-value 0)
                    relative)
                  0)))
             (when speedo-review-include-mistakes
               (when-let ((mistakes (plist-get row :mistakes))
                          (count (nth i mistakes))
                          (basis (car mistakes)))
                 (speedo--colorize basis count (number-to-string count)))))
            times))
    (nreverse times)))

(defun speedo-review--row-totals (data)
  "DATA."
  (let* ((keys (delq nil
                     (list :durations
                           (and speedo-review-include-mistakes :mistakes)
                           (and speedo-review-include-relative-times  :relatives))))
         (columns
          (let (v)
            (dolist (key keys (nreverse v))
              (let (acc)
                (dotimes (i (length (plist-get (car data) key)))
                  (push (ignore-errors
                          (cl-reduce
                           #'+ data
                           :key (lambda (row)
                                  (nth i (plist-get row key)))))
                        acc))
                (push key v)
                (push (nreverse acc) v))))))
    (when  speedo-review-include-average-column
      (dolist (key keys)
        (when-let ((vals (plist-get columns key)))
          (setq columns
                (plist-put columns key
                           (append vals
                                   (list
                                    (condition-case-unless-debug _
                                        (/ (cl-reduce #'+ vals)
                                           (length vals))
                                      (error nil)))))))))
    (dolist (key keys)
      (let* ((vals (plist-get columns key))
             (basis (car vals))
             (relative-p (eq key :relatives)))
        (setq columns
              (plist-put
               columns key
               (mapcar (lambda (it)
                         (unless (and relative-p (eq it basis))
                           (condition-case-unless-debug _
                               (propertize
                                (funcall (cond
                                          ((eq key :durations) #'speedo--format-ms)
                                          ((eq key :mistakes)  #'number-to-string)
                                          (relative-p
                                           (lambda (it)
                                             (speedo--relative-time it 0))))
                                         it)
                                'face (condition-case-unless-debug _
                                          (cond
                                           ((< it basis)
                                            (if relative-p 'speedo-behind 'speedo-ahead))
                                           ((> it basis)
                                            (if relative-p 'speedo-ahead 'speedo-behind))
                                           (t              'speedo-neutral))
                                        ((error) 'speedo-neutral)))
                             ((error) nil))))
                       vals)))))
    ;; Negative ID used in column sorting functions to distinguish this row form segments.
    (list -1
          (vconcat
           (when speedo-review-include-id-column (list " "))
           (list (propertize "Totals" 'face '(:weight bold)))
           (let* ((len (length (plist-get columns :durations)))
                  (placeholders (make-list len nil)))
             (list (cl-mapcar #'list
                              (or (plist-get columns :durations) placeholders)
                              (if speedo-review-include-relative-times
                                  (or (plist-get columns :relatives) placeholders)
                                placeholders)
                              (if speedo-review-include-mistakes
                                  (or (plist-get columns :mistakes) placeholders)
                                placeholders))))
           (when speedo-review-include-consistency-column
             (list " "))))))

(defun speedo-review--widest-timestring-component (rows index)
  "Return length of widest timestring componet at INDEX in ROWS."
  (car (cl-sort
        (mapcan (lambda (r)
                  (mapcar (lambda (it) (length (nth index it)))
                          (aref (cadr r) (if speedo-review-include-id-column 2 1))))
                rows)
        #'>)))

(defun speedo-review--format-rows (rows)
  "Format ROWS."
  (let* ((time-index (if speedo-review-include-id-column 2 1))
         (widest-duration (speedo-review--widest-timestring-component rows 0))
         (widest-relative
          (when speedo-review-include-relative-times
            (speedo-review--widest-timestring-component rows 1)))
         (widest-mistake
          (when speedo-review-include-mistakes
            (speedo-review--widest-timestring-component rows 2))))
    (mapcar (lambda (row)
              (let* ((col-descriptors (cadr row))
                     (times (aref col-descriptors time-index))
                     (col-index 0))
                (list
                 (car row)
                 (vconcat
                  (cl-subseq col-descriptors 0 time-index)
                  (mapcar
                   (lambda (component)
                     (let ((format-spec
                            (apply #'concat
                                   `("%" ,(number-to-string widest-duration) "s"
                                     ,@(when (and widest-relative (not (zerop col-index)))
                                         (list " " "%" (number-to-string widest-relative) "s"))
                                     ,@(when widest-mistake
                                         (list " " "%" (number-to-string widest-mistake)  "s"))))))
                       (prog1
                           (apply #'format
                                  (delq nil (list format-spec
                                                  (or (nth 0 component) speedo-text-place-holder)
                                                  (when (and speedo-review-include-relative-times
                                                             (not (zerop col-index)))
                                                    (or (nth 1 component)
                                                        speedo-text-place-holder))
                                                  (when speedo-review-include-mistakes
                                                    (or (nth 2 component)
                                                        speedo-text-place-holder)))))
                         (cl-incf col-index))))
                   times)
                  (cl-subseq col-descriptors (1+ time-index))))))
            rows)))

(defun speedo-review--rows (data)
  "Return table rows from DATA."
  (let ((rows
         (append
          (mapcar
           (lambda (r)
             (let* ((id (plist-get r :id))
                    (averages (speedo-review--row-averages data r id))
                    (times (append (speedo-review--row-times data r id)
                                   (when averages (list averages))))
                    (consistency
                     (when speedo-review-include-consistency-column
                       (if-let ((consistency (plist-get r :consistency)))
                           (number-to-string consistency)
                         speedo-text-place-holder))))
               (list id
                     (vconcat
                      (when speedo-review-include-id-column
                        (list (number-to-string (1+ id))))
                      (list (plist-get r :name))
                      (list times)
                      (list consistency)))))
           data)
          (when speedo-review-include-totals-row
            (list (speedo-review--row-totals data))))))
    (speedo-review--format-rows rows)))

(defun speedo-review--segment-col-length ()
  "Return length of longest segment name with padding."
  (let ((longest (car
                  (cl-sort (mapcar (lambda (segment) (length (plist-get segment :name)))
                                   (plist-get speedo--data :segments))
                           #'>))))
    (+ (max longest (length "Segment")) 2)))

(defmacro speedo-review--col-sorter (&rest body)
  "Excute BODY with anaphoric bindings.
Handle case of ignoring Totals column in sorting."
  (declare (indent defun))
  `(let* ((a-vec (cadr a))
          (b-vec (cadr b))
          (name-index (if speedo-review-include-id-column 1 0))
          (a-name (aref a-vec name-index))
          (b-name (aref b-vec name-index))
          (reversed (cdr-safe tabulated-list-sort-key)))
     (cond
      ((string= a-name "Totals") (if reversed t nil))
      ((string= b-name "Totals") (if reversed nil t))
      (t (condition-case-unless-debug _
             (progn ,@body)
           ((error) nil))))))

(defun speedo-review--sort-id-column (a b)
  "Sort rows A B by ID."
  (speedo-review--col-sorter (< (car a) (car b))))

(defun speedo-review--sort-segment-column (a b)
  "Sort rows A B by SEGMENT."
  (speedo-review--col-sorter (string< a-name b-name)))

(defun speedo-review--sort-attempt-column (a b)
  "Sort attempt column rows A and B by segment durations."
  (speedo-review--col-sorter
    (let* ((name (car tabulated-list-sort-key))
           (col-index
            (cl-position name (cl-coerce tabulated-list-format 'list)
                         :test #'string= :key #'car))
           (a (speedo--time-string-to-ms (car (split-string (aref a-vec col-index)))))
           (b (speedo--time-string-to-ms (car (split-string (aref b-vec col-index))))))
      (cond
       ;; Duration of 0 (incomplete attempt) is moved to end of list
       ((zerop a) nil)
       ((zerop b) t)
       (t (< a b))))))

(defun speedo-review--sort-consistencies (a b)
  "Sort table rows A and B by consistency."
  (speedo-review--col-sorter
    (let* ((consistency-a (aref a-vec (1- (length a-vec))))
           (consistency-b (aref b-vec (1- (length b-vec)))))
      (cond
       ;; Ensure place holders are put at end of list by default
       ((string= consistency-a speedo-text-place-holder) nil)
       ((string= consistency-b speedo-text-place-holder) t)
       (t (< (string-to-number consistency-a)
             (string-to-number consistency-b)))))))

(defun speedo-review--attempt-columns (attempts rows)
  "Return list of columns for ATTEMPTS.
ROWS are pre-formatted rows used to determine width of the column."
  (let ((target-attempt (car attempts))
        (col-width (length (aref (cadr (car rows))
                                 (if speedo-review-include-id-column 2 1)))))
    (mapcar
     (lambda (a)
       (let ((alias (or (plist-get a :alias)
                        (format-time-string
                         "%Y-%m-%d %I:%M%p"
                         (/ (plist-get a :start) 1000)))))
         (when (equal a target-attempt)
           (setq alias (propertize alias 'face '(:inherit speedo-neutral :weight bold))))
         (list (propertize alias 'speedo-attempt a)
               (+ 4 (max (length alias) col-width)) #'speedo-review--sort-attempt-column)))
     attempts)))

(defun speedo-review--columns (attempts rows)
  "Return Column format for ATTEMPTS.
Used as `tabulated-list-format'.
ROWS are used to determine column widths."
  (let* ((attempt-cols (speedo-review--attempt-columns attempts rows))
         (attempt-width (or (cadr (nth 1 attempt-cols)) 0)))
    (vconcat
     (when speedo-review-include-id-column
       (list (list "ID" 4 #'speedo-review--sort-id-column)))
     (list (list "Segment" (speedo-review--segment-col-length)
                 #'speedo-review--sort-segment-column))
     attempt-cols
     (when speedo-review-include-average-column
       (list (list "Average" (+ 2 (max (length "Average") attempt-width))
                   #'speedo-review--sort-attempt-column)))
     (when speedo-review-include-consistency-column
       (list (list "Consistency" (+ 2 (max (length "Consistency") 0))
                   #'speedo-review--sort-consistencies))))))

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
           (rows (speedo-review--rows (speedo-review--row-data attempts))))
      ;;commands are responsible for setting `speedo-review--header'
      (unless (derived-mode-p 'speedo-review-mode) (speedo-review-mode))
      (setq tabulated-list-entries rows
            tabulated-list-format (speedo-review--columns attempts rows)
            tabulated-list-use-header-line nil)
      ;; Clear the sort-key if the column it refrences has been removed.
      (unless (cl-member (car tabulated-list-sort-key)
                         (cl-coerce tabulated-list-format 'list)
                         :test #'string= :key #'car)
        (setq tabulated-list-sort-key nil))
      (tabulated-list-init-header)
      (tabulated-list-print 'remember-pos)
      (setq header-line-format speedo-review--header))))

(defun speedo-review--repeat-command (command &optional static)
  "Repeat COMMAND. If STATIC is non-nil, re-use attempts if possible.
Otherwise, COMMAND is recalculated."
  (unless static
    ;; ignore last two args: attempts and header
    (setq command (cl-subseq command 0 -2)))
  (eval command))

;;;###autoload
(defun speedo-review (&optional save attempts header)
  "Compare ATTEMPTS.
If SAVE is non-nil, the command is recorded in `speedo-review--last-command'.
If ATTEMPTS is nil, prompt user.
HEADER is shown in the review buffer."
  (interactive "p")
  (let ((attempts (or attempts (speedo-read-attempt nil 'multiple))))
    (setq speedo-review--header
          (or header
              (list (speedo--header-game-info)
                    (let ((len (length attempts)))
                      (propertize (format " %d Attempt%s" len
                                          (if (> len 1) "s" ""))
                                  'face 'speedo-neutral)))))
    (when save (setq speedo-review--last-command `(speedo-review nil ',attempts ',header)))
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
  (let* ((attempts (last (or attempts (speedo--attempts)) (abs n)))
         (header (or header (list (speedo--header-game-info)
                                  (propertize (let ((len (length attempts)))
                                                (if (eq len 1)
                                                    "Last Attempt"
                                                  (format " Last %d Attempts" len)))
                                              'face 'speedo-ahead)))))
    (when (> n 0) (setq attempts (reverse attempts)))
    (setq speedo-review--last-command
          `(speedo-review-last-attempts ,n ',attempts ',header))
    (speedo-review nil attempts header)))

;;;###autoload
(defun speedo-review-last-runs (&optional n attempts header)
  "Compare last N complete ATTEMPTS.
If N is positive, ATTEMPTS are sorted most recent first.
If N is negative, they are sorted most recent last.
HEADER is displayed in review buffer."
  (interactive "p")
  (speedo--ensure-data)
  (let* ((attempts (last (cl-remove-if-not #'speedo--attempt-complete-p
                                           (or attempts (speedo--attempts)))
                         (abs n)))
         (header (or header (list (speedo--header-game-info)
                                  (propertize (let ((len (length attempts)))
                                                (if (eq len 1)
                                                    " Last Run"
                                                  (format " Last %d Runs" len)))
                                              'face 'speedo-ahead)))))
    (when (> n 0) (setq attempts (reverse attempts)))
    (setq speedo-review--last-command `(speedo-review-last-runs ,n ',attempts ',header))
    (speedo-review nil attempts header)))

;;;###autoload
(defun speedo-review-top-runs (&optional n attempts header)
  "Compare top N complete ATTEMPTS.
If N is positive, ATTEMPTS are sorted most recent first.
If N is negative, they are sorted most recent last.
HEADER is displayed in review buffer."
  (interactive "p")
  (speedo--ensure-data)
  (let* ((runs (cl-sort (copy-tree
                         (or attempts (speedo--attempts (lambda (a)
                                                          (or (speedo--attempt-incomplete-p a)
                                                              (and
                                                               (not speedo-review-include-other-runners)
                                                               (plist-get a :runner)))))))
                        #'<
                        :key (lambda (a) (speedo--segments-duration (plist-get a :segments)))))
         (top (cl-subseq runs 0 (min (abs n) (length runs))))
         (header (or header
                     (list (speedo--header-game-info)
                           (propertize (let ((len (length top)))
                                         (if (eq len 1)
                                             (concat
                                              (unless speedo-review-include-other-runners " Personal ")
                                              "Best")
                                           (format " Top %d Runs" len)))
                                       'face 'speedo-ahead)))))
    (when (< n 0) (setq top (reverse top)))
    (setq speedo-review--last-command `(speedo-review-top-runs ,n ',runs ',header))
    (speedo-review nil top header)))

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

(dolist (el '("mistakes" "relative-times" "accumulative-times" "totals-row"))
  (eval `(speedo-review-def-col-format-toggle ,el)))

(defun speedo-review--sort-col (name)
  "Toggle sorting of column with NAME."
  (if-let ((nth (cl-position name
                             (cl-coerce tabulated-list-format 'list)
                             :test #'string=
                             :key #'car)))
      (tabulated-list-sort nth)
    (user-error "Could not find %S column" name)))

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

(defun speedo-review-post-edit (attempt)
  "Review edited ATTEMPT post edit finalize."
  (remove-hook 'speedo-edit-finalize-functions  #'speedo-review-post-edit)
  (if speedo-review--last-command
      (speedo-review--repeat-command speedo-review--last-command)
    (speedo-review nil (list attempt) "Last Edit")))

(defun speedo-review-edit-attempt (&optional n)
  "Edit Nth attempt column.
Note other columns (e.g. ID, Segment) are not counted toward N.
If there is only one atttempt, edit it.
If N is nil, attempt to edit attempt associated with column at point.
If no attempt is assoicated with that column, read an attempt."
  (interactive "P")
  (let* ((attempt-count (length speedo-review--attempts))
         (attempt
          (cond
           ((eq attempt-count 1) (car speedo-review--attempts))
           (n (nth (1- (max 1 (min (abs (prefix-numeric-value n)) attempt-count)))
                   speedo-review--attempts))
           (t (let ((col (current-column)))
                (save-excursion
                  (goto-char (point-min))
                  (move-to-column col)
                  (or (get-text-property (point) 'speedo-attempt)
                      (speedo-read-attempt speedo-review--attempts))))))))
    (add-hook 'speedo-edit-finalize-functions #'speedo-review-post-edit)
    (speedo-edit-attempt attempt)))

(define-derived-mode speedo-review-mode tabulated-list-mode "speedo-review"
  "Major mode for reviewing speedo attempts.

\\{speedo-review-mode-map}"
  (when speedo-highlight-line
    (face-remap-set-base 'hl-line nil)
    (face-remap-add-relative 'hl-line 'speedo-hl-line)
    (hl-line-mode))
  (setq buffer-face-mode-face 'speedo-default
        default-directory (file-name-directory speedo--data-file))
  (add-hook 'kill-emacs-hook  #'speedo--ask-to-save)
  (add-hook 'kill-buffer-hook #'speedo--confirm-kill-buffer nil t)
  (setq-local bookmark-make-record-function #'speedo-review-bookmark-make-record)
  (buffer-face-mode))

;;;; Key bindings
(define-key speedo-review-mode-map (kbd "A") 'speedo-review-toggle-average-column)
(define-key speedo-review-mode-map (kbd "C") 'speedo-review-toggle-consistency-column)
(define-key speedo-review-mode-map (kbd "E") 'speedo-review-edit-attempt)
(define-key speedo-review-mode-map (kbd "I") 'speedo-review-toggle-id-column)
(define-key speedo-review-mode-map (kbd "M") 'speedo-review-toggle-mistakes)
(define-key speedo-review-mode-map (kbd "R") 'speedo-review-toggle-relative-times)
(define-key speedo-review-mode-map (kbd "T") 'speedo-review-toggle-accumulative-times)
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

;;;; Bookmarks

(declare-function speedo-load-file "speedo-commands")
;;;###autoload
(defun speedo-review-bookmark-handler (record)
  "Jump to a speedo review from RECORD."
  (speedo-load-file (bookmark-prop-get record 'database) 'hide)
  (let ((command (bookmark-prop-get record 'command)))
    (speedo-review--repeat-command command (eq (car command) 'speedo-review))))

(defun speedo-review-bookmark-make-record ()
  "Return a bookmark record for the current `speedo-review' buffer."
  `(,(substring-no-properties (format-mode-line header-line-format))
    (buf      . ,speedo-review-buffer)
    (database . ,speedo--data-file)
    (command  . ,speedo-review--last-command)
    (location . ,speedo-review--last-command)
    (handler  . speedo-review-bookmark-handler)))

(provide 'speedo-review)
;;; speedo-review.el ends here
