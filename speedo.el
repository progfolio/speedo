;;; speedo.el --- Speedrun Timer  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'text-property-search)

;;; Custom Options
(defgroup speedo nil
  "Speedo: lightweight split timer."
  :group 'applications
  :prefix "speedo-")

(defgroup speedo-faces nil
  "Faces used in speedo.el."
  :group 'speedo
  :group 'faces)

(defface speedo-behind
  '((t (:weight bold :height 1.1 :slant italic :foreground "red")))
  "Face for time behind comparison."
  :group 'speedo-faces)

(defface speedo-ahead
  '((t (:weight bold :height 1.1 :slant italic :foreground "green")))
  "Face for time ahead of comparison."
  :group 'speedo-faces)

(defface speedo-gaining
  '((t (:weight bold :height 1.1 :slant italic :foreground "#CD5C5C")))
  "Face for time globally behind, current split ahead of comparison."
  :group 'speedo-faces)

(defface speedo-losing
  '((t (:weight bold :height 1.1 :slant italic :foreground "#3EB489")))
  "Face for time globally ahead, current split behind of comparison."
  :group 'speedo-faces)

(defface speedo-pb
  '((t (:weight bold :height 1.1 :slant italic :foreground "gold")))
  "Face for time ahead of comparison."
  :group 'speedo-faces)

(defface speedo-current-line
  '((t (:weight ultra-bold :height 1.04)))
  "Face for the global run timer."
  :group 'speedo-faces)

(defface speedo-comparison-line
  '((t (:weight light)))
  "Face for the global run timer."
  :group 'speedo-faces)

(defface speedo-timer
  '((t (:weight bold :foreground "#5555FF" :height 1.5)))
  "Face for the global run timer."
  :group 'speedo-faces)

(defcustom speedo-buffer "*speedo*"
  "Buffer to display splits."
  :type 'string
  :group 'speedo)

(defcustom speedo-default-comparison 'personal-best
  "Default comparison for current run."
  :type 'symbol
  :group 'speedo)

;;@INCOMPLETE: What's the appropriate default value here?
;; nil? make a dir in emacs-user-dir?
(defcustom speedo-directory default-directory
  "Default directory to search for and save splits."
  :type 'directory
  :group 'speedo)

;;; Variables

(defvar speedo-comparison speedo-default-comparison
  "Current comparison for current run.")

;;@TEST: remove hardcoded data eventually
(defvar speedo--mock-data '(:segments (( :name "ONE")
                                       ( :name "TWO")
                                       ( :name "THREE"))
                                      :attempts ((:start 1616642685487 :splits
                                                         ((:segment "ONE" :duration 10000)
                                                          (:segment "TWO" :duration 20000)
                                                          (:segment "THREE" :duration 30000)))))
  "Mock database.")
(defvar speedo--data (copy-tree speedo--mock-data) "Split database.")

(defvar speedo--segment-index -1 "Index of the current segment.")

;;; Functions

(defun speedo--plist-get* (plist &rest path)
  "Return PLIST value along key PATH.
PATH is a list of keywords which are nested within one another.
e.g. (plist-get* '(:one (:two (:three t))) :one :two :three ;; t"
  (unless (listp plist) (signal 'wrong-type-argument `(listp ,plist)))
  (while path
    (setq plist (plist-get plist (pop path))))
  plist)

(defun speedo--plist-put* (val plist &rest path)
  "Set VAL within a copy of PLIST along PATH.
PATH is a list of keywords which are nested within one another.
e.g. (plist-put nil '(:one (:two (:three t) :one :two :three
;; (:one (:two (:three nil)))
Note that missing keywords along path are added."
  (unless (listp plist) (signal 'wrong-type-argument `(listp ,plist)))
  (let* ((plen (length path)))
    (dotimes (n plen)
      (setq val (plist-put (apply #'speedo--plist-get* `(,plist ,@(butlast path (1+ n))))
                           (car (last path (1+ n))) val))))
  val)

;;@INCOMPLETE: protect against overwriting speedo--data before it is written to disk.
;; Should prompt user.
(defun speedo-load-file (&optional file)
  "Load a splits FILE."
  (interactive (list (read-file-name "Splits file: " speedo-directory)))
  (when (speedo--attempt-in-progress-p) (user-error "Cannot Load file while attempt is in progress"))
  (setq speedo--data
        (or (prog1
                (read (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string)))
              (message "Loaded splits file %S" file))
            (error "Error loading %s" file)))
  (speedo-open))

;;;; Timer
(defun speedo--sub-hour-formatter (_hours minutes seconds ms)
  "Display MINUTES:SECONDS.MS."
  (format "%d:%02d.%1d"  minutes seconds (/ ms 100)))

(defvar speedo--time-formatter #'speedo--sub-hour-formatter
  "Function to format time from timer.
It must accept four arguments: hours, minutes, seconds, milliseconds.")

(defun speedo--timestamp ()
  "Return time since unix epoch in milliseconds."
  (+ (* 1000 (string-to-number (format-time-string "%s")))
     (string-to-number (format-time-string "%3N"))))

(defun speedo--format-ms (n)
  "Format N milliseconds with `speedo-time-formatter'.
If FORMATTER is non-nil, use that format function instead.
It is called with hours, minutes, seconds, milliseconds."
  (let* ((milliseconds (mod n 1000))
         (n (/ n 1000))
         (seconds (mod n 60))
         (minutes (mod (/ n 60) 60))
         (hours (mod (/ n  (* 60 60)) 60))
         (formatter
          (or speedo--time-formatter
              (lambda (hours minutes seconds milliseconds)
                (format "%02d:%02d:%02d.%1d" hours minutes seconds
                        (/ milliseconds 100))))))
    (funcall formatter hours minutes seconds milliseconds)))

(defvar speedo--timer nil "The timer.")
(defvar speedo--timer-object nil "Internal timer object. Used for cancelling timer.")
(defvar speedo--ui-timer-object nil "Display timer object.")

(defun speedo--timer-start ()
  "Star the timer. Time is updated in milliseconds every tenth of a seocond.
Time should be accesed by views via the `speedo--timer' variable."
  ;;ensure only a single timer is running.
  (when speedo--timer-object (cancel-timer speedo--timer-object))
  (let ((start (speedo--timestamp)))
    (setq speedo--timer-object
          (run-with-timer 0 0.1 (lambda () (setq speedo--timer
                                                 (- (speedo--timestamp)
                                                    start))))))
  (speedo--display-run-timer))

(defcustom speedo-footer-format "%timer\nprevious: %previous"
  "The format for the speedo footer. It may contain %-escaped refrences to:
- %timer: the global split timer.
- %previous: the comparative value for the last split"
  :type 'string
  :group 'speedo)

(defun speedo--footer ()
  "Return propertized footer string as determined by `speedo-footer-format'."
  (let ((result speedo-footer-format))
    (dolist (escape '("timer" "previous") result)
      (setq result (replace-regexp-in-string (concat "%" escape)
                                             (propertize " " (intern
                                                              (concat "speedo-"
                                                                      escape))
                                                         t)
                                             result)))))

(defun speedo--insert-footer ()
  "Insert footer below tabulated list."
  (save-excursion
    (with-silent-modifications
      (goto-char (point-min))
      (when-let ((footer (text-property-search-forward 'speedo-footer)))
        (delete-region (prop-match-beginning footer) (point-max)))
      (goto-char (point-max))
      (insert (speedo--footer)))))

(defun speedo--display-ui ()
  "Display the UI."
  (tabulated-list-print)
  (speedo--insert-footer))

(defvar speedo--attempt-current nil "The current attempt.")

(defun speedo--relative-time (a b)
  "Return formatted timestring of B to A.
Times should be provided in ms.
Return nil if A or B is absent."
  (when (and a b)
    (let* ((previous (- a b))
           (sign (cond
                  ((< previous 0) "+")
                  ((> previous 0) "-")
                  (t "")))
           (time (speedo--format-ms (abs previous))))
      (setq time (substring time (string-match-p "[^0:]" time)))
      (when (string-prefix-p "." time) (setq time (concat "0" time)))
      (propertize (concat sign time)
                  'face (cond
                         ((< previous 0) 'speedo-behind)
                         ((> previous 0) 'speedo-ahead)
                         (t 'speedo-timer))))))

(defun speedo--insert-timers ()
  "Insert the dynamic run timers."
  (with-current-buffer speedo-buffer
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (let ((in-progress (speedo--attempt-in-progress-p))
              ahead behind gaining losing)
          (when-let* ((pb-splits (plist-get (speedo--run-pb) :splits))
                      (pb-split-duration
                       (plist-get (nth speedo--segment-index pb-splits) :duration))
                      (pb-previous-duration
                       (speedo--splits-duration
                        (seq-take pb-splits (max speedo--segment-index 1))))
                      (split-duration (- (speedo--timestamp)
                                         (plist-get (speedo--split-current) :start)))
                      (previous-duration
                       (or (speedo--splits-duration
                            (seq-take (plist-get
                                       (if in-progress
                                           speedo--attempt-current
                                         (car (last (plist-get speedo--data :attempts))))
                                       :splits)
                                      (max speedo--segment-index 1)))
                           ;;in case of first split, there is no previous duration
                           0))
                      (current-total (+ split-duration previous-duration))
                      ;; we don't want to double pb-total in case of first split
                      (pb-total (+ pb-split-duration (if (zerop speedo--segment-index)
                                                         0
                                                       pb-previous-duration))))
            (setq ahead   (< current-total pb-total)
                  behind  (> current-total pb-total)
                  losing  (and ahead (> split-duration pb-split-duration))
                  gaining (and behind (< split-duration pb-split-duration)))
            (when (or losing behind)
              (when-let ((current-relative (text-property-search-forward 'current-relative-timer)))
                (put-text-property (prop-match-beginning current-relative)
                                   (prop-match-end current-relative)
                                   'display (speedo--relative-time pb-total
                                                                   current-total)))))
          (when-let ((timer (text-property-search-forward 'speedo-timer)))
            (put-text-property
             (prop-match-beginning timer) (prop-match-end timer)
             'display
             (propertize (speedo--format-ms speedo--timer)
                         'face
                         (cond
                          (gaining '(:inherit (speedo-gaining speedo-timer)))
                          (losing  '(:inherit (speedo-losing speedo-timer)))
                          (ahead   '(:inherit (speedo-ahead speedo-timer)))
                          (behind  '(:inherit (speedo-behind speedo-timer)))
                          (t       'speedo-timer))))))))))

(defun speedo--display-run-timer ()
  "Display the run timer."
  (when speedo--ui-timer-object (cancel-timer speedo--ui-timer-object))
  (setq speedo--ui-timer-object (run-with-timer 0 0.1 #'speedo--insert-timers)))

;;;; Attempts

(defun speedo--attempt-init ()
  "Initialize a new attempt."
  (setq speedo--segment-index -1
        tabulated-list-entries #'speedo--list-splits
        speedo--attempt-current
        (list :start (speedo--timestamp)
              :splits
              (mapcar (lambda (segment) (list :segment (plist-get segment :name)))
                      (plist-get speedo--data :segments))))
  (speedo--refresh-header)
  (speedo--timer-start))

(defun speedo--attempt-record ()
  "Record `speedo--attempt-init' in `speedo--data'."
  (setq speedo--data
        (plist-put speedo--data :attempts
                   (append (plist-get speedo--data :attempts)
                           (list speedo--attempt-current)))))

(defun speedo--attempt-complete-p (attempt)
  "Return t if ATTEMPT has a time for all segments, else nil."
  (cl-every (lambda (split) (plist-get split :duration))
            (plist-get attempt :splits)))

(defun speedo--splits-duration (splits)
  "Return duration of SPLITS in ms.
If a split is missing a :duration, return nil."
  (condition-case _
      (cl-reduce (lambda (acc split) (+ acc (plist-get split :duration)))
                 splits :initial-value 0)
    (error nil)))

(defun speedo--attempt-in-progress-p ()
  "Return t if an attempt is in progress."
  (and speedo--attempt-current t))

(defun speedo--run-pb (&optional attempts nocache nosave)
  "Return personal best run.
If NOCACHE is non-nil, recalculate from ATTEMPTS.
IF NOSAVE is non-nil, do not cache the result."
  (let ((attempts (or attempts (plist-get speedo--data :attempts))))
    (if nocache
        (when-let ((index
                    (cl-position
                     (plist-get
                      (car
                       (seq-sort
                        (lambda (a b) (< (speedo--splits-duration (plist-get a :splits))
                                         (speedo--splits-duration (plist-get b :splits))))
                        (cl-remove-if-not #'speedo--attempt-complete-p attempts)))
                      :start)
                     (mapcar (lambda (attempt) (plist-get attempt :start)) attempts))))
          (unless nosave (speedo--plist-put* index speedo--data :runs :pb))
          (nth index attempts))
      (if-let ((index (plist-get (plist-get speedo--data :runs) :pb)))
          (nth index attempts)
        ;;calculate if no cache exists
        (speedo--run-pb attempts 'nocache)))))

(defvar speedo--segment-start nil
  "Set when a new segment is started during an attempt.")

;;@TERMINOLOGY: a split is like a lap on a stopwatch
(defun speedo--split-current ()
  "Return the current split from `speedo--attempt-current'."
  (nth speedo--segment-index (plist-get speedo--attempt-current :splits)))

(defun speedo--split-end ()
  "Record a split for the current segment."
  (let ((current (speedo--split-current)))
    (setf current
          (plist-put current :duration (- (speedo--timestamp)
                                          (plist-get current :start))))))

(defun speedo--split-start ()
  "Recrod start time of current split."
  (let ((current (speedo--split-current)))
    (setf current (plist-put current :start (speedo--timestamp)))))

(defun speedo--attempt-end ()
  "Save the current attempt to `speedo--data'.
Reset timers."
  (setq speedo--data (plist-put speedo--data :attempts
                                (append (plist-get speedo--data :attempts)
                                        (list (copy-tree speedo--attempt-current)))))
  (message "run saved.")
  (setq speedo--attempt-current nil)
  (speedo--run-pb nil 'nocache)
  (setq tabulated-list-entries #'speedo--list-last-attempt)
  (speedo--refresh-header)
  (cancel-timer speedo--timer-object)
  (cancel-timer speedo--ui-timer-object))

;;; Commands
(defun speedo-next ()
  "Start the next segment or a new attempt."
  (interactive)
  (with-current-buffer speedo-buffer
    (if (speedo--attempt-in-progress-p)
        (let ((last (1- (length (plist-get speedo--attempt-current :splits)))))
          (speedo--split-end)
          (when (= speedo--segment-index last) (speedo--attempt-end)))
      (speedo--attempt-init))
    (cl-incf speedo--segment-index)
    (speedo--split-start)
    (speedo--display-ui)
    ;;split time relative to pb split
    ;;@DECOMPSE into function
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications
        (when-let* ((ui (text-property-search-forward 'speedo-previous))
                    (pb (speedo--run-pb))
                    (previous (1- speedo--segment-index)))
          (put-text-property (prop-match-beginning ui) (prop-match-end ui)
                             'display (speedo--relative-time
                                       (plist-get
                                        (nth previous (plist-get pb :splits))
                                        :duration)
                                       (plist-get
                                        (nth previous (plist-get speedo--attempt-current :splits))
                                        :duration))))))
    (speedo--refresh-header)
    (unless (speedo--attempt-in-progress-p) (speedo--insert-timers))
    (forward-line speedo--segment-index)))

(defun speedo-previous ()
  "Select the previous segment."
  (interactive)
  (with-current-buffer speedo-buffer
    (unless (speedo--attempt-in-progress-p) (user-error "No attempt in progress"))
    (when (= speedo--segment-index 0) (user-error "No previous segment"))
    ;; clear out attempt data for this split and the previous
    (let ((current (speedo--split-current)))
      (setf current (plist-put current :duration nil)))
    (cl-decf speedo--segment-index)
    (let ((current (speedo--split-current)))
      (setf current (plist-put current :duration nil)))
    (forward-line -1)
    (let ((p (point)))
      (speedo--display-ui)
      (goto-char p))))

(defun speedo-mistake ()
  "Record a mistake in the current split."
  (interactive)
  (if (speedo--attempt-in-progress-p)
      (let ((current (speedo--split-current)))
        (setf current
              (plist-put current :mistakes
                         (append  (plist-get current :mistakes)
                                  (list (- (speedo--timestamp)
                                           (plist-get speedo--attempt-current :start))))))
        (message "mistake recorded"))
    (user-error "No run in progress")))

(defun speedo-reset ()
  "Reset the current split."
  (interactive)
  (unless (speedo--attempt-in-progress-p) (user-error "No attempt in progress"))
  (speedo--attempt-end)
  (setq tabulated-list-entries #'speedo--list-last-attempt
        speedo--segment-index -1)
  (speedo--display-ui))

(defcustom speedo-text-place-holder "⸺"
  "Placeholder text used when no data is available for a field."
  :type 'string
  :group 'speedo)

(defun speedo--split-time-relative (attempt n)
  "Return ATTEMPT's Nth split time relative to start."
  (let* ((splits (plist-get attempt :splits))
         (current (nth n splits))
         (duration (plist-get current :duration)))
    (when duration
      (cl-reduce (lambda (acc split) (+ acc (plist-get split :duration)))
                 (seq-take splits (1+ n))
                 :initial-value 0))))

(defun speedo--list-splits ()
  "Return a list of splits for UI."
  (let* ((index 0)
         (pb (speedo--run-pb))
         (pb-splits (plist-get pb :splits)))
    (mapcar (lambda (segment)
              (let ((name (plist-get segment :name))
                    (current (= index speedo--segment-index)))
                (prog1
                    `(,index
                      ;;@OPTIMIZE: just check current once and construct the full array
                      [,(if current
                            (propertize name 'face 'speedo-current-line)
                          name)
                       ,(let ((s (or (when pb
                                       (speedo--relative-time
                                        (speedo--splits-duration (seq-take pb-splits (1+ index)))
                                        (speedo--splits-duration
                                         (seq-take (plist-get speedo--attempt-current :splits)
                                                   (1+ index)))))
                                     speedo-text-place-holder)))
                          (if current
                              (propertize s 'current-relative-timer t
                                          'face '(:inherit (speedo-current-line speedo-comparison-line)))
                            s))
                       ,(let ((s (or (when-let ((current (speedo--split-time-relative
                                                          speedo--attempt-current index)))
                                       (speedo--format-ms current))
                                     (when-let ((pb (speedo--split-time-relative pb index)))
                                       (propertize (speedo--format-ms pb) 'face 'speedo-comparison-line))
                                     speedo-text-place-holder)))
                          (if current (propertize s 'current-segment-timer t
                                                  'face '(:inherit (speedo-current-line speedo-comparison-line)))
                            s))
                       ])
                  (setq index (1+ index)))))
            (plist-get speedo--data :segments))))

;;@DECOMPOSE: previous split compared to pb split relative
;; (speedo--relative-time
;;  (plist-get (nth index pb-splits) :duration)
;;  (plist-get
;;   (nth index
;;        (plist-get speedo--attempt-current :splits))
;;   :duration))

(defun speedo--list-last-attempt ()
  "Return a list last attempt's splits for UI."
  (let* ((index 0)
         (attempts (plist-get speedo--data :attempts))
         (attempt (car (last attempts)))
         (pb (if (equal attempt (speedo--run-pb))
                 ;;compare against last PB
                 (speedo--run-pb (butlast attempts) 'nocache 'nosave)
               (speedo--run-pb)))
         (pb-splits (plist-get pb :splits)))
    (mapcar
     (lambda (segment)
       (prog1
           (let ((name (plist-get segment :segment)))
             `(,index
               [,name
                ,(or (when (and pb (speedo--attempt-complete-p attempt))
                       (speedo--relative-time
                        (speedo--splits-duration (seq-take pb-splits (1+ index)))
                        (speedo--splits-duration (seq-take (plist-get attempt :splits)
                                                           (1+ index)))))
                     speedo-text-place-holder)
                ,(if-let ((reltime (speedo--split-time-relative attempt index)))
                     (speedo--format-ms reltime)
                   speedo-text-place-holder)
                ]))
         (setq index (1+ index))))
     (plist-get attempt :splits))))

(defun speedo--init-ui ()
  "Refresh the UI."
  (setq tabulated-list-entries #'speedo--list-splits))

(defun speedo--refresh-header ()
  "Refresh the header."
  (setq tabulated-list-format
        `[(,(propertize "Akogare Mario World 100%%" 'face
                        '(:height 1.2 :weight bold :foreground "green" :extend t))
           25)
          ,(let* ((attempts (plist-get speedo--data :attempts))
                  (complete (length (cl-remove-if-not #'speedo--attempt-complete-p attempts)))
                  (total (+ (length attempts) (if (speedo--attempt-in-progress-p) 1 0))))
             (list (if (zerop total)
                       ""
                     (format " %d/%d %0.2f%%%%" complete total
                             (* 100 (/ complete (float total)))))
                   9))
          ("" 1)
          ])
  (tabulated-list-init-header))

(defvar speedo-mode-map (make-sparse-keymap)
  "Keymap for speedo mode.")

(defun speedo-bury ()
  "Bury the `speedo-buffer'."
  (interactive)
  (with-current-buffer speedo-buffer (bury-buffer)))

(define-key speedo-mode-map (kbd "<kp-1>") 'speedo-next)
(define-key speedo-mode-map (kbd "<kp-3>") 'speedo-reset)
(define-key speedo-mode-map (kbd "<kp-8>") 'speedo-previous)
(define-key speedo-mode-map (kbd "<kp-5>") 'speedo-mistake)
(define-key speedo-mode-map (kbd "q") 'speedo-bury)
(define-key speedo-mode-map [t] 'ignore)

(define-derived-mode speedo-mode tabulated-list-mode "speedo"
  "Major mode for speedrun split timer.

\\{speedo-mode-map}"
  ;;have to turn off blink cursor mode, too...
  (internal-show-cursor (selected-window) nil)
  (speedo--init-ui)
  (speedo--refresh-header)
  (speedo--display-ui))

;;@Incomplete: make this a format string
;; that can display different things?
;; or reserve that for the header?
(defcustom speedo-buffer "*speedo*"
  "Name of the splits buffer."
  :type 'string
  :group 'speedo)

(defun speedo-open ()
  "Open the splits buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create speedo-buffer))
  (unless (derived-mode-p 'speedo)
    (speedo-mode)))

(provide 'speedo)

;;@TEST:
(defun speedo--data-reset ()
  "Maybe reset data...doesn't always work."
  (interactive)
  (when speedo--ui-timer-object (cancel-timer speedo--ui-timer-object))
  (when speedo--timer-object (cancel-timer speedo--timer-object))
  (setq speedo--data (copy-tree speedo--mock-data)
        speedo--attempt-current nil
        speedo--segment-index -1))
;;; speedo.el ends here
