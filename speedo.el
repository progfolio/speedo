;;; speedo.el --- Speedrun Timer  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the consequence of my frustration with other Linux split timers.
;; I don't want to run an entire web browser framework and I already run Emacs.
;; I also wanted to add some tools to make it easier to analyze my practice.

;;; Code:
;;;; Requirements
(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'tabulated-list)
(require 'text-property-search)

;;;; Groups
(defgroup speedo nil
  "Speedo: lightweight split timer."
  :group 'applications
  :prefix "speedo-")

(defgroup speedo-faces nil
  "Faces used in speedo.el."
  :group 'speedo
  :group 'faces)

;;;; Faces
(defface speedo-ahead
  '((t (:inherit speedo-emphasis :foreground "green")))
  "Face for time ahead of comparison."
  :group 'speedo-faces)

(defface speedo-behind
  '((t (:inherit speedo-emphasis :foreground "red")))
  "Face for time behind comparison."
  :group 'speedo-faces)

(defface speedo-comparison-line
  '((t (:weight light)))
  "Face for the global run timer."
  :group 'speedo-faces)

(defface speedo-current-line
  '((t (:weight ultra-bold)))
  "Face for the global run timer."
  :group 'speedo-faces)

(defface speedo-default
  '((t (:family "Hermit" :height 1.0)))
  "Face for emphasized information.")

(defface speedo-emphasis
  '((t (:weight bold :slant italic :height 1.0)))
  "Face for emphasized information.")

(defface speedo-gaining
  '((t (:inherit speedo-emphasis :foreground "#CD5C5C")))
  "Face for time globally behind, current split ahead of comparison."
  :group 'speedo-faces)

(defface speedo-header-game-info
  '((t (:height 1.2 :weight ultra-bold :foreground "#DD5668" :extend t)))
  "Face for the game title and category in the header line."
  :group 'speedo-faces)

(defface speedo-header-game-stats
  '((t (:height 1.2 :weight bold)))
  "Face for the attempted to completed ratio."
  :group 'speedo-faces)

(defface speedo-hl-line
  '((t (:background "#202060" :extend t)))
  "Face for highlighted line.")

(defface speedo-losing
  '((t (:inherit speedo-emphasis :foreground "#3EB489")))
  "Face for time globally ahead, current split behind of comparison."
  :group 'speedo-faces)

(defface speedo-pb
  '((t (:inherit speedo-emphasis :foreground "gold")))
  "Face for time ahead of comparison."
  :group 'speedo-faces)

(defface speedo-timer
  '((t (:weight ultra-bold :foreground "#5555FF" :height 1.5)))
  "Face for the global run timer."
  :group 'speedo-faces)

;;; Customizations
(defcustom speedo-buffer "*speedo*"
  "Buffer to display splits."
  :type 'string
  :group 'speedo)

(defcustom speedo-default-comparison-standard 'personal-best
  "Default comparison for current run."
  :type 'symbol
  :group 'speedo)

(defcustom speedo-comparison-standards '(personal-best
                                         best-segments
                                         latest-attempt
                                         latest-run
                                         world-record
                                         ;;best-split-times?
                                         ;;average-segments
                                         ;;median-segments
                                         ;;balanced-pb
                                         )
  "List of comparison standards.
`speedo-next-comparison' and `speedo-previous-comparsion' cycle these in order."
  :type 'list
  :group 'speedo)

(defcustom speedo-default-splits-file nil
  "The default splits file to load when `speedo' is run.
If it is not an absolute path, it is expanded relative to `speedo-directory'."
  :type 'file
  :group 'speedo)

(defcustom speedo-directory user-emacs-directory
  "Default directory to search for and save splits."
  :type 'directory
  :group 'speedo)

(defcustom speedo-footer-format "%timer\n%previous\n\nComparing Against: %basis"
  "The format for the speedo footer. It may contain %-escaped refrences to:
- %timer: the global split timer.
- %previous: the comparative value for the last split
- %basis: the basis for comparison"
  :type 'string
  :group 'speedo)

(defcustom speedo-footer-previous-format "previous: %s"
  "Format string for the previous split time UI.
It may contain one %-escaped reference to the previous split time."
  :type 'string
  :group 'speedo)

(defcustom speedo-hide-cursor t
  "If non-nil, hide the cursor in `speedo-buffer'."
  :type 'boolean
  :group 'speedo)

(defcustom speedo-highlight-line t
  "If non-nil, highlight the current UI line."
  :type 'boolean
  :group 'speedo)

(defcustom speedo-text-place-holder "⸺"
  "Placeholder text used when no data is available for a field."
  :type 'string
  :group 'speedo)

;;; Variables
(defvar speedo--current-attempt nil "The current attempt.")
(defvar speedo--comparison-basis nil "The object to compare attempts against.")
(defvar speedo--comparison-standard speedo-default-comparison-standard "The standard for comparison.")
(defvar speedo--data nil "Split database.")
(defvar speedo--data-file nil "The filepath of the loaded splits database.")
(defvar speedo--review-last-run nil "Used to keep track of when we want to review the last run.")
(defvar speedo--segment-index -1 "Index of the current segment.")
(defvar speedo--segment-start nil "Set when a new segment is started during an attempt.")
(defvar speedo--time-formatter #'speedo--sub-hour-formatter
  "Function to format time from timer.
It must accept four arguments: hours, minutes, seconds, milliseconds.")
(defvar speedo--timer nil "The global timer.")
(defvar speedo--timer-object nil "Internal timer object. Used for cancelling timer.")
(defvar speedo--ui-timer-object nil "Display timer object.")
(defvar speedo-mode-map (make-sparse-keymap) "Keymap for speedo mode.")

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

(defun speedo--read-file (file)
  "Read FILE into an elisp object."
  ;;@FIX: we need to be more robust here.
  (ignore-errors
    (read (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))))

(defun speedo--data-modified-p ()
  "Compare `speedo--data' to `speedo--data-file' and return t if they are not `equal'."
  (when (and speedo--data speedo--data-file)
    (not (equal speedo--data (speedo--read-file speedo--data-file)))))

;;;; Timer
(defun speedo--sub-hour-formatter (_hours minutes seconds ms)
  "Display MINUTES:SECONDS.MS."
  (format "%d:%02d.%1d"  minutes seconds (/ ms 100)))

;;@FIX: doesn't format time correctly.
;; (defun speedo--compact-time-formatter (hours minutes seconds ms)
;;   "Display compact time string for HOURS MINUTES SECONDS MS."
;;   (let* ((hours (and (> hours 0) (format "%02d" hours)))
;;          (minutes (when (or hours (> 0 minutes)) (format "%02d" minutes)))
;;          (seconds (format (if minutes "%02d" "%d") seconds)))
;;     (concat hours   (when hours ":")
;;             minutes (when minutes ":")
;;             seconds (when ms ".") (format "%d" (/ ms 10)))))

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

(defun speedo--splits-duration (splits)
  "Return duration of SPLITS in ms.
If a split is missing a :duration, return nil."
  (condition-case _
      (cl-reduce (lambda (acc split) (+ acc (plist-get split :duration)))
                 splits :initial-value 0)
    (error nil)))

(defun speedo--segment-pb (n)
  "Return best recorded time for segment N."
  (car
   (seq-sort #'<
             (delq nil
                   (mapcar (lambda (attempt)
                             (plist-get (nth n (plist-get attempt :splits)) :duration))
                           (plist-get speedo--data :attempts))))))

(defun speedo--best-segments ()
  "Return list of best segments."
  (list :start 0
        :splits
        (let ((index 0))
          (mapcar (lambda (segment)
                    (prog1
                        (setq segment (plist-put segment
                                                 :segment (plist-get segment :name))
                              segment (plist-put segment :duration (speedo--segment-pb index)))
                      (cl-incf index)))
                  (copy-tree (plist-get speedo--data :segments))))))

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

(defun speedo--comparison-basis (&optional standard)
  "Set variable `speedo--comparison-basis' to STANDARD."
  (setq speedo--comparison-basis
        (pcase standard
          ('personal-best (speedo--run-pb (plist-get speedo--data :attempts) 'nocache 'nosave))
          ('average-segments (error "Not implemented"))
          ('balanced-pb (error "Not implemented"))
          ('best-segments (speedo--best-segments))
          ('best-split-times (error "Not implemented"))
          ('latest-attempt (car (last (plist-get speedo--data :attempts))))
          ('latest-run (car (last (cl-remove-if-not #'speedo--attempt-complete-p
                                                    (plist-get speedo--data :attempts)))))
          ('median-segments (error "Not implemented"))
          ('world-record (plist-get speedo--data :world-record))
          (_ (speedo--run-pb (plist-get speedo--data :attempts) 'nocache 'nosave)))))

(defun speedo--insert-timers ()
  "Insert the dynamic run timers."
  (with-current-buffer speedo-buffer
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (let ((last-attempt (unless speedo--current-attempt
                              (car (last (plist-get speedo--data :attempts)))))
              ahead behind gaining losing)
          (when-let* ((basis-splits (plist-get speedo--comparison-basis :splits))
                      (basis-index (min speedo--segment-index (1- (length basis-splits))))
                      (basis-split-duration
                       (plist-get (nth basis-index basis-splits) :duration))
                      (basis-previous-duration
                       (speedo--splits-duration
                        (seq-take basis-splits (max basis-index 1))))
                      (split-duration (- (speedo--timestamp)
                                         (plist-get
                                          (or (speedo--split-current)
                                              ;;grab last split of last attempt
                                              (car (last (plist-get last-attempt :splits))))
                                          :start)))
                      (previous-duration
                       (or (speedo--splits-duration
                            (seq-take
                             (plist-get (or speedo--current-attempt last-attempt) :splits)
                             (max speedo--segment-index 1)))
                           ;;in case of first split, there is no previous duration
                           0))
                      (current-total (+ split-duration previous-duration))
                      ;; we don't want to double basis-total in case of first split
                      (basis-total (+ basis-split-duration
                                      (if (zerop speedo--segment-index)
                                          0
                                        basis-previous-duration))))
            (setq ahead   (< current-total basis-total)
                  behind  (> current-total basis-total)
                  losing  (and ahead (> split-duration basis-split-duration))
                  gaining (and behind (< split-duration basis-split-duration)))
            (when (or losing behind)
              (when-let ((current-relative (text-property-search-forward 'comparison-timer)))
                (put-text-property (prop-match-beginning current-relative)
                                   (prop-match-end current-relative)
                                   'display (speedo--relative-time basis-total
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
                          ((and ahead (not speedo--current-attempt))
                           '(:inherit (speedo-pb speedo-timer)))
                          (ahead   '(:inherit (speedo-ahead speedo-timer)))
                          (behind  '(:inherit (speedo-behind speedo-timer)))
                          (t       'speedo-timer))))))))))

(defun speedo--display-run-timer ()
  "Display the run timer."
  (when speedo--ui-timer-object (cancel-timer speedo--ui-timer-object))
  (setq speedo--ui-timer-object (run-with-timer 0 0.1 #'speedo--insert-timers)))

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

(defun speedo--footer ()
  "Return propertized footer string as determined by `speedo-footer-format'."
  (let ((result speedo-footer-format))
    ;; dynamic elements
    (dolist (escape '("timer" "previous") result)
      (setq result (replace-regexp-in-string
                    (concat "%" escape)
                    (propertize " " (intern (concat "speedo-" escape)) t)
                    result)))
    (dolist (escape '("basis") result)
      (setq result (replace-regexp-in-string
                    (concat "%" escape)
                    (intern (concat "speedo--footer-" escape))
                    result)))))

(defun speedo--footer-basis (_match)
  "Return the footer comparison basis string."
  (pcase speedo--comparison-standard
    ('personal-best "Personal Best")
    ('best-segments "Best Segments")
    ('latest-run "Latest Run")
    ('latest-attempt "Latest Attempt")
    ('world-record "World Record")
    (_ "Unknown")))

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
  "Display the UI (sans header)."
  (with-current-buffer speedo-buffer
    (tabulated-list-print 'remember-pos 'update)
    (speedo--insert-footer)))

(defun speedo--refresh-header ()
  "Refresh the header."
  (with-current-buffer speedo-buffer
    (setq tabulated-list-format
          (vector
           (let ((info (speedo--header-game-info))) (list info (length info)))
           (list (speedo--header-attempt-ratio) 10)
           ;; This column ignored for now.
           '("" 1)))
    (tabulated-list-init-header)))

(defun speedo--attempt-init ()
  "Initialize a new attempt."
  (setq speedo--segment-index -1
        speedo--current-attempt
        (list :start (speedo--timestamp)
              :splits
              (mapcar (lambda (segment) (list :segment (plist-get segment :name)))
                      (plist-get speedo--data :segments))))
  (speedo--comparison-basis speedo--comparison-basis)
  (speedo--refresh-header)
  (speedo--timer-start))

(defun speedo--attempt-complete-p (attempt)
  "Return t if ATTEMPT has a time for all segments, else nil."
  (cl-every (lambda (split) (plist-get split :duration))
            (plist-get attempt :splits)))

(defun speedo--split-current ()
  "Return the current split from `speedo--current-attempt'."
  (nth speedo--segment-index (plist-get speedo--current-attempt :splits)))

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
                                        (list (copy-tree speedo--current-attempt)))))
  (message "attempt ended")
  (setq speedo--current-attempt nil
        speedo--review-last-run t)
  (speedo--run-pb nil 'nocache)
  (speedo--refresh-header)
  (cancel-timer speedo--timer-object)
  (cancel-timer speedo--ui-timer-object))

(defun speedo--insert-previous-split-time ()
  "Insert previous split relative time in UI."
  (when (> speedo--segment-index 0) ; there is no previous for the first split
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications
        (when-let* ((ui (text-property-search-forward 'speedo-previous))
                    (previous (1- speedo--segment-index)))
          (put-text-property
           (prop-match-beginning ui) (prop-match-end ui)
           'display (format speedo-footer-previous-format
                            (speedo--relative-time
                             (plist-get
                              (nth previous (plist-get speedo--comparison-basis :splits))
                              :duration)
                             (plist-get
                              (nth previous (plist-get speedo--current-attempt :splits))
                              :duration)))))))))

(defun speedo--clear ()
  "Clear the last attempts times from UI."
  (with-current-buffer speedo-buffer
    (setq speedo--review-last-run nil)
    (speedo--comparison-basis speedo--comparison-basis)
    (speedo--display-ui)
    (speedo--insert-timers)))

(defun speedo--hide-cursor ()
  "Hide cursor in `speedo-buffer'."
  (internal-show-cursor (selected-window) nil))

(defun speedo--show-cursor ()
  "Show cursor in `speedo-buffer'."
  (internal-show-cursor (selected-window) t))

(defun speedo--ask-to-save ()
  "Ask to save data if `speedo--data-modified-p' during `kill-emacs-hook'."
  (when (and (speedo--data-modified-p)
             (yes-or-no-p (format "Speedo has modified splits for %S. Save before exit? "
                                  speedo--data-file)))
    (speedo-save-file)))

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
         (basis-splits (plist-get speedo--comparison-basis :splits)))
    (mapcar
     (lambda (segment)
       (let ((name (plist-get segment :name))
             (current (= index speedo--segment-index))
             (speedo--time-formatter (lambda (_h m s ms)
                                       (format "%02d:%02d" m (if (> ms 500)
                                                                 (1+ s)
                                                               s)))))
         (prog1
             (list
              index
              (vector
               (if current (propertize name 'face 'speedo-current-line) name)
               (let* ((speedo--time-formatter nil)
                      (s (or (when (and speedo--comparison-basis
                                        (or speedo--current-attempt
                                            speedo--review-last-run))
                               (speedo--relative-time
                                (speedo--splits-duration
                                 (seq-take basis-splits (1+ index)))
                                (speedo--splits-duration
                                 (seq-take (plist-get speedo--current-attempt :splits)
                                           (1+ index)))))
                             speedo-text-place-holder)))
                 (if current
                     (propertize
                      s
                      'comparison-timer t
                      'face '(:inherit (speedo-current-line speedo-comparison-line)))
                   s))
               (let ((s (or (when-let ((current (speedo--split-time-relative
                                                 speedo--current-attempt index)))
                              (speedo--format-ms current))
                            (when-let ((basis (speedo--split-time-relative speedo--comparison-basis index)))
                              (propertize (speedo--format-ms basis) 'face 'speedo-comparison-line))
                            speedo-text-place-holder)))
                 (if current (propertize s 'current-segment-timer t
                                         'face '(:inherit (speedo-current-line speedo-comparison-line)))
                   s))))
           (setq index (1+ index)))))
     (plist-get speedo--data :segments))))

(defun speedo--header-attempt-ratio ()
  "Return a string representing the completed to total attempts."
  (let* ((attempts (plist-get speedo--data :attempts))
         (complete (length (cl-remove-if-not #'speedo--attempt-complete-p attempts)))
         (total (+ (length attempts) (if speedo--current-attempt 1 0))))
    (propertize
     (if (zerop total)
         ""
       (format " %d/%d %d%%%%" complete total
               (* 100 (/ complete (float total)))))
     'face 'speedo-header-game-stats)))

(defun speedo--header-game-info ()
  "Return string with game title and category."
  (propertize (format "%s %s"
                      (or (plist-get speedo--data :title) "")
                      (or (replace-regexp-in-string
                           "%" "%%"
                           (plist-get speedo--data :category) "")))
              'face 'speedo-header-game-info))

;;; Commands
(defun speedo-next ()
  "Start the next segment or a new attempt."
  (interactive)
  (with-current-buffer speedo-buffer
    (if speedo--current-attempt
        (let ((last (1- (length (plist-get speedo--current-attempt :splits)))))
          (speedo--split-end)
          (when (= speedo--segment-index last) (speedo--attempt-end)))
      (speedo--attempt-init))
    (cl-incf speedo--segment-index)
    (speedo--split-start)
    (speedo--display-ui)
    (speedo--insert-previous-split-time)
    (speedo--refresh-header)
    (unless speedo--current-attempt (speedo--insert-timers))
    (goto-char (point-min))
    (forward-line speedo--segment-index)))

(defun speedo-previous ()
  "Select the previous segment."
  (interactive)
  (with-current-buffer speedo-buffer
    (unless speedo--current-attempt (user-error "No attempt in progress"))
    (when (= speedo--segment-index 0) (user-error "No previous segment"))
    ;; clear out attempt data for this split and the previous
    (let ((current (speedo--split-current)))
      (setf current (plist-put current :duration nil)))
    (cl-decf speedo--segment-index)
    (let ((current (speedo--split-current)))
      (setf current (plist-put current :duration nil)))
    (forward-line -1)
    (speedo--display-ui)))

(defun speedo-mistake ()
  "Record a mistake in the current split."
  (interactive)
  (if speedo--current-attempt
      (let ((current (speedo--split-current)))
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
  (if (not speedo--current-attempt)
      (speedo--clear)
    (speedo--attempt-end)
    (setq speedo--segment-index -1)
    (speedo--display-ui)))

(defun speedo-bury ()
  "Bury the `speedo-buffer'."
  (interactive)
  (with-current-buffer speedo-buffer
    (internal-show-cursor (selected-window) t) ;;show hidden cursor
    (bury-buffer)))

(defun speedo-save-file ()
  "Save `speedo--data' to `speedo--data-file'."
  (interactive)
  (if (speedo--data-modified-p)
      (with-temp-buffer
        (let ((print-level nil)
              (print-length nil)
              (print-circle t))
          (insert (pp-to-string speedo--data))
          (write-region (point-min) (point-max) speedo--data-file)))
    (message "(No changes need to be saved)")))

(defun speedo--comparison-n (n)
  "Compare against Nth relative standard in `speedo-comparison-standards'.
Negative N cycles backward, positive forward."
  (unless speedo--comparison-standard (user-error "No current comparison standard"))
  (let ((next (nth (mod (+ n (cl-position speedo--comparison-standard
                                          speedo-comparison-standards))
                        (length speedo-comparison-standards))
                   speedo-comparison-standards)))
    (setq speedo--comparison-standard next)
    (speedo--comparison-basis next))
  (speedo--display-ui)
  (speedo--insert-timers))

(defun speedo-comparison-next (&optional n)
  "Compare against Nth next standard in `speedo-comparison-standards'."
  (interactive "p")
  (speedo--comparison-n (or n 1)))

(defun speedo-comparison-previous (&optional n)
  "Compare against Nth next standard in `speedo-comparison-standards'."
  (interactive "p")
  (speedo--comparison-n (- (or n 1))))

(defun speedo-comparison-default ()
  "Compare against `speedo-default-comparison-standard'."
  (interactive)
  (setq speedo--comparison-standard speedo-default-comparison-standard)
  (speedo--comparison-basis speedo-default-comparison-standard)
  (speedo--display-ui))

;;;###autoload
(defun speedo-load-file (&optional file)
  "Load a splits FILE."
  (interactive)
  (let ((file (or file (read-file-name "Splits file: " speedo-directory))))
    (when speedo--current-attempt
      (user-error "Cannot Load file while attempt is in progress"))
    (when (and (speedo--data-modified-p)
               (y-or-n-p (format "%S modified, but not saved to disk. Save before loading %s? "
                                 speedo--data-file file)))
      (speedo-save-file))
    (if-let ((data (speedo--read-file file)))
        (prog1
            (setq speedo--data data
                  speedo--data-file file)
          (speedo--comparison-basis speedo--comparison-standard)
          (message "Loaded splits file %S" file)
          (speedo)
          (speedo--refresh-header)
          (speedo--display-ui))
      (error "Error loading %S" file))))

;;;###autoload
(defun speedo ()
  "Open the splits buffer."
  (interactive)
  (unless speedo--comparison-basis (speedo--comparison-basis speedo--comparison-standard))
  (unless speedo--data (speedo-load-file
                        (when speedo-default-splits-file
                          (expand-file-name speedo-default-splits-file speedo-directory))))
  (switch-to-buffer (get-buffer-create speedo-buffer))
  (set-window-dedicated-p (selected-window) t)
  (when speedo-hide-cursor (speedo--hide-cursor))
  (unless (derived-mode-p 'speedo-mode) (speedo-mode)))

(define-derived-mode speedo-mode tabulated-list-mode "speedo"
  "Major mode for speedrun split timer.

\\{speedo-mode-map}"
  (when speedo-hide-cursor
    (when (bound-and-true-p blink-cursor-mode) (blink-cursor-mode -1))
    (speedo--hide-cursor)
    (add-hook 'quit-window-hook #'speedo--show-cursor nil 'local))
  (when speedo-highlight-line
    (face-remap-set-base 'hl-line nil)
    (face-remap-add-relative 'hl-line 'speedo-hl-line)
    (hl-line-mode))
  (add-hook 'kill-emacs-hook #'speedo--ask-to-save)
  (setq buffer-face-mode-face 'speedo-default
        tabulated-list-entries #'speedo--list-splits)
  (buffer-face-mode)
  (speedo--refresh-header)
  (speedo--display-ui))

;;;; Key bindings
(define-key speedo-mode-map (kbd "<kp-1>") 'speedo-next)
(define-key speedo-mode-map (kbd "<kp-3>") 'speedo-reset)
(define-key speedo-mode-map (kbd "<kp-4>") 'speedo-comparison-previous)
(define-key speedo-mode-map (kbd "<kp-6>") 'speedo-comparison-next)
(define-key speedo-mode-map (kbd "<kp-7>") 'speedo-comparison-default)
(define-key speedo-mode-map (kbd "<kp-8>") 'speedo-previous)
(define-key speedo-mode-map (kbd "<kp-5>") 'speedo-mistake)
(define-key speedo-mode-map (kbd "q") 'speedo-bury)
(define-key speedo-mode-map [t] 'ignore)

(provide 'speedo)

;;; speedo.el ends here
