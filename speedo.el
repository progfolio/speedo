;;; speedo.el --- Speedrun Timer  -*- lexical-binding: t; eval: (add-hook 'after-save-hook #'recompile nil 'local) -*-

;; Copyright (C) 2021 Nicholas Vollmer
;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/speedo
;; Created: March 26, 2021
;; Keywords: games
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0

;;; Commentary:
;; This is the consequence of my frustration with other Linux split timers.
;; I don't want to run an entire web browser framework and I already run Emacs.
;; I also wanted to add some tools to make it easier to analyze my practice.

;;; Code:
;;;; Requirements
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'face-remap)
(require 'tabulated-list)
(require 'text-property-search)
(require 'speedo-vars)
(require 'speedo-faces)

;;@TODO: this should be a low-level function with a command based off of it.
(declare-function speedo-save-file "speedo-commands" (&optional force))
(declare-function speedo-load-file "speedo-commands" (&optional file hide))

;;; Functions

;;;; Utilities
(defun speedo--goto-index ()
  "Move point to segment assoicated with `speedo--segment-index'."
  (when (< speedo--segment-index (length (plist-get speedo--data :segments)))
    (goto-char (point-min))
    (while (not (= (if-let ((id (tabulated-list-get-id))) id -100) speedo--segment-index))
      (forward-line))))

(defun speedo--plist-get* (plist &rest path)
  "Return PLIST value along key PATH.
PATH is a list of keywords which are nested within one another.
e.g. (plist-get* '(:one (:two (:three t))) :one :two :three) ;; t"
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
  (unless path (error "Attemtpted to put %S on empty PATH" val))
  (unless (listp plist) (signal 'wrong-type-argument `(listp ,plist)))
  (let* ((plen (length path)))
    (dotimes (n plen)
      (let ((p (apply
                #'speedo--plist-get*
                (append (list plist) (butlast path (1+ n))))))
        (setq val (plist-put p (car (last path (1+ n))) val)))))
  val)

(defun speedo--plist-remove (plist &rest keys)
  "Return a copy of PLIST with KEYS removed.
This is different from setting KEYS to nil."
  (let (result)
    (dolist (keyword (nreverse (cl-remove-if-not #'keywordp plist)) result)
      (unless (member keyword keys)
        (push (plist-get plist keyword) result)
        (push keyword result)))))

(defun speedo--colorize (basis comparison string)
  "Propertize STRING by comparing BASIS to COMPARISON."
  (propertize string
              'face (cond ((< basis comparison) 'speedo-behind)
                          ((> basis comparison) 'speedo-ahead)
                          (t                    'speedo-neutral))))

(defun speedo--relative-time (a b)
  "Return formatted timestring of B to A.
Times should be provided in ms.
Return nil if A or B is absent."
  (when (and a b)
    (let* ((previous (- a b))
           (sign (cond
                  ((< previous 0) "+")
                  ((> previous 0) "-")
                  (t " ")))
           (time (speedo--format-ms (abs previous))))
      (setq time (substring time (string-match-p "[^0:]" time)))
      (when (string-prefix-p "." time) (setq time (concat "0" time)))
      (speedo--colorize previous 0 (concat sign time)))))

;;;; Predicates
(defun speedo--attempt-complete-p (attempt)
  "Return t if ATTEMPT is complete, else nil."
  (not (plist-member attempt :reset)))

(defun speedo--attempt-mine-p (attempt)
  "Return t if ATTEMPT does not belong to another runner, else nil."
  (not (plist-member attempt :runner)))

(defun speedo--attempt-ignored-p (attempt)
  "Return t if ATTEMPT is tagged \"ignore\"."
  (member "ignore" (plist-get attempt :tags)))

(defun speedo--attempt-incomplete-p (attempt)
  "Return t if ATTEMPT is incomplete, else nil."
  (not (speedo--attempt-complete-p attempt)))

(defun speedo--database-p (obj)
  "Return t if OBJ is a well formed database object.
It must be a non-empty plist with at least the following keys:
  - :title
  - :segments"
  (and obj (listp obj)
       (cl-every (lambda (requirement) (plist-member obj requirement))
                 '(:title :segments))))

(defun speedo--db-file-p (file)
  "Return t if FILE ends with `speedo--file-extension' or is a directory.
Used to filter candidates during `speedo-load-file'."
  (or (file-directory-p file)
      (string-suffix-p speedo--file-extension file 'ignore-case)))

(defun speedo--data-modified-p ()
  "Return t if `speedo--data' and `speedo--data-file' are not `equal'."
  (when (and speedo--data speedo--data-file)
    (not (equal speedo--data (speedo--convert-data (speedo--read-file speedo--data-file))))))

(defun speedo--read-file (file)
  "Read FILE into an elisp object."
  (condition-case err
      (let (obj)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (prog1
              (setq obj (read (current-buffer)))
            (unless (speedo--database-p obj)
              (signal 'wrong-type-argument (list 'speedo--database-p obj))))))
    (error (user-error "Speedo could not read %S: %S" file err))))

(defun speedo-read-attempt (&optional collection multiple)
  "Read attempts from COLLECTION.
If COLLECTION is nil, use `speedo--data' or throw an error.
IF MULTIPLE is non-nil, use `completing-read-multiple', else `completing-read'.
MULTIPLE results are returned in a list, single results are not."
  (let* ((candidates
          (mapcar
           (lambda (attempt)
             (cons (string-trim
                    (string-join
                     (list
                      (or (plist-get attempt :alias)
                          (format-time-string "%Y-%m-%d %I:%M%p"
                                              (/ (plist-get attempt :start) 1000)))
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
           (or collection (plist-get (speedo--ensure-data) :attempts))))
         (selections (funcall (if multiple #'completing-read-multiple
                                #'completing-read)
                              (if multiple "Attempts: " "Attempt: ")
                              candidates))
         result)
    (unless multiple (setq selections (list selections)))
    (setq result
          (delete-dups
           (mapcar (lambda (selection) (alist-get selection candidates nil nil #'string=))
                   selections)))
    (if multiple result (car result))))

;;;; Timer
(defun speedo--formatter-lossless (h m s ms)
  "Return lossless H:M:S.MS timestring for storing data."
  (format "%02d:%02d:%02d.%03d" h m s ms))

(defun speedo--formatter-compact (h m s ms)
  "Return shortest time string from H M S MS."
  (let ((time (concat
               (cond ((> h 0) (format "%d:%02d:" h m))
                     ((> m 0) (format "%d:" m)))
               (format "%04.1f" (+ s (/ ms 1000.0))))))
    (replace-regexp-in-string
     "\\(?:^0\\([^z-a]+?$\\)\\)" "\\1" time)))

(defun speedo--formatter-rounded (hours minutes seconds ms)
  "Display rounded HOURS MINUTES SECONDS MS."
  (let ((seconds (round (+ seconds (/ ms 1000.0)))))
    (speedo--formatter-compact hours minutes seconds 0)))

(defun speedo--formatter-sub-hour (_hours minutes seconds ms)
  "Display MINUTES:SECONDS.MS."
  (format "%d:%02d.%1d"  minutes seconds (/ ms 100)))

(defun speedo--parse-time-string (time-string)
  "Convert TIME-STRING into list of form:
\\(milliseconds seconds minutes hours)."
  (when (string-match-p "[^.0-:]" time-string)
    (error "Invalid character in time-string"))
  (let ((result
         (list (if (string-match "\\(?:\\([[:digit:]]\\)\\.\\([[:digit:]]*\\)$\\)"
                                 time-string)
                   (let ((ms (match-string 2 time-string)))
                     (setq time-string (replace-match "\\1" nil nil time-string))
                     (truncate (* (string-to-number (concat "0." ms)) 1000)))
                 0)))
        (components (nreverse (split-string time-string ":"))))
    ;; 3 because we have ms at this point and need sec, min, hr
    (dotimes (_ 3 (nreverse result))
      (push (let ((el (pop components)))
              (cond
               ((null el) 0)
               ((stringp el) (truncate (string-to-number el)))
               (t el)))
            result))))

(defun speedo--time-string-to-ms (time)
  "Convert TIME to ms."
  (let ((places '(1 1000 60000 3600000))
        (index 0))
    (truncate
     (apply #'+
            (mapcar
             (lambda (unit)
               (prog1 (* unit (nth index places)) (cl-incf index)))
             (speedo--parse-time-string time))))))

(defun speedo--ms-to-date (ms)
  "Convert MS into human readable date string."
  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (/ ms 1000.0))))

(defun speedo--date-to-ms (date)
  "Convert ISO 8601 DATE string to milliseconds."
  (* 1000
     (string-to-number
      (format-time-string "%s" (date-to-time date)))))

(defun speedo--timestamp (&optional time)
  "Return TIME since unix epoch in milliseconds."
  (+ (* 1000 (string-to-number (format-time-string "%s" time)))
     (string-to-number (format-time-string "%3N" time))))

(defun speedo--format-ms (n)
  "Format N milliseconds with `speedo--time-formatter'.
Formatter is called with hours, minutes, seconds, milliseconds."
  (let* ((milliseconds (mod n 1000))
         (n (/ n 1000))
         (seconds (mod n 60))
         (minutes (mod (/ n 60) 60))
         ;; Don't use mod here because we don't care about
         ;; dividing any farther than "hours"
         ;; using mod to check would truncate the hours
         ;; in cases where hours % 60 = 0
         (hours (/ n  (* 60 60))))
    (funcall (or speedo--time-formatter #'speedo--formatter-compact)
             hours minutes seconds milliseconds)))

(defun speedo-total-play-time (&optional attempts)
  "Return sum of ATTEMPTS durations as timestamp."
  (let ((speedo--time-formatter #'speedo--formatter-compact))
    (speedo--format-ms
     (apply #'+
            (flatten-tree
             (mapcar (lambda (attempt)
                       (mapcar (lambda (split) (or (plist-get split :duration) 0))
                               (plist-get attempt :splits)))
                     (or attempts (speedo--attempts))))))))

(defun speedo--splits-duration (splits &optional start end)
  "Return duration of SPLITS list in ms.
If a split is missing a :duration, return nil.
If START and END are non-nil, a subsequence of SPLITS is considered.
See `cl-subseq' for acceptable values."
  (when (or start end) (setq splits (cl-subseq splits (or start 0) end)))
  (condition-case _
      (cl-reduce #'+ splits
                 :key (lambda (s) (plist-get s :duration)) :initial-value 0)
    (error nil)))

(defun speedo--attempts (&optional filter)
  "Return possibly FILTERed attempts.
FILTER should be a function which takes an attempt and returns non-nil if the
attempt should be disregarded.
If nil, FILTER defaults to ignoring attempts tagged with \"ignore\"."
  (cl-remove-if (or filter #'speedo--attempt-ignored-p)
                (plist-get speedo--data :attempts)))

(defun speedo--delete-attempts (attempts data)
  "Return copy of DATA with ATTEMPTS removed."
  (interactive)
  (let ((d (copy-tree data)))
    (mapc
     (lambda (attempt)
       (setq d (plist-put d :attempts
                          (cl-remove attempt (plist-get d :attempts) :test #'equal))))
     attempts)
    d))

(defun speedo--segment-pb (n)
  "Return best recorded time for segment N."
  (car (cl-sort
        (delq nil
              (mapcar (lambda (attempt)
                        (plist-get (nth n (plist-get attempt :splits)) :duration))
                      (speedo--attempts)))
        #'<)))

(defun speedo-pb-chance (&optional env)
  "Return chance of getting PB from current split as a float.
Chance is an average of the sum of the current attempt duration and
the duration of remaining segments in each previous run.
If ENV is non-nil, it is a speedo timer enviornment object used for calculation."
  (format "%.2f%%"
          (* 100
             (if-let (speedo--current-attempt
                      (splits  (cl-subseq (plist-get speedo--current-attempt :splits)
                                          0 speedo--segment-index))
                      (current (or (plist-get env :duration)
                                   (speedo--splits-duration splits)
                                   0))
                      (pb (speedo--splits-duration
                           (plist-get (speedo--run-pb nil nil 'nosave) :splits)))
                      ((< current pb)))
                 (let ((possible-pbs
                        (mapcar (lambda (run)
                                  (if (< (cl-reduce
                                          #'+
                                          (cl-subseq (plist-get run :splits) speedo--segment-index)
                                          :key (lambda (split) (plist-get split :duration))
                                          :initial-value current)
                                         pb)
                                      1.0 0.0))
                                (speedo--attempts #'speedo--attempt-incomplete-p))))
                   (/ (apply #'+ possible-pbs) (length possible-pbs)))
               0.0))))

(defun speedo--best-segments ()
  "Return list of best durations for each segment in `speedo--data'."
  (let (durations)
    (dotimes (n (length (plist-get speedo--data :segments)))
      (push (speedo--segment-pb n) durations))
    (nreverse durations)))

(defun speedo--run-by (key computer &optional attempts nocache nosave)
  "Get run from `speedo--data' by KEY.
If there is no run by that key or NOCACHE is non-nil, compute the run with
COMPUTER.
COMPUTER must be a function which takes a list of ATTEMPTS and retruns a run.
If ATTEMPTS is nil, `speedo--attempts' are used.
If NOSAVE is non-nil, the computed result is not saved to `speedo--data'."
  (let ((attempts (or attempts (speedo--attempts))))
    (if nocache
        (when-let* ((found (funcall computer (copy-tree attempts)))
                    (index (cl-position (plist-get found :start) attempts
                                        :key (lambda (a) (plist-get a :start)))))
          (unless nosave
            (setq speedo--data (speedo--plist-put* index speedo--data :runs key)))
          found)
      (if-let ((index (speedo--plist-get* speedo--data :runs key)))
          (nth index attempts)
        ;;calculate if no cache exists
        (speedo--run-by key computer attempts 'nocache)))))

(defun speedo--run-worst (&optional attempts nocache nosave)
  "Return personal worst run.
If NOCACHE is non-nil, recalculate from ATTEMPTS.
IF NOSAVE is non-nil, do not cache the result."
  (speedo--run-by
   :worst
   (lambda (a) (car (cl-sort
                     (cl-remove-if-not #'speedo--attempt-complete-p a)
                     #'>
                     :key (lambda (run)
                            (speedo--splits-duration (plist-get run :splits))))))
   attempts nocache nosave))

(defun speedo--run-pb (&optional attempts nocache nosave)
  "Return personal best run.
If NOCACHE is non-nil, recalculate from ATTEMPTS.
IF NOSAVE is non-nil, do not cache the result."
  (speedo--run-by
   :pb
   (lambda (attempts)
     (car (cl-sort
           (cl-remove-if-not (lambda (a) (and (speedo--attempt-complete-p a)
                                              (speedo--attempt-mine-p a)))
                             attempts)
           #'<
           :key (lambda (run)
                  (speedo--splits-duration (plist-get run :splits))))))
   attempts nocache nosave))

(defun speedo-target-world-record ()
  "Return world record run."
  (car (speedo--attempts (lambda (attempt)
                           (not (member "world record" (plist-get attempt :tags)))))))

(defun speedo-target-personal-best ()
  "Return personal best run from `speedo--data'."
  (speedo--run-pb (speedo--attempts) 'nocache 'nosave))

(defun speedo-target-personal-worst ()
  "Return personal worst run from `speedo--data'."
  (speedo--run-worst (speedo--attempts) 'nocache 'nosave))

(defun speedo-target-best-segments ()
  "Return synthesized attempt with best times for each segment."
  (list :start 0
        :splits
        (let ((index 0))
          (mapcar (lambda (segment)
                    (prog1
                        (setq segment
                              (plist-put segment :segment (plist-get segment :name))
                              segment
                              (plist-put segment :duration (speedo--segment-pb index)))
                      (cl-incf index)))
                  (copy-tree (plist-get speedo--data :segments))))))

(defun speedo-target-last-attempt ()
  "Return last attempt."
  (car (last (speedo--attempts))))

(defun speedo-target-last-run ()
  "Return last complete attempt."
  (car (last (cl-remove-if-not #'speedo--attempt-complete-p (speedo--attempts)))))

(defun speedo--target-attempt (fn &optional cache)
  "Set and return variable `speedo--target-attempt' to result of FN.
If CACHE is non-nil, use the cache."
  (let ((target (or (rassoc fn speedo-comparison-targets)
                    (error "Unrecognized comparison target"))))
    (if cache
        (if-let ((member (plist-member speedo--target-attempts fn)))
            (setq speedo--target-attempt (cadr member)
                  speedo--comparison-target target)
          ;;calculate if the target has not been cached yet.
          (speedo--target-attempt fn))
      (let ((result (funcall fn)))
        (setq speedo--target-attempts (plist-put speedo--target-attempts fn result))
        (setq speedo--comparison-target target
              speedo--target-attempt result)))))

(defvar speedo-display-functions '(speedo-live-segment)
  "Hook run on every tick of the redisplay timer.
Each element is passed a timer env object produced by `speedo--timer-env'.
`speedo-buffer' is current.")

(defvar speedo-footer-display-functions nil
  "Hook run on every tick of the redisplay timer.
Each element is passed a timer env object produced by `speedo--timer-env'.
`speedo-buffer' is current.
It is run prior to `speedo-display-functions'.
It is set via `speedo-footer-format' when `speedo--footer' is called.")

(defmacro speedo-replace-ui-anchor (anchor &rest body)
  "Replace ANCHOR with result of BODY."
  (declare (indent 1) (debug (symbolp  &rest form)))
  `(save-excursion
     (goto-char (point-min))
     (when-let ((anchor (text-property-search-forward ',anchor)))
       (put-text-property (prop-match-beginning anchor) (prop-match-end anchor)
                          'display
                          ,@body))))

(defun speedo-global-timer (&optional env)
  "Display the global timer calculated from ENV."
  (if env
      (speedo-replace-ui-anchor speedo-global-timer-result
        (let ((ahead (plist-get env :ahead)))
          (propertize
           (speedo--format-ms (or speedo--timer 0))
           'face (if speedo--attempt-in-progress
                     (cond
                      ((plist-get env :gaining) 'speedo-gaining)
                      ((plist-get env :losing)  'speedo-losing)
                      (ahead                    'speedo-ahead)
                      ((plist-get env :behind)  'speedo-behind)
                      (t                        'speedo-neutral))
                   (cond
                    (ahead                   'speedo-pb)
                    ((plist-get env :behind) 'speedo-behind)
                    (t                       'speedo-neutral))))))
    (let ((result (speedo--format-ms (or speedo--timer 0))))
      ;; Covers the case where env is nil due to no available comparison target.
      ;; e.g. a fresh database.
      (when speedo--attempt-in-progress
        (speedo-replace-ui-anchor speedo-global-timer-result result))
      (propertize
       result
       'face 'speedo-global-timer
       'speedo-global-timer-result t))))
;;@TODO: color code against PB? (Might not be accurate early on when learning a game)
(defun speedo-projected-best (&optional env)
  "Display sum of completed segments plus best times for remaining segments.
ENV is used to determine when we are being called."
  (unless env
    (if-let (speedo--timer
             (completed (delq nil (mapcar (lambda (it) (plist-get it :duration))
                                          (plist-get speedo--current-attempt :splits))))
             (segments (delq  nil (speedo--best-segments)))
             (best (cl-subseq segments (length completed)))
             (projection (apply #'+ (append (or completed (list 0)) best)))
             ((< speedo--timer projection)))
        (speedo--format-ms projection)
      (propertize "⌛" 'face '(:weight bold)))))

(defun speedo-target (&optional env)
  "Display the current comparison target in the footer.
ENV is non-nil when we are in the redisplay timer hook."
  (unless env (propertize (car speedo--comparison-target) 'face  '(:weight bold))))

(defun speedo-live-segment (env)
  "If current segment is behind, display relative loss.
The loss is displayed live in the comparison column of the table.
If `speedo-previous-split' is part of the footer, it is replaced there as well.
Timer ENV is used to determine if segment is behind."
  (when-let (((plist-get env :current-behind))
             (target   (plist-get env :target-duration))
             (current  (plist-get env :duration))
             (relative (speedo--relative-time target current)))
    (speedo-replace-ui-anchor live-comparison relative)
    (when speedo-footer-want-live-segment
      (speedo-replace-ui-anchor speedo-previous-split
        (format (or speedo-footer-live-segment-format "%s") relative)))))

(defun speedo--timer-env ()
  "Calculate environment passed to each FN in `speedo-display-functions'."
  (when-let ((target-splits (plist-get speedo--target-attempt :splits))
             (target-index  (max 0 (min speedo--segment-index
                                        (1- (length target-splits)))))
             (target-split-duration
              (plist-get (nth target-index target-splits) :duration))
             (target-previous-duration
              (speedo--splits-duration target-splits 0 (max target-index 1)))
             (split (speedo--current-split))
             (split-duration
              ;;last split has been cleaned after attempt ended
              (or (plist-get split :duration)
                  (- (speedo--timestamp) (plist-get split :start))))
             (previous-duration
              (or (speedo--splits-duration
                   (plist-get speedo--current-attempt :splits)
                   0 (max speedo--segment-index 1))
                  ;;in case of first split, there is no previous duration
                  0))
             (current-total (+ split-duration previous-duration))
             ;; we don't want to double target-total in case of first split
             (target-total (+ target-split-duration
                              (if (zerop speedo--segment-index)
                                  0
                                target-previous-duration))))
    (let ((ahead          (< current-total  target-total))
          (behind         (> current-total  target-total))
          (current-behind (> split-duration target-split-duration))
          (current-ahead  (< split-duration target-split-duration)))
      (list
       :ahead                 ahead
       :behind                behind
       :current-ahead         current-ahead
       :current-behind        current-behind
       :duration              current-total
       :gaining               (and behind current-ahead)
       :losing                (and ahead  current-behind)
       :split-duration        split-duration
       :target-duration       target-total
       :target-split-duration target-split-duration))))

(defun speedo--redisplay ()
  "Run `speedo-display-functions' in context of `speedo-buffer'."
  (with-current-buffer speedo-buffer
    (save-excursion
      (with-silent-modifications
        (let ((env (speedo--timer-env)))
          (run-hook-with-args 'speedo-footer-display-functions env)
          (run-hook-with-args 'speedo-display-functions env))))))

(defconst speedo--tick-speed 0.1 "UI and timer object are updated every tick.")

(defun speedo--timer-display-start ()
  "Start timer to display UI timers."
  (if speedo--ui-timer-object
      (cancel-timer speedo--ui-timer-object)
    (setq speedo--ui-timer-object (run-with-timer 0 speedo--tick-speed #'speedo--redisplay))))

(defvar speedo--timer-start nil "When the timer was started.")

(defun speedo--timer-update ()
  "Update `speedo--timer'."
  (setq speedo--timer (- (speedo--timestamp) speedo--timer-start)))

(defun speedo--timer-start ()
  "Start the game timer. Time is updated in milliseconds every tenth of a seocond.
Time should be accesed by views via the `speedo--timer' variable."
  ;;ensure only a single timer is running.
  (if speedo--timer-object
      (progn
        (cancel-timer speedo--timer-object)
        (setq speedo--timer-start nil))
    (setq speedo--timer-start (speedo--timestamp))
    (setq speedo--timer-object (run-with-timer 0 speedo--tick-speed #'speedo--timer-update))
    (speedo--timer-display-start)))

(defvar speedo-footer-interpolation-regexp
  "\\(?:\\(%[^%]+?\\)\\([[:space:]]\\|\n\\|$\\)\\)"
  "Regexp used to interpolate `speedo-footer-format' string.")

(defun speedo--footer ()
  "Return footer string according to `speedo-footer-format'.
Set `speedo-footer-display-functions'."
  (setq speedo-footer-display-functions nil)
  (mapconcat (lambda (cell)
               (let* ((fn        (car cell))
                      (formatter (cdr cell))
                      (output    (funcall fn)))
                 (add-hook 'speedo-footer-display-functions (eval `(function ,fn)) 1)
                 (propertize
                  (if (functionp formatter)
                      (funcall formatter output)
                    (replace-regexp-in-string "%it" (or output " ") formatter))
                  fn t)))
             speedo-footer-format))

(defun speedo-previous-split (&optional env)
  "Display relative time of previous split in the footer.
Non-nil ENV signals that we are in the redisplay timer."
  (when-let (((not env))
             ((and speedo--current-attempt
                   speedo--target-attempt
                   ;; There is no previous for the first split.
                   (> speedo--segment-index 0)))
             (previous (1- speedo--segment-index))
             (current (or speedo--current-attempt (speedo-target-last-attempt)))
             (target-previous-split
              (nth previous (plist-get speedo--target-attempt :splits)))
             (previous-duration
              (plist-get (nth previous (plist-get current :splits)) :duration))
             (relative (speedo--relative-time
                        (plist-get target-previous-split :duration)
                        previous-duration)))
    (when (< previous-duration (nth previous speedo--best-segments))
      (setq relative (propertize (or relative " ")
                                 'face 'speedo-pb 'footer-previous t)))
    relative))

(defun speedo-footer-colorized-mistakes (count)
  "Return mistake COUNT colorized by comparison to target attempt."
  (let ((target (cl-reduce #'+ (plist-get speedo--target-attempt :splits)
                           :key (lambda (s) (length (plist-get s :mistakes)))
                           :initial-value 0)))
    (format (propertize (number-to-string count)
                        'face
                        (cond
                         ((< count target) 'speedo-ahead)
                         ((> count target) 'speedo-behind)
                         ((= count target) 'speedo-gaining)
                         (t 'speedo-neutral))))))

(defun speedo-mistakes (&rest _)
  "Insert mistake count in the UI."
  (let ((count (cl-reduce #'+ (plist-get (if speedo--attempt-in-progress
                                             speedo--current-attempt
                                           (speedo-target-last-attempt))
                                         :splits)
                          :key (lambda (s) (length (plist-get s :mistakes)))
                          :initial-value 0)))
    (propertize (if (functionp speedo-footer-mistakes-format)
                    (funcall speedo-footer-mistakes-format count)
                  (format speedo-footer-mistakes-format count)))))

(defun speedo--insert-footer ()
  "Insert footer below splits."
  (save-excursion
    (with-silent-modifications
      (goto-char (point-min))
      (when-let ((footer (text-property-search-forward 'speedo-footer)))
        (delete-region (prop-match-beginning footer) (point-max)))
      (goto-char (point-max))
      (insert (speedo--footer)))))

(defun speedo--display-ui ()
  "Display the UI table and footer (sans header)."
  (with-current-buffer speedo-buffer
    (let ((line (line-number-at-pos nil 'absolute)))
      (run-hooks 'speedo-pre-ui-display-hook)
      ;; Unfortunately, we can't rely on `tabulated-list-print' to remember our position
      (tabulated-list-print nil 'update)
      (speedo--insert-footer)
      (speedo--redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))
    (run-hooks 'speedo-post-ui-display-hook)))

(defun speedo--update-header ()
  "Refresh the header."
  (with-current-buffer speedo-buffer
    (setq header-line-format
          (list (speedo--header-game-info) (speedo--header-attempt-ratio) " "
                '(:propertize (:eval (replace-regexp-in-string "\\(?:\\.[^z-a]*\\)" "" (speedo-total-play-time)))
                              face speedo-header-game-info)))))

(defun speedo--attempt-init ()
  "Initialize a new attempt."
  ;; cache target attempts
  ;; We let-bind speedo--comparison-target here, so the user's value is not changed.
  (let (speedo--comparison-target)
    (dolist (target speedo-comparison-targets)
      (speedo--target-attempt (cdr target))))
  (setq speedo--attempt-in-progress t
        speedo--segment-index -1
        speedo--best-segments (speedo--best-segments)
        speedo--current-attempt
        (list :start (speedo--timestamp)
              :splits
              (mapcar (lambda (segment) (list :segment (plist-get segment :name)))
                      (plist-get speedo--data :segments))))
  (speedo--target-attempt (cdr speedo--comparison-target))
  (speedo--timer-start)
  (speedo--split-start)
  (speedo--update-header))

(defun speedo--current-split ()
  "Return the current split from `speedo--current-attempt'."
  (nth speedo--segment-index (plist-get speedo--current-attempt :splits)))

(defun speedo--split-end ()
  "Record a split for the current segment."
  (let ((current (speedo--current-split)))
    (setf current
          (plist-put current :duration (- (speedo--timestamp)
                                          (plist-get current :start))))))

(defun speedo--split-start ()
  "Recrod start time of current split."
  (let ((current (speedo--current-split)))
    (setf current (plist-put current :start (speedo--timestamp)))))

(defun speedo--data-add-attempt (attempt)
  "Return `speedo--data' with ATTEMPT appended to :attempts."
  (let ((cleaned
         (plist-put attempt :splits
                    (mapcar (lambda (split) (speedo--plist-remove split :start))
                            (cl-remove-if-not (lambda (split) (plist-get split :duration))
                                              (plist-get attempt :splits))))))
    (setq speedo--data (plist-put speedo--data :attempts
                                  (append (plist-get speedo--data :attempts)
                                          (list (copy-tree cleaned)))))))

(defun speedo--attempt-end ()
  "Save the current attempt to `speedo--data'.
Reset timers."
  (setq speedo--attempt-in-progress nil
        speedo--timer-object    (cancel-timer speedo--timer-object)
        speedo--ui-timer-object (cancel-timer speedo--ui-timer-object))
  (speedo--data-add-attempt speedo--current-attempt)
  (speedo--run-pb nil 'nocache) ;; Last attempt may be new PB
  (speedo--display-ui)
  (speedo--update-header)
  (message "attempt ended"))

(defun speedo--clear ()
  "Clear the last attempts times from UI."
  (setq speedo--timer nil
        speedo--target-attempts nil
        speedo--current-attempt nil)
  (speedo--target-attempt (cdr speedo--comparison-target))
  (speedo--display-ui)
  (goto-char (point-min)))

(defun speedo--hide-cursor ()
  "Hide cursor in `speedo-buffer'."
  (internal-show-cursor (selected-window) nil))

(defun speedo--show-cursor ()
  "Show cursor in `speedo-buffer'."
  (internal-show-cursor (selected-window) t))

(defun speedo--ask-to-save ()
  "Ask to save data if `speedo--data-modified-p' during `kill-emacs-hook'."
  (when (and (speedo--data-modified-p)
             (yes-or-no-p
              (format "Speedo has modified splits for %S. Save before exit? "
                      speedo--data-file)))
    ;; Force because we've already checked if the data has been modified
    (speedo-save-file 'force)))

(defun speedo--split-time-relative (attempt n)
  "Return ATTEMPT's Nth split time relative to start."
  (when-let ((splits (plist-get attempt :splits))
             (current (nth n splits))
             (duration (plist-get current :duration)))
    (cl-reduce #'+ (cl-subseq splits 0 (1+ n))
               :key (lambda (split) (plist-get split :duration))
               :initial-value 0)))

(defun speedo--timer-rows ()
  "Return a list of table rows for timer."
  (let* ((segments (plist-get speedo--data :segments))
         (segment-count (length segments))
         splits)
    (dotimes (index segment-count)
      (let* ((attempt speedo--current-attempt)
             (current-line (= index speedo--segment-index))
             (segment (nth index segments))
             (name (let ((n (plist-get segment :name)))
                     (if current-line (propertize n 'face 'speedo-current-line) n)))
             (target-splits (plist-get speedo--target-attempt :splits))
             (attempt-splits (plist-get attempt :splits))
             (current-face '(:inherit (speedo-current-line speedo-comparison-line)))
             (best-split
              (when-let ((best (nth index speedo--best-segments))
                         (segment-duration
                          (plist-get (nth index attempt-splits) :duration)))
                (< segment-duration best)))
             (comparison
              (let* ((s (or
                         (when (and target-splits speedo--current-attempt)
                           (when-let* ((target-duration
                                        (speedo--splits-duration
                                         target-splits 0 (min (+ index 1)
                                                              (length target-splits))))
                                       (attempt-duration
                                        (speedo--splits-duration
                                         attempt-splits 0 (min (+ index 1)
                                                               (length attempt-splits))))
                                       (relative
                                        (speedo--relative-time target-duration
                                                               attempt-duration))
                                       (target-split
                                        (plist-get (nth index target-splits) :duration))
                                       (attempt-split
                                        (plist-get (nth index attempt-splits) :duration)))
                             (cond
                              ((< attempt-split target-split
                                  target-duration attempt-duration)
                               (propertize relative 'face 'speedo-gaining))
                              ((< target-split attempt-split
                                  attempt-duration target-duration)
                               (propertize relative 'face 'speedo-losing))
                              ((= attempt-split target-split
                                  target-duration attempt-duration)
                               (propertize relative 'face 'speedo-neutral))
                              (t relative))))
                         speedo-text-place-holder)))
                (when current-line (setq s (propertize s 'live-comparison t)))
                (if best-split (propertize s 'face 'speedo-pb) s)))
             (speedo--time-formatter #'speedo--formatter-compact)
             (split-time
              (let ((s (or (when-let ((current (speedo--split-time-relative attempt index)))
                             (speedo--format-ms current))
                           (when-let ((target (speedo--split-time-relative
                                               speedo--target-attempt index)))
                             (propertize (speedo--format-ms target)
                                         'face 'speedo-comparison-line))
                           speedo-text-place-holder)))
                (if current-line (propertize s 'current-segment-timer t 'face current-face) s))))
        (push (list index (vector name comparison split-time)) splits)))
    (nreverse splits)))

(defun speedo--header-attempt-ratio ()
  "Return a string representing the completed to total attempts."
  (let* ((attempts (speedo--attempts))
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
                      (or (when-let ((category (plist-get speedo--data :category)))
                            (replace-regexp-in-string
                             "%" "%%" category))
                          ""))
              'face 'speedo-header-game-info))

;;; Save/Restore Customizations
(defun speedo--load-config ()
  "Execute `:config` section of `speedo--data'.
If `speedo-confirm-evaluate' is non-nil, confirm before evaluation."
  (when-let* ((member (cadr (plist-member speedo--data :config)))
              (config (if (stringp member)
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name member
                                               (file-name-directory speedo--data-file)))
                            (read (format "(%s)" (buffer-string))))
                        member))
              (permission (if speedo-confirm-evaluate
                              (yes-or-no-p (format (concat "Evaluate :config section of %s? "
                                                           "This may contain arbitrary elisp. "
                                                           "You should inspect it before executing.")
                                                   speedo--data-file))
                            t)))
    (eval (append '(progn) config))))

(defun speedo--custom-variables ()
  "Return a list of speedo.el's custom variables."
  (let (symbols)
    (obarray-map (lambda (ob)
                   (when (and (string-prefix-p "speedo" (symbol-name ob))
                              (custom-variable-p ob))
                     (push ob symbols)))
                 obarray)
    (nreverse symbols)))

(defun speedo--convert-data (data &optional human)
  "Return a copy of converted DATA.
If HUMAN is non-nil convert data to readable timestamps.
Else, data is converted numerically.

The aim here is to make the saved data human readable/editiable without
sacrificing performance at runtime."
  (let ((data (copy-tree data))
        (fn (if human #'speedo--format-ms
              #'speedo--time-string-to-ms))
        (speedo--time-formatter #'speedo--formatter-lossless))
    (dolist (attempt (plist-get data :attempts) data)
      (setq attempt
            (plist-put attempt :start
                       (funcall
                        (if human #'speedo--ms-to-date #'speedo--date-to-ms)
                        (plist-get attempt :start))))
      (when-let ((reset (plist-member attempt :reset)))
        (setq attempt (plist-put attempt :reset (funcall fn (plist-get attempt :reset)))))
      (setq attempt
            (plist-put attempt :splits
                       (mapcar
                        (lambda (split)
                          (let ((duration (plist-member split :duration))
                                (mistakes (plist-member split :mistakes)))
                            (when duration
                              (setq split
                                    (plist-put split :duration
                                               (funcall fn (plist-get split :duration)))))
                            (when mistakes
                              (setq split
                                    (plist-put split :mistakes
                                               (mapcar fn (plist-get split :mistakes)))))
                            split))
                        (plist-get attempt :splits)))))))

(defun speedo--write-data (data file &rest args)
  "Write formatted DATA to FILE.
ARGS are passed to `write-region'"
  (with-temp-buffer
    (let ((print-level nil)
          (print-length nil)
          (print-circle nil)
          (transformations
           ;;eol-properties
           '(("\\(?::[[:alpha:]]+$\\)" . (lambda () (replace-match "\n\\&")))
             ;;join-lines
             ("\\(?::\\(?:\\(?:mistake\\|run\\|tag\\)s\\)\\)" .
              (lambda () (forward-line) (join-line)))
             ;;data list indentation
             ("\\(?:(:\\)" . (lambda () (replace-match "( :")))))
          (empty-lines "\\(?:^[[:space:]]*$\\)"))
      (insert (pp-to-string data))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (dolist (transformation transformations)
        (save-excursion
          (while (re-search-forward (car transformation) nil 'noerror)
            (funcall (cdr transformation)))))
      (flush-lines empty-lines)
      (delete-trailing-whitespace (point-min) (point-max))
      (elisp-enable-lexical-binding)
      (add-file-local-variable-prop-line 'mode 'emacs-lisp)
      (indent-region (point-min) (point-max))
      (apply #'write-region `(,(point-min) ,(point-max) ,file ,@args)))))

(defun speedo--nth-target (n)
  "Compare against Nth relative target in `speedo-comparison-targets'.
Negative N cycles backward, positive forward."
  (unless speedo--comparison-target (user-error "No current comparison target"))
  (let ((next (nth (mod (+ n (or (cl-position speedo--comparison-target
                                              speedo-comparison-targets)
                                 -1))
                        (length speedo-comparison-targets))
                   speedo-comparison-targets)))
    (speedo--target-attempt (cdr next) 'cache))
  (speedo--display-ui)
  (speedo--redisplay))

(defun speedo--load-file (file)
  "Load a splits FILE."
  (if-let ((data (speedo--read-file file)))
      (prog1
          (setq speedo--segment-index -1
                speedo--timer nil
                speedo--current-attempt nil
                speedo--attempt-in-progress nil
                speedo--data (speedo--convert-data data)
                speedo--comparison-target (car speedo-comparison-targets)
                speedo--data-file file)
        (speedo--target-attempt (cdr speedo--comparison-target)))
    (error "Could not load: %S. Malformed?" file)))

(defun speedo--ensure-data ()
  "Ensure `speedo--data' is set and return it."
  (unless speedo--data (speedo-load-file nil 'hide))
  speedo--data)

(defun speedo--timer-columns-init ()
  "Initialize format of the UI."
  (setq tabulated-list-format
        ;;@INCOMPLETE: widths of columns should be defcustoms
        (vector
         ;; Find longest segment name and give it proper width
         (list
          "Segment"
          (floor
           (* 1.2
              (car (cl-sort (mapcar (lambda (segment) (length (plist-get segment :name)))
                                    (plist-get speedo--data :segments))
                            #'>)))))
         '("Comparison" 12)
         '("Time" 12 nil :right-align t))))

(defun speedo--confirm-kill-buffer ()
  "Clean up before killing `speedo-buffer'."
  (when (and (string= (buffer-name (current-buffer)) speedo-buffer)
             speedo--attempt-in-progress
             (yes-or-no-p "Killing buffer will reset current attempt. Proceed?"))
    (speedo--attempt-end)))

(define-derived-mode speedo-mode tabulated-list-mode "speedo"
  "Major mode for speedrun split timer.

\\{speedo-mode-map}"
  (condition-case-unless-debug err
      (speedo--load-config)
    ((error) (message "Error loading %s :config data: %S" speedo--data-file err)))
  (when speedo-hide-cursor
    (when (bound-and-true-p blink-cursor-mode) (blink-cursor-mode -1))
    (speedo--hide-cursor)
    (add-hook 'quit-window-hook #'speedo--show-cursor nil 'local))
  (when speedo-highlight-line
    (face-remap-set-base 'hl-line nil)
    (face-remap-add-relative 'hl-line 'speedo-hl-line)
    (hl-line-mode))
  (add-hook 'kill-emacs-hook  #'speedo--ask-to-save)
  (add-hook 'kill-buffer-hook #'speedo--confirm-kill-buffer nil t)
  (setq buffer-face-mode-face 'speedo-default
        tabulated-list-entries #'speedo--timer-rows
        default-directory (file-name-directory speedo--data-file))
  (buffer-face-mode)
  (speedo--update-header)
  (speedo--timer-columns-init)
  (speedo--display-ui))

;;;; Key bindings
(define-key speedo-mode-map (kbd "<kp-1>")  'speedo-next)
(define-key speedo-mode-map (kbd "SPC")     'speedo-next)
(define-key speedo-mode-map (kbd "<down>")  'speedo-next)
(define-key speedo-mode-map (kbd "<kp-3>")  'speedo-reset)
(define-key speedo-mode-map (kbd "r")       'speedo-reset)
(define-key speedo-mode-map (kbd "<kp-4>")  'speedo-comparison-previous)
(define-key speedo-mode-map (kbd "<left>")  'speedo-comparison-previous)
(define-key speedo-mode-map (kbd "<kp-6>")  'speedo-comparison-next)
(define-key speedo-mode-map (kbd "<right>") 'speedo-comparison-next)
(define-key speedo-mode-map (kbd "<kp-8>")  'speedo-previous)
(define-key speedo-mode-map (kbd "<up>")    'speedo-previous)
(define-key speedo-mode-map (kbd "<kp-5>")  'speedo-mistake)
(define-key speedo-mode-map (kbd "m")       'speedo-mistake)
(define-key speedo-mode-map (kbd "q")       'speedo-quit-window)
(define-key speedo-mode-map (kbd "c")       'speedo-compact-mode)

(provide 'speedo)
;;; speedo.el ends here
