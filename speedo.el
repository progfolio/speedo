;;; speedo.el --- Speedrun Timer  -*- lexical-binding: t; eval: (add-hook 'after-save-hook #'recompile nil 'local) -*-

;; Copyright (C) 2021-2025 Nicholas Vollmer
;; Author: Nicholas Vollmer
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
    (while (not (= (if-let* ((id (tabulated-list-get-id))) id -100) speedo--segment-index))
      (forward-line))))

(defun speedo--plist-get* (plist &rest path)
  "Return PLIST value along key PATH.
PATH is a list of keywords which are nested within one another.
e.g. (plist-get* \\='(:one (:two (:three t))) :one :two :three) ;; t"
  (unless (listp plist) (signal 'wrong-type-argument `(listp ,plist)))
  (while path
    (setq plist (plist-get plist (pop path))))
  plist)

(defun speedo--plist-put* (val plist &rest path)
  "Set VAL within a copy of PLIST along PATH.
PATH is a list of keywords which are nested within one another.
e.g. (plist-put nil \\='(:one (:two (:three t) :one :two :three
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
(defsubst speedo--attempt-in-progress-p ()
  "Return t if an attempt is in progress, nil otherwise."
  (eq speedo--state 'running))

(defsubst speedo--pre-run-p ()
  "Return t before the start of an attempt, nil otherwise.
This is to distinguish from just after a run has ended."
  (eq speedo--state 'pre))

(defsubst speedo--post-run-p ()
  "Return t after a run, nil otherwise.
This is to distinguish from the timer's cleared pre-run state."
  (eq speedo--state 'post))

(defsubst speedo--attempt-complete-p (attempt)
  "Return t if ATTEMPT is complete, else nil."
  (not (plist-get attempt :reset)))

(defsubst speedo--attempt-mine-p (attempt)
  "Return t if ATTEMPT does not belong to another runner, else nil."
  (not (plist-member attempt :runner)))

(defsubst speedo--attempt-ignored-p (attempt)
  "Return t if ATTEMPT is tagged \"ignore\"."
  (member "ignore" (plist-get attempt :tags)))

(defsubst speedo--attempt-incomplete-p (attempt)
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

;;;; Database
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
          (or
           (mapcar
            (lambda (attempt)
              (cons (string-trim
                     (string-join
                      (list
                       (or (plist-get attempt :alias)
                           (format-time-string "%Y-%m-%d %I:%M%p"
                                               (/ (plist-get attempt :start) 1000)))
                       (if-let* ((duration (speedo--format-ms
                                           (speedo--segments-duration
                                            (plist-get attempt :segments)))))
                           duration
                         "       ")
                       (if (speedo--attempt-complete-p attempt)
                           (propertize "complete" 'face '(:weight bold))
                         "reset")
                       (mapconcat (lambda (tag) (format "%S" tag))
                                  (plist-get attempt :tags) " "))
                      " "))
                    attempt))
            (or collection (plist-get (speedo--ensure-data) :attempts)))
           (user-error "Cannot read empty attempt list")))
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
               (format "%02d.%d" s (/ ms 100)))))
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
  (* 1000 (string-to-number (format-time-string "%s" (date-to-time date)))))

(defun speedo--timestamp (&optional time)
  "Return TIME since unix epoch in milliseconds."
  (+ (* 1000 (string-to-number (format-time-string "%s" time)))
     (string-to-number (format-time-string "%3N" time))))

(defun speedo--format-ms (n)
  "Format N milliseconds with `speedo--time-formatter'.
Formatter is called with hours, minutes, seconds, milliseconds."
  (let* ((milliseconds (% n 1000))
         (n (/ n 1000))
         (seconds (% n 60))
         (minutes (% (/ n 60) 60))
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
                       (mapcar (lambda (segment) (or (plist-get segment :duration) 0))
                               (plist-get attempt :segments)))
                     (or attempts (speedo--attempts))))))))

(defun speedo--segments-duration (segments &optional start end)
  "Return duration of SEGMENTS list in ms.
If START and END are non-nil, a subsequence of SEGMENTS is considered.
See `cl-subseq' for acceptable values."
  (when (or start end) (setq segments (cl-subseq segments (or start 0) end)))
  (cl-reduce #'+ segments
             :key (lambda (s) (or (plist-get s :duration) 0)) :initial-value 0))

(defun speedo--attempts (&optional filter)
  "Return possibly FILTERed attempts.
FILTER should be a function which takes an attempt and returns non-nil if the
attempt should be included in the results.
If nil, FILTER defaults to ignoring attempts tagged with \"ignore\"."
  (cl-remove-if-not (or filter (lambda (a) (not (speedo--attempt-ignored-p a))))
                    (plist-get speedo--data :attempts)))

(defun speedo--delete-attempts (attempts data)
  "Return copy of DATA with ATTEMPTS removed."
  (interactive)
  (cl-loop for stored in (plist-get data :attempts)
           when (not (member stored attempts))
           collect stored))

(defun speedo--segment-pb (n)
  "Return best recorded time for segment N."
  (car (cl-sort
        (delq nil
              (mapcar (lambda (attempt)
                        (plist-get (nth n (plist-get attempt :segments)) :duration))
                      (speedo--attempts)))
        #'<)))

(defun speedo-pb-chance (&optional env)
  "Return chance of getting PB from current segment as a float.
Chance is an average of the sum of the current attempt duration and
the duration of remaining segments in each previous run.
If ENV is non-nil, it is a speedo timer enviornment object used for calculation."
  (format "%.2f%%"
          (* 100
             (if-let* (speedo--current-attempt
                      (segments  (cl-subseq (plist-get speedo--current-attempt :segments)
                                            0 speedo--segment-index))
                      (current (or (plist-get env :duration)
                                   (speedo--segments-duration segments)
                                   0))
                      (pb (speedo--segments-duration
                           (plist-get (speedo--run-pb nil nil 'nosave) :segments)))
                      ((< current pb)))
                 (let ((possible-pbs
                        (mapcar (lambda (run)
                                  (if (< (cl-reduce
                                          #'+
                                          (cl-subseq (plist-get run :segments) speedo--segment-index)
                                          :key (lambda (segment) (plist-get segment :duration))
                                          :initial-value current)
                                         pb)
                                      1.0 0.0))
                                (speedo--attempts #'speedo--attempt-complete-p))))
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
      (if-let* ((index (speedo--plist-get* speedo--data :runs key)))
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
                            (speedo--segments-duration (plist-get run :segments))))))
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
                  (speedo--segments-duration (plist-get run :segments))))))
   attempts nocache nosave))

(defun speedo-target-world-record ()
  "Return world record run."
  (car (speedo--attempts (lambda (attempt)
                           (member "world record" (plist-get attempt :tags))))))

(defun speedo-target-personal-best ()
  "Return personal best run from `speedo--data'."
  (speedo--run-pb (speedo--attempts) 'nocache 'nosave))

(defun speedo-target-personal-worst ()
  "Return personal worst run from `speedo--data'."
  (speedo--run-worst (speedo--attempts) 'nocache 'nosave))

(defun speedo-target-best-segments ()
  "Return synthesized attempt with best times for each segment."
  (list :start 0
        :segments
        (let ((index -1))
          (delq nil
                (mapcar (lambda (segment)
                          (cl-incf index)
                          (when-let* ((pb (speedo--segment-pb index)))
                            (plist-put segment :duration pb)))
                        (copy-tree (plist-get speedo--data :segments)))))))

(defun speedo-target-last-attempt ()
  "Return last attempt."
  (car (last (speedo--attempts))))

(defun speedo-target-last-run ()
  "Return last complete attempt."
  (car (last (cl-remove-if-not #'speedo--attempt-complete-p (speedo--attempts)))))

(defun speedo--cache-targets ()
  "Cache `speedo-comparison-targets'."
  (let (speedo--comparison-target)
    (dolist (target speedo-comparison-targets)
      (speedo--target-attempt (cdr target)))))

(defun speedo--target-attempt (fn &optional cache)
  "Set and return variable `speedo--target-attempt' to result of FN.
If CACHE is non-nil, use the cache."
  (let ((target (or (rassoc fn speedo-comparison-targets)
                    (error "Unrecognized comparison target"))))
    (if cache
        (if-let* ((member (plist-member speedo--target-attempts fn)))
            (setq speedo--target-attempt (cadr member)
                  speedo--comparison-target target)
          ;;calculate if the target has not been cached yet.
          (speedo--target-attempt fn))
      (let ((result (funcall fn)))
        (setq speedo--target-attempts (plist-put speedo--target-attempts fn result))
        (setq speedo--comparison-target target
              speedo--target-attempt result)))))

(defmacro speedo-replace-ui-anchor (anchor &rest body)
  "Replace ANCHOR with result of BODY."
  (declare (indent 1) (debug (symbolp  &rest form)))
  `(save-excursion
     (goto-char (point-min))
     (when-let* ((anchor (text-property-search-forward ',anchor)))
       (put-text-property (prop-match-beginning anchor) (prop-match-end anchor)
                          'display
                          ,@body))))

(defun speedo-global-timer (&optional env)
  "Display the global timer calculated from ENV."
  (if env
      (speedo-replace-ui-anchor speedo-global-timer-result
        (let ((ahead (plist-get env :ahead)))
          (propertize
           (speedo--format-ms (or speedo--time 0))
           'face (if (speedo--attempt-in-progress-p)
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
    (let ((result (speedo--format-ms (or speedo--time 0))))
      ;; Covers the case where env is nil due to no available comparison target.
      ;; e.g. a fresh database.
      (if (speedo--attempt-in-progress-p)
          (speedo-replace-ui-anchor speedo-global-timer-result result))
      (propertize result 'face 'speedo-global-timer))))

;;@TODO: color code against PB? (Might not be accurate early on when learning a game)
(defun speedo-projected-best (&optional env)
  "Display sum of completed segments plus best times for remaining segments.
ENV is used to determine when we are being called."
  (unless env
    (if-let* (speedo--time
             (completed (delq nil (mapcar (lambda (it) (plist-get it :duration))
                                          (plist-get speedo--current-attempt :segments))))
             (segments (delq  nil (speedo--best-segments)))
             (best (cl-subseq segments (length completed)))
             (projection (apply #'+ (append (or completed (list 0)) best)))
             ((< speedo--time projection)))
        (speedo--format-ms projection)
      (propertize "⌛" 'face '(:weight bold)))))

(defun speedo-target (&optional env)
  "Display the current comparison target in the footer.
ENV is non-nil when we are in the redisplay timer hook."
  (unless env (propertize (car speedo--comparison-target) 'face  '(:weight bold))))

(defun speedo-live-segment (env)
  "If current segment is behind, display relative loss.
The loss is displayed live in the comparison column of the table.
If `speedo-previous-segment' is part of the footer, replace it there as well.
Timer ENV is used to determine if segment is behind."
  (when-let* (((plist-get env :current-behind))
             (target   (plist-get env :target-duration))
             (current  (plist-get env :duration))
             (relative (speedo--relative-time target current)))
    (speedo-replace-ui-anchor live-comparison relative)
    (when speedo-footer-want-live-segment
      (speedo-replace-ui-anchor speedo-previous-segment
        (format (or speedo-footer-live-segment-format "%s") relative)))))

(defun speedo--timer-env ()
  "Calculate environment passed to each FN in `speedo-display-functions'."
  (when-let* ((target-segments         (plist-get speedo--target-attempt :segments))
             (target-index            (max 0 (min speedo--segment-index
                                                  (1- (length target-segments)))))
             (target-segment          (nth target-index target-segments))
             ((not (plist-get target-segment :skip)))
             (target-segment-duration (plist-get target-segment :duration))
             (target-previous-duration
              (speedo--segments-duration target-segments 0 (max target-index 1)))
             (segment (speedo--current-segment))
             (segment-duration
              ;;last segment has been cleaned after attempt ended
              (or (plist-get segment :duration)
                  (- (speedo--timestamp) (plist-get segment :start))))
             (previous-duration
              (or (speedo--segments-duration
                   (plist-get speedo--current-attempt :segments)
                   0 (max speedo--segment-index 1))
                  ;;in case of first segment, there is no previous duration
                  0))
             (current-total (+ segment-duration previous-duration))
             ;; we don't want to double target-total in case of first segment
             (target-total (+ target-segment-duration
                              (if (zerop speedo--segment-index)
                                  0
                                target-previous-duration))))
    (let ((ahead          (< current-total  target-total))
          (behind         (> current-total  target-total))
          (current-behind (> segment-duration target-segment-duration))
          (current-ahead  (< segment-duration target-segment-duration)))
      (list
       :ahead                 ahead
       :behind                behind
       :current-ahead         current-ahead
       :current-behind        current-behind
       :duration              current-total
       :gaining               (and behind current-ahead)
       :losing                (and ahead  current-behind)
       :segment-duration        segment-duration
       :target-duration       target-total
       :target-segment-duration target-segment-duration))))

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

(defun speedo--timer-update ()
  "Update `speedo--timer'."
  (setq speedo--time (- (speedo--timestamp) speedo--timer-start)))

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
                    (replace-regexp-in-string
                     "%it"
                     (propertize (or output " ") (intern (format "%s-result" fn)) t)
                     formatter))
                  fn t)))
             speedo-footer-format))

(defun speedo-previous-segment (&optional env)
  "Display relative time of previous segment in the footer.
Non-nil ENV signals that we are in the redisplay timer."
  (when-let* (((not env))
             ((and speedo--current-attempt
                   speedo--target-attempt
                   ;; There is no previous for the first segment.
                   (> speedo--segment-index 0)))
             (previous (1- speedo--segment-index))
             (current (or speedo--current-attempt (speedo-target-last-attempt)))
             (target-previous-segment
              (nth previous (plist-get speedo--target-attempt :segments)))
             (previous-duration
              (plist-get (nth previous (plist-get current :segments)) :duration))
             (relative (speedo--relative-time
                        (plist-get target-previous-segment :duration)
                        previous-duration)))
    (when (< previous-duration (nth previous speedo--best-segments))
      (setq relative (propertize (or relative " ")
                                 'face 'speedo-pb 'footer-previous t)))
    relative))

(defun speedo-footer-colorized-mistakes (count)
  "Return mistake COUNT colorized by comparison to target attempt."
  (let ((target (cl-reduce #'+ (plist-get speedo--target-attempt :segments)
                           :key (lambda (s) (length (plist-get s :mistakes)))
                           :initial-value 0)))
    (format (propertize (number-to-string count)
                        'face
                        (cond
                         ((< count target) 'speedo-ahead)
                         ((> count target) 'speedo-behind)
                         ((= count target) 'speedo-gaining)
                         (t                'speedo-neutral))))))

(defun speedo-mistakes (&rest _)
  "Insert mistake count in the UI."
  (let* ((target (pcase speedo--state
                   ('pre     speedo--target-attempt)
                   ('running speedo--current-attempt)
                   ('post    (speedo-target-last-attempt))))
         (count (if target
                    (cl-reduce #'+ (plist-get target :segments)
                               :key (lambda (s) (length (plist-get s :mistakes))))
                  0))
         (s (unless (speedo--pre-run-p)
              (if (functionp speedo-footer-mistakes-format)
                  (funcall speedo-footer-mistakes-format count)
                (format speedo-footer-mistakes-format count)))))
    (if (speedo--pre-run-p)
        (number-to-string count)
      (speedo-replace-ui-anchor speedo-mistakes-result s))))

(defun speedo--insert-footer ()
  "Insert footer below splits."
  (save-excursion
    (with-silent-modifications
      (goto-char (point-min))
      (when-let* ((footer (text-property-search-forward 'speedo-footer)))
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
  (speedo--cache-targets)
  (setq speedo--state 'running
        speedo--segment-index -1
        speedo--best-segments (speedo--best-segments)
        speedo--current-attempt
        (list :start (speedo--timestamp)
              :segments (copy-tree (plist-get speedo--data :segments))))
  (speedo--target-attempt (cdr speedo--comparison-target))
  (speedo--timer-start)
  (speedo--segment-start)
  (speedo--update-header))

(defun speedo--current-segment ()
  "Return the current segment from `speedo--current-attempt'."
  (nth speedo--segment-index (plist-get speedo--current-attempt :segments)))

(defun speedo--segment-end ()
  "Record a split for the current segment."
  (let ((current (speedo--current-segment)))
    (setf current
          (plist-put current :duration (- (speedo--timestamp)
                                          (plist-get current :start))))))

(defun speedo--segment-start ()
  "Recrod start time of current split."
  (let ((current (speedo--current-segment)))
    (setf current (plist-put current :start (speedo--timestamp)))))

(defun speedo--data-add-attempt (attempt)
  "Return `speedo--data' with ATTEMPT appended to :attempts."
  (let ((cleaned
         (plist-put attempt :segments
                    (mapcar (lambda (segment) (speedo--plist-remove segment :start))
                            (cl-remove-if-not (lambda (segment)
                                                (or (plist-get segment :duration)
                                                    (plist-get segment :skip)))
                                              (plist-get attempt :segments))))))
    (setq speedo--data (plist-put speedo--data :attempts
                                  (append (plist-get speedo--data :attempts)
                                          (list (copy-tree cleaned)))))))

(defun speedo--attempt-end ()
  "Save the current attempt to `speedo--data'.
Reset timers."
  (setq speedo--state 'post
        speedo--timer-object    (cancel-timer speedo--timer-object)
        speedo--ui-timer-object (cancel-timer speedo--ui-timer-object))
  (speedo--data-add-attempt speedo--current-attempt)
  (speedo--run-pb nil 'nocache) ;; Last attempt may be new PB
  (speedo--display-ui)
  (speedo--update-header)
  (goto-char (point-max))
  (text-property-search-backward 'speedo-global-timer)
  (message "attempt ended"))

(defun speedo--clear ()
  "Clear the last attempts times from UI."
  (setq speedo--time nil
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
             (target-segments  (plist-get speedo--target-attempt :segments))
             (target-segment   (nth index target-segments))
             (target-duration
              (speedo--segments-duration
               target-segments 0 (min (+ index 1)
                                      (length target-segments))))
             (target-segment-duration (plist-get target-segment :duration))
             (attempt-segments (plist-get attempt :segments))
             (attempt-segment  (nth index (plist-get attempt :segments)))
             (attempt-duration
              (speedo--segments-duration
               attempt-segments 0 (min (+ index 1)
                                       (length attempt-segments))))
             (attempt-segment-duration (plist-get attempt-segment :duration))
             (attempt-segment-skipped  (plist-get attempt-segment :skip))
             (target-segment-skipped   (plist-get target-segment :skip))
             (skipped (or attempt-segment-skipped target-segment-skipped))
             (current-face '(:inherit (speedo-current-line speedo-comparison-line)))
             (best-segment
              (unless skipped
                (when-let* ((best (nth index speedo--best-segments))
                           (segment-duration
                            (plist-get (nth index attempt-segments) :duration)))
                  (< segment-duration best))))
             (comparison
              (let* ((s (or
                         (when-let* (((not skipped))
                                    (target-segments)
                                    (target-segment-duration)
                                    (speedo--current-attempt)
                                    (attempt-segment-duration)
                                    (target-duration)
                                    (attempt-duration)
                                    (relative
                                     (speedo--relative-time target-duration
                                                            attempt-duration)))
                           (cond
                            ((< attempt-segment-duration target-segment-duration
                                target-duration attempt-duration)
                             (propertize relative 'face 'speedo-gaining))
                            ((< target-segment-duration attempt-segment-duration
                                attempt-duration target-duration)
                             (propertize relative 'face 'speedo-losing))
                            ((= attempt-segment-duration target-segment-duration
                                target-duration attempt-duration)
                             (propertize relative 'face 'speedo-neutral))
                            (t relative)))
                         speedo-text-place-holder)))
                (when current-line (setq s (propertize s 'live-comparison t)))
                (if best-segment (propertize s 'face 'speedo-pb) s)))
             (speedo--time-formatter #'speedo--formatter-compact)
             (split-time (or (pcase speedo--state
                               ('pre (unless (or skipped (null target-duration))
                                       target-duration))
                               ('running (if (< index speedo--segment-index)
                                             (unless attempt-segment-skipped
                                               attempt-duration)
                                           (unless (or (null target-segments)
                                                       target-segment-skipped)
                                             target-duration)))
                               ('post (unless attempt-segment-skipped attempt-duration))
                               (_ (error "Uknown timer state")))
                             speedo-text-place-holder)))
        (unless (stringp split-time)
          (setq split-time (speedo--format-ms split-time)))
        (when current-line
          (setq split-time
                (propertize split-time 'current-segment-timer t 'face current-face)))
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
                      (or (when-let* ((category (plist-get speedo--data :category)))
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
      (when-let* ((reset (plist-member attempt :reset)))
        (setq attempt (plist-put attempt :reset (funcall fn (plist-get attempt :reset)))))
      (setq attempt
            (plist-put attempt :segments
                       (mapcar
                        (lambda (segment)
                          (let ((duration (plist-member segment :duration))
                                (mistakes (plist-member segment :mistakes)))
                            (when duration
                              (setq segment
                                    (plist-put segment :duration
                                               (funcall fn (plist-get segment :duration)))))
                            (when mistakes
                              (setq segment
                                    (plist-put segment :mistakes
                                               (mapcar fn (plist-get segment :mistakes)))))
                            segment))
                        (plist-get attempt :segments)))))))

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
  (let ((next (nth (% (+ n (or (cl-position speedo--comparison-target
                                              speedo-comparison-targets)
                                 -1))
                        (length speedo-comparison-targets))
                   speedo-comparison-targets)))
    (speedo--target-attempt (cdr next) 'cache))
  (speedo--display-ui)
  (speedo--redisplay))

(defun speedo--load-file (file)
  "Load a splits FILE."
  (if-let* ((data (speedo--read-file file)))
      (prog1
          (setq speedo--segment-index -1
                speedo--time nil
                speedo--current-attempt nil
                speedo--state 'pre
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
             (speedo--attempt-in-progress-p)
             (yes-or-no-p "Save current attempt before killing buffer?"))
    (speedo--attempt-end)))

(defvar speedo-mode-map (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "<kp-1>")  'speedo-next)
                          (define-key map (kbd "SPC")     'speedo-next)
                          (define-key map (kbd "<down>")  'speedo-next)
                          (define-key map (kbd "<kp-3>")  'speedo-reset)
                          (define-key map (kbd "r")       'speedo-reset)
                          (define-key map (kbd "<kp-2>")  'speedo-skip)
                          (define-key map (kbd "s")       'speedo-skip)
                          (define-key map (kbd "<kp-4>")  'speedo-comparison-previous)
                          (define-key map (kbd "<left>")  'speedo-comparison-previous)
                          (define-key map (kbd "<kp-6>")  'speedo-comparison-next)
                          (define-key map (kbd "<right>") 'speedo-comparison-next)
                          (define-key map (kbd "<kp-8>")  'speedo-previous)
                          (define-key map (kbd "<up>")    'speedo-previous)
                          (define-key map (kbd "<kp-5>")  'speedo-mistake)
                          (define-key map (kbd "m")       'speedo-mistake)
                          (define-key map (kbd "q")       'speedo-quit-window)
                          (define-key map (kbd "c")       'speedo-compact-mode)
                          (define-key map (kbd "e")       'speedo-edit-last-attempt)
                          map))

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

(provide 'speedo)
;;; speedo.el ends here
