;;; speedo-vars.el --- Settings and variables        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicholas Vollmer

;; Author: Nicholas Vollmer
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
(defgroup speedo nil
  "Speedo: lightweight split timer."
  :group 'applications
  :prefix "speedo-")

;;; Customizations
(defcustom speedo-buffer "*speedo*"
  "Buffer to display splits."
  :type 'string
  :group 'speedo)

(defcustom speedo-compact-last-split-separator ?-
  "Separates splits and last split when symbol `speedo-compact-mode' is non-nil.
It may be any of the following values:

  - a character
    The character is repeated across the length of the split table line.
  - a string
    The literal string is inserted on a line before the last split.
  - a function
    The function is called with no arguments and must return a string."
  :type (or 'character 'string 'function)
  :group 'speedo)

(defcustom speedo-comparison-targets '(("Personal Best"  . speedo-target-personal-best)
                                       ("Best Segments"  . speedo-target-best-segments)
                                       ("World Record"   . speedo-target-world-record)
                                       ("Last Attempt"   . speedo-target-last-attempt)
                                       ("Last Run"       . speedo-target-last-run)
                                       ("Personal Worst" . speedo-target-personal-worst)
                                       ("None"           . ignore))
  ;;best-split-times, average-segments, median-segments, balanced-pb
  "Alist of comparison targets.
Each element's car is a cons cell of form: \\=(FN . DESCRIPTION)
FN is a function called with no arguments which returns an attempt.
DESCRIPTION is either a string or a function which returns a string to display
in the UI footer.
`speedo-next-comparison' and `speedo-previous-comparsion' cycle these in order."
  :type 'list
  :group 'speedo)

(defcustom speedo-confirm-evaluate t
  "If non-nil, confirm evaluation of `:config` `speedo--data'."
  :type 'boolean
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

(defcustom speedo-footer-format
  '((speedo-global-timer   .   "%it \n")
    (speedo-target         .   "Comparing Against: %it\n")
    (speedo-projected-best .   "   Projected Best: %it\n")
    (speedo-previous-split .   "         Previous: %it\n")
    (speedo-pb-chance      .   "               PB: %it\n")
    (speedo-mistakes       .   "         Mistkaes: %it\n"))
  "An alist representing the structure of the UI footer.
Each member is of the form (FUNCTION . FORMAT).
Function is a unary function which accepts a timer env object.
It should return a string.
If FORMAT is a string the return value can be interpolated in the string
via the shorthand `%it`.
Alternatively, FORMAT may be a function which accepts the output of
FUNCTION and returns a string."
  :type 'alist
  :group 'speedo)

(declare-function speedo-footer-colorized-mistakes "speedo")
(defcustom speedo-footer-mistakes-format #'speedo-footer-colorized-mistakes
  "Format string for the mistake counter UI.
It may contain one %-escaped reference to the mistake count.
It may aslo be a function which takes the count as it's sole argument and
returns a string."
  :type (or 'string 'function)
  :group 'speedo)

(defcustom speedo-footer-want-live-segment t
  "When non-nil, display time current segment time loss in the footer."
  :type 'boolean
  :group 'speedo)

(defcustom speedo-footer-live-segment-format "     Live Segment: %s\n"
  "Format string for live segment split time UI.
It may contain one %-escaped reference to the relative time comparing the
current segment's time to the target's time for that segement."
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

(defcustom speedo-pre-ui-display-hook nil
  "Hook run just before UI display is udpated.
`speedo-buffer' is current."
  :type 'hook
  :group 'speedo)

(defcustom speedo-post-ui-display-hook nil
  "Hook run after UI display is udpated.
`speedo-buffer' is current."
  :type 'hook
  :group 'speedo)

(defcustom speedo-text-place-holder "-"
  "Placeholder text used when no data is available for a field."
  :type 'string
  :group 'speedo)

(defcustom speedo-compact-segment-limit 10
  "Limit of segments to display in command `speedo-compact-mode'.
Note this includes the last segment."
  :type 'interger)

;;; Variables
(defvar speedo--attempt-in-progress nil "Whether or not an attempt is in progress.")
(defvar speedo--best-segments nil "List of lowest durations for each segment.")
(defvar speedo--comparison-target (car speedo-comparison-targets)
  "The current `speedo-comparison-targets' cell.")
(defvar speedo--current-attempt nil "The current attempt.")
(defvar speedo--data nil "Split database.")
(defvar speedo--data-file nil "The filepath of the loaded splits database.")
(defvar speedo--review nil "Non-nil after run complete, before clear/init.")
(defvar speedo--segment-index -1 "Index of the current segment.")
(defvar speedo--segment-start nil "Timestamp marking new segment start.")
(defvar speedo--target-attempt  nil "The cached target attempt.")
(defvar speedo--target-attempts nil "Cache for target attempts.")
(declare-function speedo--formatter-compact "speedo")
(defvar speedo--time-formatter #'speedo--formatter-compact
  "Function to format time from timer.
It must accept four arguments: hours, minutes, seconds, milliseconds.")
(defvar speedo--timer nil "The global timer.")
(defvar speedo--timer-object nil "Internal timer object. Used for cancelling timer.")
(defvar speedo--ui-timer-object nil "Display timer object.")
(defvar speedo-mode-map (make-sparse-keymap) "Keymap for speedo mode.")
(defconst speedo--file-extension ".spd")

(provide 'speedo-vars)
;;; speedo-vars.el ends here
