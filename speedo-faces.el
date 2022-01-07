;;; speedo-faces.el --- faces for speedo.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords: faces

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
(defgroup speedo-faces nil
  "Faces used in speedo.el."
  :group 'speedo
  :group 'faces)

(defface speedo-ahead
  '((t (:inherit speedo-emphasis :foreground "green")))
  "Face for time ahead of comparison."
  :group 'speedo-faces)

(defface speedo-behind
  '((t (:inherit speedo-emphasis :foreground "red")))
  "Face for time behind comparison."
  :group 'speedo-faces)

(defface speedo-neutral
  '((t (:inherit speedo-emphasis :foreground "#5555FF")))
  "Face for time equal to comparison."
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
  '((t (:family "Hermit")))
  "Default face in `speedo-buffer'.")

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
  '((t (:inherit hl-line)))
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
  '((t (:inherit speedo-neutral :weight ultra-bold :height 1.5)))
  "Face for the global run timer."
  :group 'speedo-faces)

(provide 'speedo-faces)
;;; speedo-faces.el ends here
