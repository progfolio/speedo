;; -*- lexical-binding: t; -*-
(face-remap-set-base 'speedo-timer :height 1.5)
(with-current-buffer (get-buffer-create speedo-buffer)
  (unless (string= (buffer-name) "*Akogare Mario World*")
    (setq speedo-buffer (rename-buffer "*Akogare Mario World*"))))
