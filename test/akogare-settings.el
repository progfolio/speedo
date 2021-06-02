;; -*- lexical-binding: t; -*-
(setq speedo-buffer "*Akogare Mario World*")
(add-hook 'speedo-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*Akogare Mario World*")
              (face-remap-add-relative 'speedo-timer :height 3.0))))
