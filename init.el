;;; init.el --- Bootstrap Org Configuration -*- lexical-binding: t -*-

;; Separate Custom changes from main config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(load (expand-file-name "Emacs" user-emacs-directory) nil 'nomessage)
