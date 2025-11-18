;;; init.el --- Bootstrap Org Configuration -*- lexical-binding: t -*-

;; Separate Custom changes from main config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'org)
(org-babel-load-file (expand-file-name "Emacs.org" user-emacs-directory))
