;;; init.el --- Bootstrap Org Configuration -*- lexical-binding: t -*-

;; Separate Custom changes from main config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((org-file (expand-file-name "Emacs.org"
                                  user-emacs-directory))
      (el-file  (expand-file-name "Emacs.el"
                                  user-emacs-directory)))
  (unless (file-exists-p el-file)
    (require 'org)
    (org-babel-tangle-file org-file))
  (load el-file nil 'nomessage))
