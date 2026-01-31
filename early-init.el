;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq initial-scratch-message nil)

(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist file-name-handler-alist-original)))
