;;; early-init.el -*- lexical-binding: t -*-

(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors nil)

;; Disable UI elements early (faster than init.el)
(menu-bar-mode -1)
(tool-bar-mode -1) 
(scroll-bar-mode -1)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Max out GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable file-name-handler-alist during startup  
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Prevent package.el loading (we'll control it)
(setq package-enable-at-startup nil)

