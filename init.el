;;; init.el --- HOMIE'S EMACS CONFIG -*- lexical-binding: t -*-

;; ============================================================================
;; PERFORMANCE & STARTUP
;; ============================================================================

(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)

;; (setq debug-on-error t)

(defvar efs/default-font-size 170)
(defvar efs/default-variable-font-size 170)

;; Garbage collection threshold
(setq gc-cons-threshold (* 50 1000 1000))

;; Display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; ============================================================================
;; NO-LITTERING - KEEP .emacs.d CLEAN
;; ============================================================================

;; Clean .emacs.d
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; ============================================================================
;; UI & APPEARANCE
;; ============================================================================

;; Theme
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

;; Font
(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)

;; UI cleanup
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Icons
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon t)
  (setq mode-line-default-help-echo nil)
  
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'mode-line-highlight nil :box nil))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines
  :ensure t
  :hook (emacs-lisp-mode . page-break-lines-mode))

;; ============================================================================
;; DASHBOARD
;; ============================================================================

(advice-add #'dashboard-replace-displayable :override #'identity)

(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-startup-banner "~/.emacs.d/banners/7.txt")
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-items '((projects . 10)
                          (recents  . 10)))
  
  ;; Icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-heading-icons
        '((recents   . "nf-oct-history")
          (projects  . "nf-oct-rocket")
          (bookmarks . "nf-oct-bookmark")
          (agenda    . "nf-oct-calendar")))

  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")
                                   (registers . "e")))
  
  ;; Layout
  (setq dashboard-center-content t)
  (setq dashboard-navigation-cycle t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq inhibit-startup-screen t)
  
  (dashboard-setup-startup-hook)
  
  ;; Fix dashboard centering on window resize
  (defun my-dashboard-fullscreen-fix ()
    "Fix dashboard centering in fullscreen"
    (when (string= (buffer-name) "*dashboard*")
      (dashboard-refresh-buffer)))
  
  (add-hook 'window-configuration-change-hook 'my-dashboard-fullscreen-fix)
  (add-hook 'dashboard-mode-hook 'page-break-lines-mode))

;; ============================================================================
;; SYSTEM-SPECIFIC CONFIGURATION
;; ============================================================================

;; Use GNU ls for dired on macOS
(when (eq system-type 'darwin)
  (when-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-use-ls-dired t
          dired-listing-switches "-alh --group-directories-first")))

;; ============================================================================
;; DEVELOPMENT TOOLS
;; ============================================================================

(use-package company
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-ignore-buffers '("\\` " "\\*Ibuffer\\*"))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :config
  (setq helpful-max-buffers 5))

;; ============================================================================
;; ORG MODE SETUP
;; ============================================================================

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DM Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

(setq org-agenda-files
      '("~/emacs-dotfiles/org/Tasks.org"
       "~/emacs-dotfiles/org/Birthdays.org"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; ============================================================================
;; FORGE - PERSONAL WORKFLOW
;; ============================================================================

(setq forge-dashboard-file "~/.emacs.d/forge/forge-dashboard.org")
(setq forge-journal-file "~/.emacs.d/forge/forge-journal.org")

(defun forge/open-dashboard ()
  "Open Forge dashboard file"
  (interactive)
  (find-file forge-dashboard-file))

(defun forge/open-journal ()
  "Open Forge journal file"
  (interactive)
  (find-file forge-journal-file))

(defun forge/caveman-q ()
  "Insert Caveman Q template in journal"
  (interactive)
  (find-file forge-journal-file)
  (goto-char (point-max))
  (insert (format "\n* Caveman Q (%s)\n- What am I stuck on?\n- What do I know?\n- What's one small action I can take?\n"
                  (format-time-string "%H:%M")))
  (recenter))

;; Forge keybindings
(global-set-key (kbd "C-c f d") 'forge/open-dashboard)
(global-set-key (kbd "C-c f j") 'forge/open-journal)
(global-set-key (kbd "C-c f c") 'forge/caveman-q)

;; ============================================================================
;; UTILITY FUNCTIONS & KEYBINDINGS
;; ============================================================================

(defun open-init-file ()
  "Open init.el for editing"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-init-file ()
  "Reload init.el"
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(global-set-key (kbd "C-c i") 'open-init-file)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ============================================================================
;; FONT FIX - PREVENTS WINDOW RESIZE CRASHES
;; ============================================================================

(add-hook 'after-init-hook
          (lambda ()
            (run-with-timer 0.5 nil 
                            (lambda ()
                              (set-face-attribute 'default nil 
                                                  :font "JetBrains Mono" 
                                                  :height efs/default-font-size)))))

;; ============================================================================
;; CUSTOM VARIABLES (auto-generated)
;; ============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
