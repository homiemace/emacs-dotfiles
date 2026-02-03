;;; init.el --- Homie's Emacs Configuration -*- lexical-binding: t -*-

;; (setq debug-on-error t)

(defvar efs/default-font-size 170)
(defvar efs/default-variable-font-size 170)

(add-hook 'emacs-startup-hook
(lambda ()
  (setq gc-cons-threshold 100000000
        gc-cons-percentage 0.1)))

;; Display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

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

(use-package exec-path-from-shell
  :ensure t
  :if (memq system-type '(gnu/linux darwin))  ; only on linux/macos
  :config
  (setq exec-path-from-shell-arguments nil)
  ;; auto-detect the shell
  (when (eq system-type 'darwin)  ; macOS
    (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish"))
  (exec-path-from-shell-initialize))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-nondirectory (buffer-file-name))
                      "Emacs.org")
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)

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

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines
  :ensure t
  :hook (emacs-lisp-mode . page-break-lines-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'check-parens nil t)))

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

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

(when (eq system-type 'darwin)
  (when-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-use-ls-dired t
          dired-listing-switches "-alh --group-directories-first")))

(defun my/authinfo-openrouter-api-key ()
  (require 'auth-source)
  (let ((match (car (auth-source-search
                     :host "openrouter.ai"
                     :user "apikey"
                     :require '(:secret)))))
    (when match
      (let ((secret (plist-get match :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(use-package gptel
:ensure t
:commands (gptel)
:init
(setq gptel-model 'mixtral-8x7b-instruct
	gptel-use-tools t)
:config
(setq gptel-backend
      (gptel-make-openai "Ollama"
        :host "localhost:11434"
        :stream t
        :models '(mistral:latest)
        :request-params '(:transforms ["middle-out"]))))

(use-package gptel-agent
  :vc ( :url "https://github.com/karthink/gptel-agent"
        :rev :newest)
  :config (gptel-agent-update))

(use-package minuet
  :ensure t
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1)
  (setq minuet-context-window 512)

  (plist-put minuet-openai-fim-compatible-options
             :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options
             :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options
             :model "qwen2.5-coder:3b")

  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :max_tokens 56))

(use-package company
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3))

(use-package format-all
:commands (format-all-mode format-all-region-or-buffer)
:hook ((prog-mode . format-all-mode)
       (format-all-mode . format-all-ensure-formatter))
:bind (("C-c =" . format-all-region-or-buffer))
:custom
(format-all-show-errors 'errors))

(use-package magit
  :ensure t)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package svelte-mode
  :mode "\\.svelte\\'"
  :hook (svelte-mode . lsp-deferred))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package lsp-pyright
:ensure t
:custom (lsp-pyright-langserver-command "pyright")
:hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(with-eval-after-load 'lsp-mode
  ;; Use gopls and enable some common settings
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-analyses '((shadow . t)
                             (unusedparams . t))))

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
  ;; Uncomment the following line to have sorting remembered across sessions
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; Project-wide search with ripgrep
(global-set-key (kbd "C-c s") 'counsel-rg)

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

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Check for DM Sans cross-platform
  (let ((dm-font (car (seq-filter
                       (lambda (f)
                         (string-match "DM Sans" f))
                       (font-family-list)))))
    
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
      (set-face-attribute (car face) nil
                          :font dm-font
                          :weight 'regular
                          :height (cdr face)))))

(defun my/quick-journal-capture ()
  "Quick capture to journal."
  (interactive)
  (org-capture nil "jj"))

(use-package org
  :demand t
  :hook (org-mode . efs/org-mode-setup)
  :bind (("C-c j" . my/quick-journal-capture))
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files
        '("~/emacs-dotfiles/org/Tasks.org"
          "~/emacs-dotfiles/org/Birthdays.org"
          "~/emacs-dotfiles/org/Habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-refile-targets
  '(("Archive.org" :maxlevel . 1)
    ("Tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("W" "Work Tasks" tags-todo "+work-email")

  ;; Low-effort next actions
  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
   ((org-agenda-overriding-header "Low Effort Tasks")
    (org-agenda-max-todos 20)
    (org-agenda-files org-agenda-files)))

  ("w" "Workflow Status"
   ((todo "WAIT"
          ((org-agenda-overriding-header "Waiting on External")
           (org-agenda-files org-agenda-files)))
    (todo "REVIEW"
          ((org-agenda-overriding-header "In Review")
           (org-agenda-files org-agenda-files)))
    (todo "PLAN"
          ((org-agenda-overriding-header "In Planning")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "BACKLOG"
          ((org-agenda-overriding-header "Project Backlog")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "READY"
          ((org-agenda-overriding-header "Ready for Work")
           (org-agenda-files org-agenda-files)))
    (todo "ACTIVE"
          ((org-agenda-overriding-header "Active Projects")
           (org-agenda-files org-agenda-files)))
    (todo "COMPLETED"
          ((org-agenda-overriding-header "Completed Projects")
           (org-agenda-files org-agenda-files)))
    (todo "CANC"
          ((org-agenda-overriding-header "Cancelled Projects")
           (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp "~/emacs-dotfiles/org/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("jj" "Journal" entry
         (file+olp+datetree "~/emacs-dotfiles/org/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
    ("jm" "Meeting" entry
         (file+olp+datetree "~/emacs-dotfiles/org/Journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

    ("w" "Workflows")
    ("we" "Checking Email" entry (file+olp+datetree "~/emacs-dotfiles/org/Journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "~/emacs-dotfiles/org/Metrics.org" "Weight")
     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(with-eval-after-load 'org
  (efs/org-font-setup)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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

(use-package org-roam
:ensure t
:custom
(org-roam-directory (file-truename "/path/to/org-files/"))
:bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n g" . org-roam-graph)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-capture)
       ;; Dailies
       ("C-c n j" . org-roam-dailies-capture-today))
:config
;; If you're using a vertical completion framework, you might want a more informative completion interface
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)
;; If using org-roam-protocol
(require 'org-roam-protocol))

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

;; JavaScript Wiki directory
(setq my/js-wiki-dir (expand-file-name "wiki/javascript" user-emacs-directory))

(defun my/open-js-wiki ()
  "Open JavaScript wiki index"
  (interactive)
  (find-file (expand-file-name "index.org" my/js-wiki-dir)))

(defun my/js-wiki-search ()
  "Search JavaScript wiki files using grep"
  (interactive)
  (let ((default-directory my/js-wiki-dir))
    (counsel-rg "")))

(defun my/js-wiki-new-entry ()
  "Create new wiki entry"
  (interactive)
  (let ((title (read-string "Entry title: ")))
    (find-file (expand-file-name 
                (concat (downcase (replace-regexp-in-string " " "-" title)) ".org")
                my/js-wiki-dir))
    (insert (format "#+TITLE: %s\n#+FILETAGS: :javascript:\n\n* Big Questions\n\n** What is this?\n\n** Why is this important?\n\n** When will I need this?\n\n** How does it work?\n\n* Code Examples\n\n#+begin_src javascript\n\n#+end_src\n" title))
    (goto-char (point-min))
    (forward-line 4)))

;; Keybindings
(global-set-key (kbd "C-c w j") 'my/open-js-wiki)
(global-set-key (kbd "C-c w s") 'my/js-wiki-search)
(global-set-key (kbd "C-c w n") 'my/js-wiki-new-entry)

(defun open-init-file ()
  "Open Emacs.org for editing"
  (interactive)
  (find-file "~/.emacs.d/Emacs.org"))

(defun reload-init-file ()
  "Reload init.el"
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(global-set-key (kbd "C-c i") 'open-init-file)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-hook 'after-init-hook
          (lambda ()
            (run-with-timer 0.5 nil 
                            (lambda ()
                              (set-face-attribute 'default nil 
                                                  :font "JetBrains Mono" 
                                                  :height efs/default-font-size)))))

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
