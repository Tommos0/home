;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Tom Klaver
;; Homepage: https://github.com/tommos0

;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Personal Emacs configuration
;;
;;; Code:
;;;; Initialization, load path etc.
(provide 'init)
(defmacro noop (&rest _)
  "Do nothing.")

(setq gc-cons-threshold (* 1024 1024 256)) ;; 256MB
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file)
;;;; Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq native-comp-async-report-warnings-errors nil)
;;;; Custom built-in config
(setq
  backup-directory-alist '(("." . "~/.emacs.saves"))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
(push (expand-file-name "~/.local/bin") exec-path)
(save-place-mode 1)
(global-prettify-symbols-mode nil)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers 1)
(column-number-mode t)
(menu-bar-mode -1)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'kill-current-buffer)
(bind-key "C-x / p" #'yank-from-kill-ring)
(bind-key "M-p" #'yank-from-kill-ring)

(defun xdg-open () (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (concat "xdg-open " file)))))

(defun yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

(bind-key "C-x / y" #'yank-buffer-path)
(setenv "EDITOR" "emacsclient")
(tool-bar-mode -1)
(setq frame-resize-pixelwise t)
(setq-default line-spacing 4)
(setq vc-follow-symlinks nil)
(setq create-lockfiles nil)
(setq use-short-answers t)
(bind-key "C-\\" #'universal-argument)
(put 'narrow-to-region 'disabled nil)
(setq visible-bell 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)
(setq async-shell-command-buffer 'new-buffer)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(use-package project)
;;;; Terminal optimizations
(use-package evil-terminal-cursor-changer
  :config
  (evil-terminal-cursor-changer-activate))

(use-package clipetty
  :config
  (global-clipetty-mode))

;;;; Theme
(use-package gruvbox-theme
  :custom-face
  (highlight ((t (:background "#4e463f"))))
  (error ((t (:foreground nil :background "#660000" :underline t))))
  (warning ((t (:foreground nil :background "#793101" :underline t))))
  (default ((t (:background "#282828"))))
  (match ((t (:background "darkorange"))))
  (auto-dim-other-buffers ((t (:background "#1e1e1e"))))
  (auto-dim-other-buffers-hide ((t (:background "#1e1e1e"))))
  :init
  (load-theme 'gruvbox-dark-medium t))

;;(set-face-attribute 'default nil :family "Liberation Mono" :height 96)
;;(set-face-attribute 'default nil :family "Fira Code" :height 105)
;;(set-face-attribute 'default nil :family "Hack" :height 105)
;;(set-face-attribute 'default nil :family "DejaVu Mono" :height 105)

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode))

(noop
 (use-package all-the-icons
   :if (display-graphic-p))
 (use-package all-the-icons-dired
   :hook (dired-mode . all-the-icons-dired-mode))
 (use-package all-the-icons-completion
   :hook
   (marginalia-mode . all-the-icons-completion-marginalia-setup)))

;;;; Evil
(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-minibuffer t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  :bind
  (:map evil-insert-state-map ("C-k" . nil))
  (:map evil-insert-state-map ("C-j" . nil))
  (:map evil-normal-state-map ("M-." . nil))
  (:map global-map ("C-@" . nil))
  (:map global-map ("C-SPC" . nil))
  :config
  (evil-define-key 'insert 'global (kbd "C-SPC") nil)
  (evil-define-key 'insert 'global (kbd "C-@") nil)
  (evil-set-leader 'insert (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key '(normal insert) 'global (kbd "<leader>u") 'universal-argument)
  ;;(define-key global-map (kbd "<leader>P") 'safe-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'safe-buffer)
  (defun evil-jump-backward-same-file (&optional count) (interactive)
         (let ((evil-jumps-cross-buffers nil))
           (evil-jump-backward count)))
  (defun evil-jump-forward-same-file (&optional count) (interactive)
         (let ((evil-jumps-cross-buffers nil))
           (evil-jump-forward count)))
  (evil-define-key 'normal 'global (kbd "C-S-o") 'evil-jump-backward-same-file)
  (evil-define-key 'normal 'global (kbd "C-S-i") 'evil-jump-forward-same-file)
  (evil-set-initial-state 'rcirc-mode 'normal)
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-cleverparens
  :hook
  (emacs-lisp-mode . evil-cleverparens-mode)
  :config
  ;; fixes mess created by alt-tabbing to KITTY window (insert M-[ key somehow).
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-[") nil))

(use-package evil-surround
  :custom
  (global-evil-surround-mode t))

;;;; Version control
(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-list-refs-sortby "-creatordate")
  :config
  (require 'magit-extras))

;; (use-package forge
;;   :after magit
;;   :config
;;   ;; Token is stored in 'auth-sources
;;   (add-to-list 'forge-alist '("ahold" "api.github.com" "github.com" forge-github-repository)))

(use-package git-gutter
  :custom
  (global-git-gutter-mode t))

(use-package git-timemachine
  :straight (git-timemachine
             :type git
             :host github
             :repo "emacsmirror/git-timemachine"
             :branch "master"))

(use-package browse-at-remote
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "gitnmp.arp.ncsc.nl" :type "gitlab" :actual-host "gitnmp.arp.ncsc.nl")
               '(:host "ahold" :type "github" :actual-host "github.com")))

;;;; Org mode
;; needs to be early to avoid loading built-in org mode by some other packages
(use-package org
  :bind
  (:map org-mode-map ("TAB" . org-cycle))
  :config
  (require 'org-tempo)
  (evil-define-key 'motion org-mode-map "TAB" nil)
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil))
  (setq org-startup-indented t))

(define-derived-mode typescript-mode typescript-ts-mode "typescript")
(define-derived-mode json-mode json-ts-mode "json")

(require 'ob-shell)
(use-package org-download)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((sql . t)))

(use-package ob-sql-mode)
;;(require 'ob-sql-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;;;; Minibuffer
(use-package vertico
  :after (evil)
  :init
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  :bind
  (:map vertico-map
    ("C-j" . 'vertico-next)
    ("C-k" . 'vertico-previous)
    ("C-n" . 'vertico-scroll-up)
    ("C-p" . 'vertico-scroll-down)
    ("C-u" . 'vertico-scroll-down)
    ("C-d" . 'vertico-scroll-up)
    ("C-S-k" . 'vertico-scroll-down)
    ("C-S-j" . 'vertico-scroll-up)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;;;; LLM
(use-package gptel
  :straight (gptel
             :type git
             :host github
             :repo "karthink/gptel"
             :branch "master")
  :custom
  (gptel-default-mode 'org-mode))

;;;; Packages
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 25000)
  (recentf-max-menu-items 25))

(use-package dumb-jump)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package eglot
  :straight nil
  :ensure nil
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  :config
  ;; https://github.com/joaotavora/eglot/issues/268#issuecomment-544890756
  (setq eglot-stay-out-of '(flymake))
  (add-hook 'eglot--managed-mode-hook (lambda () (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)))
  (add-to-list 'eglot-server-programs '(graphql-mode . ("graphql-lsp" "-m" "stream" "server")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;;              `((typescript-ts-mode typescript-mode) .
  ;;                ,(eglot-alternatives '(("typescript-language-server" "--stdio")
  ;;                                       ("deno" "lsp" :initializationOptions (:enable t :lint t))))))

  ;;(setq eglot-events-buffer-size 400000000)
  :bind
  (:map eglot-mode-map ("C-c a" . eglot-code-actions))
  :hook
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  ;; (graphql-mode . eglot-ensure)
  )

(use-package eglot-booster
  ;https://github.com/jdtsmith/eglot-booster
  :straight (eglot-booster
             :type git
             :host github
             :repo "jdtsmith/eglot-booster"
             :branch "main")
  :after eglot
  :config (eglot-booster-mode))

(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
  ("C-c ! l" . flymake-show-buffer-diagnostics)
  ("C-c ! p" . flymake-goto-prev-error)
  ("C-c ! n" . flymake-goto-next-error))
  :config
  (add-hook 'prog-mode-hook 'flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package auto-highlight-symbol
  :hook
  (emacs-lisp-mode . auto-highlight-symbol-mode))

(use-package consult
  :after (evil)
  :init
  (defun eshell/new ()
    "Create a new eshell buffer."
    (interactive)
    (eshell 'N))

  (defun consult-ripgrep/here ()
    "ripgrep in default-directory"
    (interactive)
    (consult-ripgrep default-directory (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))))

  (defun consult-ripgrep/project ()
    "ripgrep in project root"
    (interactive)
    (when (project-current)
      (consult-ripgrep (project-root (project-current)) (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))))

  (defun consult-find/here ()
    "Find file by name in default-directory"
    (interactive)
    (consult-find default-directory))

   (defun consult-completion-at-point ()
    "completion-at-point with consult"
    (interactive)
    (let ((completion-in-region-function #'consult-completion-in-region))
      (completion-at-point)))

  :config
  (evil-define-key 'insert eat-line-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key 'insert shell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key 'insert sql-interactive-mode (kbd "C-r") 'consult-history)
  (evil-define-key 'insert minibuffer-mode-map (kbd "C-r") 'consult-history)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  :bind
  ("C-x / /" . consult-ripgrep/here)
  ("C-x p /" . consult-ripgrep/project)
  ("C-x / f" . consult-find/here)
  ("C-x / s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x j" . consult-bookmark)
  ("C-x C-r" . consult-recent-file))

(use-package which-key
  :init
  (which-key-mode))

(use-package company
  :init
  (global-company-mode)
  :hook
  (eshell-mode . (lambda () (setq-local company-backends '(company-capf))))
  :custom
  (company-search-filtering t)
  :config
  (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'company-complete)
  :bind
  (:map company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . company-complete)
        ("C-s" . consult-completion-at-point)
        ("TAB" . company-complete))
  (:map evil-insert-state-map
        ("C-c SPC" . company-complete)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h o" . helpful-symbol))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package embark
  :bind
  (:map evil-normal-state-map
        ("M-." . embark-act))
  (:map evil-insert-state-map
        ("M-." . embark-act)))

(use-package embark-consult)
(use-package xref
  :straight nil
  :bind
  ;; This is reserved for embark-act
  (:map global-map ( "M-." . nil)))

(noop
 (use-package vterm
   :init
   (defun vterm/new ()
     "Create a new vterm buffer."
     (interactive)
     (vterm 'N))
   (defun vterm/project ()
     "Create a new vterm buffer in project root."
     (interactive)
     (let ((default-directory (project-root (project-current))))
       (vterm 'N)))
   :custom
   (vterm-buffer-name-string "vterm %s")
   :config
   (push '(vterm/project "VTerm") project-switch-commands)
   :bind
   ("C-x / t" . vterm/new)
   (:map
    project-prefix-map ("t" . vterm/project))))

(use-package eshell
  :straight nil
  :config
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
  (let ((newest-cmd-ring (make-ring 1)))
  (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
  (let ((eshell-history-ring newest-cmd-ring))
      (eshell-write-history eshell-history-file-name t)))))

  (setq eshell-ls-initial-args '("-larth"))
  (defalias 'l 'eshell/ls)

  :custom
  (eshell-save-history-on-exit nil)
  (eshell-history-size 25000)

  :hook
  ;; Updates history after each command (instead of at exit).
  (eshell-pre-command . eshell-append-history)
  :bind
  ("C-x / e" . eshell/new))


;; (use-package bash-completion
;;   :config
;;   (bash-completion-setup))

(use-package wgrep)
(use-package dired
  :straight nil
  :ensure nil
  :config
  (defun dired-copy-file-path-as-kill () (interactive) (dired-copy-filename-as-kill 0))
  (bind-key "C-c C-y" #'dired-copy-file-path-as-kill 'dired-mode-map)
  (defun dired-find-marked-files ()
         "Open each of the marked files"
         (interactive)
         (mapc 'find-file (dired-get-marked-files)))
  (bind-key "F" #'dired-find-marked-files 'dired-mode-map)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-dwim-target t))
(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package sudo-edit)
(use-package avy
  :bind
  ("C-a" . avy-goto-char-timer))

(use-package htmlize)
(use-package string-inflection)
(use-package dtrt-indent
  :custom
  (dtrt-indent-global-mode t))
(use-package command-log-mode)
(use-package docker
  :bind
  ("C-x d" . docker))
(use-package kubernetes)
(use-package elmacro)
(use-package rcirc
  :custom
  (rcirc-default-nick "Tommos0")
  (rcirc-server-alist '(("irc.libera.chat"
                        :channels ("#emacs")
                        :port 6697 :encryption tls))))
(use-package direnv
 :config
 (direnv-mode))
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

;;;; Languages
;;;;; misc
(use-package markdown-mode)
(use-package yaml-mode)
(use-package graphql-mode)
(use-package elf-mode)
;; ;(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
;;;;; Elisp
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;;;;; NodeJS
(push (expand-file-name "~/.yarn/bin") exec-path)
(use-package nvm-switch
  :straight (nvm-switch
             :type git
             :host github
             :repo "tommos0/nvm-switch.el"
             :branch "main"))


(use-package jest-ts
  :straight (jest-ts
             :type git
             :host github
             :repo "tommos0/jest-ts.el"
             :branch "master"))

(use-package flymake-eslint
  :config
  (defun flymake-eslint-enable--delayed ()
    "Seems necessary to make flymake-eslint actually enable."
    (run-at-time "0.01 sec" nil (lambda () (flymake-eslint-enable) (flymake-start))))
  
  (add-hook 'typescript-ts-mode-hook 'flymake-eslint-enable--delayed)
  (add-hook 'tsx-ts-mode-hook 'flymake-eslint-enable--delayed))

(use-package prettier-js
  :hook
  (typescript-ts-mode . prettier-js-mode)
  (tsx-ts-mode . prettier-js-mode)
  (graphql-mode . prettier-js-mode)
  (json-mode . prettier-js-mode)
  (js-json-mode . prettier-js-mode)
  (json-ts-mode . prettier-js-mode)
  (css-mode . prettier-js-mode)
  (scss-mode . prettier-js-mode)
  (yaml-mode . prettier-js-mode)
  (yaml-ts-mode . prettier-js-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;;;;; Python
(defun pdbtrack-shell-mode-hook ()
  (add-hook
   'comint-output-filter-functions
   'python-pdbtrack-comint-output-filter-function t))
(add-hook 'shell-mode-hook 'pdbtrack-shell-mode-hook)

(use-package pymacs
  :config
  (defun pymacs--first-time-setup ()
    "First time Pymacs setup.
     Setup venv + copy python file to python load path."
    (interactive)
    (call-process-shell-command "python -m venv ~/pymacs")
    (let ((python-pkg-dir (expand-file-name (car (file-expand-wildcards "~/pymacs/lib/python*/site-packages")))))
      (copy-file (expand-file-name "straight/repos/Pymacs/Pymacs.py" user-emacs-directory)
                 (expand-file-name "Pymacs.py" python-pkg-dir))))
  :custom
  (pymacs-python-command (expand-file-name "~/pymacs/bin/python")))

;;;;;; Ruff
(use-package ruff-flymake
  :straight nil
  :ensure nil
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-flymake-mode))

;;;;; SQL
(setq sql-postgres-login-params nil)

;; define your connections
(setq sql-connection-alist
      '((localhost (sql-product 'postgres)
                   (sql-database (concat "postgresql://"
                                         "postgres"
                                         ":password"
                                         "@localhost"
                                         ":5432"
                                         "/postgres"
                                         )))
        ;; (secondary-db (sql-product 'postgres)
        ;;               (sql-database (concat "postgresql://"
        ;;                                     "username:"
        ;;                                     (read-passwd "Enter password: ")
        ;;                                     "@host"
        ;;                                     ":port"
        ;;                                     "/database")))
        ))

;;;;; Common Lisp
(use-package sly)
;;;; Misc
(defun restart-async-process () (interactive)
       (let* ((current-process (get-buffer-process (current-buffer)))
              (current-command (car (last (process-command current-process)))))
         (kill-process (get-buffer-process (current-buffer)))
         (sleep-for .5)
         (async-shell-command current-command (current-buffer))))


;;;; Scratch area

(defun open-in-vscode ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (number-to-string (line-number-at-pos)))
         (col (number-to-string (+ 1 (current-column))))
         (filestr (concat file ":" line ":" col)))
    (start-process "code" nil "/usr/bin/code" "--goto" filestr)))


;; (defvar org-auto-redisplay-after-eval t)
;; (defun org-redisplay-when-auto-redisplay ()
;;   "Redisplay when `org-auto-redisplay-after-eval' is non-nil."
;;   (when org-auto-redisplay-after-eval
;;     (org-redisplay-inline-images)))

;; (advice-add 'org-babel-execute-src-block
;;       :after 'org-redisplay-when-auto-redisplay)

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice))
               sym))

(use-package
  eat
  :straight
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el")))
 :config
 (push '(eat-mode eat--line-input-ring eat--line-input-ring-index comint-bol) consult-mode-histories)
 (evil-define-key 'insert eat-line-mode-map (kbd "C-p") 'eat-line-previous-input)
 (evil-define-key 'insert eat-line-mode-map (kbd "C-n") 'eat-line-next-input)
 (setq eat-enable-auto-line-mode t)
 :hook
 (eat-eshell-mode . (lambda () (setq-local eshell-visual-commands nil)))
 (eshell-mode . eat-eshell-mode)
 :bind
 (:map eat-line-mode-map
       ("C-k" . eat-previous-shell-prompt)
       ("C-j" . eat-next-shell-prompt)))

(defun cleanup-windows-paste ()
  "When pasting through a terminal from windows, you'd expect CRLF endings.
But I'm seeing CRHTCR (\\n\\t\\n).
This command changes that sequence to just one line break."
  (interactive)
  (query-replace-regexp "\n\t\n" "\n" nil (point-min) (point-max)))

(defun unlock-gpg-key ()
  "Unlock GPG key."
  (interactive)
  (async-shell-command "gpg --sign --dry-run /dev/null"))

(noop
 (defun previous-line-same-indent ()
   "Go to the first line above the cursor where the indentation is less than the current line."
   (interactive "^")
   (evil-set-jump)
   (let ((original-indent (current-indentation)))
     (while (and (>= (current-indentation) original-indent)
                 (not (bobp))) ;; Not beginning of the buffer
       (previous-line))
     (evil-first-non-blank))))

(use-package bm-bookmarks
  :straight nil
  :ensure nil
  :custom
  (bm-default-action #'kill-new)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'bm-search-bookmarks)
  (evil-define-key 'normal 'global (kbd "<leader>bs") 'bm-save-bookmark))

(condition-case nil
    (load "private")
  (file-error (message "No private.el found")))

;;; init.el ends here

